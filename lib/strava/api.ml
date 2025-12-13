open Core
open Models.Laps
open Models.Streams
open Strava_api
open Models.Strava_models
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Or_error.Let_syntax

(* alias to Time_ns *)
module Time_ns = Time_ns_unix

(* TODO: what is table index ? do i need to create one ? *)
(* TODO: activity 1419970951 is marked as Threadmill run but strava only shows
   it as RUN is the API giving correct type *)
(* TODO: there is a gap in activities from 2016-09-19T18:22:17Z =>
   2016-11-23T14:40:56Z for some reason *)

let pull_activities token num_activities =
  let activities = list_activities ~token ~page:1 ~per_page:num_activities () in
  print_endline activities

let pull_streams_aux token activity_id =
  let resp = get_streams token activity_id in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  (* let filename = sprintf "raw_streams_%d.json" activity_id in *)
  (* Yojson.Safe.to_file filename json; *)
  Or_error.try_with (fun () -> Streams.t_of_yojson_smoothed json)

(* TODO: try with should raise a specific error otherwise i can't figure out what went wrong *)
let pull_streams token activity_id =
  let%bind streams = pull_streams_aux token activity_id in
  let filename = sprintf "streams_%d.json" activity_id in
  Yojson.Safe.to_file filename (Streams.yojson_of_t streams);
  printf "Saved streams to file %s\n" filename;
  Ok ()

let pull_laps_aux token activity_id =
  let resp = get_laps token activity_id in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  (* let filename = sprintf "raw_laps_%d.json" activity_id in *)
  (* Yojson.Safe.to_file filename json; *)
  Or_error.try_with (fun () -> StravaLaps.t_of_yojson json)

let pull_laps token activity_id =
  let%bind laps = pull_laps_aux token activity_id in
  let filename = sprintf "laps_%d.json" activity_id in
  Yojson.Safe.to_file filename (StravaLaps.yojson_of_t laps);
  printf "Saved laps to file %s\n" filename;
  Ok ()

let process_user token =
  let resp = athlete_info token in
  let timestamp = Time_ns.now () |> Time_ns.to_string in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let filename = sprintf "%s_raw_user.json" timestamp in
  Yojson.Safe.to_file filename json;
  printf "Saved raw user info to file %s\n" filename;
  let%bind strava_athlete =
    Or_error.try_with (fun () -> StravaAthlete.t_of_yojson json)
  in
  let out = [%yojson_of: StravaAthlete.t] strava_athlete in
  let filename = sprintf "%s_user.json" timestamp in
  Yojson.Safe.to_file filename out;
  printf "Saved user info to file %s\n" filename;
  Ok ()

(* NOTE: I don't know where strava gets these zones., they are not the same
   as my suunto and not the same as what i had entered manually before. For
   now we ignore this data and maybe compute it ourselves *)
let process_zones token =
  let resp = zones token in
  let timestamp = Time_ns.now () |> Time_ns.to_string in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let filename = sprintf "%s_raw_zones.json" timestamp in
  Yojson.Safe.to_file filename json;
  printf "Saved raw zones info to file %s\n" filename;
  (* let%bind zones = Or_error.try_with (fun () -> StravaZones.t_of_yojson json) in *)
  (* let out = [%yojson_of: StravaZones.t] zones in *)
  (* let filename = sprintf "%s_zones.json" timestamp in *)
  (* Yojson.Safe.to_file filename out; *)
  (* printf "Saved zones info to file %s\n" filename; *)
  Ok ()

let process_activities token num_activities =
  let resp = list_activities ~token ~page:1 ~per_page:num_activities () in
  let timestamp = Time_ns.now () |> Time_ns.to_string in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let filename = sprintf "%s_activities.json" timestamp in
  Yojson.Safe.to_file filename json;
  printf "Saved activities to file %s\n" filename;
  let%bind strava_activities =
    Or_error.try_with (fun () -> StravaActivities.t_of_yojson json)
  in
  let activities =
    List.map ~f:Models.Activity.t_of_StravaActivity strava_activities
  in
  let activities =
    List.map
      ~f:(fun activity ->
        printf "processing activity=%d %s\n" activity.id activity.start_date;
        printf "downloading streams\n";
        let streams = pull_streams_aux token activity.id in
        printf "downloading laps\n";
        let laps = pull_laps_aux token activity.id in
        match (streams, laps) with
        | Ok streams, Ok laps ->
            printf "successfully downloaded laps & streams\n";
            let laps = Laps.t_of_StravaLaps laps in
            printf "calculating stats\n";
            Models.Activity.calculate_stats activity streams laps
        | Error e, Ok _ -> Error.raise e
        | Ok _, Error e -> Error.raise e
        | Error e1, Error e2 ->
            failwith
              (sprintf "Errors:\n%s\n%s\n" (Error.to_string_hum e1)
                 (Error.to_string_hum e2)))
      activities
  in
  let out = [%yojson_of: Models.Activity.t list] activities in
  let filename = sprintf "%s_stats.json" timestamp in
  Yojson.Safe.to_file filename out;
  printf "Saved stats to file %s\n" filename;
  Ok ()

let process_activity ~token (activity : Models.Activity.t) =
  printf "\tprocessing activity=%d %s\n" activity.id activity.start_date;

  Time_ns.pause (Time_ns.Span.create ~ms:500 ());
  printf "\t\tdownloading streams\n";
  let streams = pull_streams_aux token activity.id in

  Time_ns.pause (Time_ns.Span.create ~ms:500 ());
  printf "\t\tdownloading laps\n";
  let laps = pull_laps_aux token activity.id in

  let err_prefix = "error while downloading/parsing" in
  match (streams, laps) with
  | Ok streams, Ok laps ->
      printf "\t\tsuccessfully downloaded laps & streams\n";
      let laps = Laps.t_of_StravaLaps laps in
      printf "\t\tcalculating stats\n";
      let activity = Models.Activity.calculate_stats activity streams laps in
      Some activity
  | Error e, Ok _ ->
      printf "\t\t%s streams: %s\n" err_prefix (Error.to_string_hum e);
      None
  | Ok _, Error e ->
      printf "\t\t%s laps: %s\n" err_prefix (Error.to_string_hum e);
      None
  | Error e1, Error e2 ->
      printf "\t\t%s laps & streams:\n%s\n%s\n" err_prefix
        (Error.to_string_hum e1) (Error.to_string_hum e2);
      None

let fetch_athlete ~token =
  let resp = athlete_info token in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let%bind strava_athlete =
    Or_error.try_with (fun () -> StravaAthlete.t_of_yojson json)
  in
  Ok strava_athlete

let fetch_one_page ~token ~page ~per_page =
  printf "downloading activity page %d (per_page=%d)\n" page per_page;
  let resp = list_activities ~token ~page ~per_page () in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  (* Yojson.Safe.to_file *)
  (*   (sprintf "activities_page_%d_per_%d.json" page per_page) *)
  (*   json; *)
  let%bind strava_activities =
    Or_error.try_with (fun () -> StravaActivities.t_of_yojson json)
  in
  let activities =
    List.map ~f:Models.Activity.t_of_StravaActivity strava_activities
  in
  Ok activities

let filter_activities (activities : Models.Activity.t list) exclude =
  List.filter
    ~f:(fun activity ->
      match List.exists ~f:(Int.equal activity.id) exclude with
      | true ->
          printf "...skipping activity (%d) - already exists\n" activity.id;
          false
      | false -> true)
    activities

let fetch_activities ?(max_pages = 1) ~token ~num_activities ~start_page
    ~exclude () =
  let per_page = 100 in
  let page_list = List.init max_pages ~f:(Int.( + ) 1) in
  let activities =
    List.fold ~init:[]
      ~f:(fun acc page_num ->
        if List.length acc >= num_activities then (
          printf "reached num_activities=%d, skipping page=%d\n" num_activities
            page_num;
          acc)
        else if page_num < start_page then (
          printf "skipping page=%d (start_page=%d)\n" page_num start_page;
          acc)
        else
          let activities = fetch_one_page ~token ~page:page_num ~per_page in
          match activities with
          | Error e ->
              printf "failed to fetch activities on page=%d per_page=%d:\n%s\n"
                page_num per_page (Error.to_string_hum e);
              acc
          | Ok activities ->
              printf "found %d activities on page=%d\n" (List.length activities)
                page_num;
              let activities =
                List.filter
                  ~f:(fun activity ->
                    match List.exists ~f:(Int.equal activity.id) exclude with
                    | true ->
                        printf "...skipping activity (%d) - already exists\n"
                          activity.id;
                        false
                    | false -> true)
                  activities
              in
              printf "%d activities after filtering\n" (List.length activities);
              let remaining = num_activities - List.length acc in
              printf "take %d activities\n" remaining;
              acc @ List.take activities remaining)
      page_list
  in
  let activities =
    activities |> List.map ~f:(process_activity ~token) |> List.filter_opt
  in
  Ok activities

(* TODO: new stuff - where to put it ? *)
let deg2rad angle = angle /. 180. *. Float.pi

(** computes distance between 2 geo points in kms *)
let haversine_distance point1 point2 =
  let lat1, lon1 = (List.nth_exn point1 0, List.nth_exn point1 1) in
  let lat2, lon2 = (List.nth_exn point2 0, List.nth_exn point2 1) in
  let open Float in
  let r = 6371. in
  let dLat = deg2rad (lat2 - lat1) in
  let dLon = deg2rad (lon2 - lon1) in
  let a =
    square (sin (dLat / 2.))
    + (cos (deg2rad lat1) * cos (deg2rad lat2) * square (sin (dLon / 2.)))
  in
  let c = 2. * atan2 (sqrt a) (sqrt (1. -. a)) in
  r * c

(** Measure the distance from a point to line (formed by start and end points).
    It is calculated by creating a triangle from all 3 points and then
    calculating the height of the perpendicular line starting from the point to
    the line. *)
let distance_from_line ~start ~end_ ~point =
  let open Float in
  (* calculate the lengths of the 3 sides of the triangle *)
  let line_dist = haversine_distance start end_ in
  let start_to_point_dist = haversine_distance start point in
  let end_to_point_dist = haversine_distance end_ point in

  (* calculate the triangle's semi perimeter: s = (a + b + c) / 2 *)
  let semi_perimeter =
    (line_dist + start_to_point_dist + end_to_point_dist) / 2.
  in
  (* calculate the triangle's area (heron's formula) *)
  let area =
    sqrt
      (semi_perimeter
      * (semi_perimeter - line_dist)
      * (semi_perimeter - start_to_point_dist)
      * (semi_perimeter - end_to_point_dist))
  in
  (* calculate the height of the perpendicular from the point to the line *)
  let distance_from_line = 2. * area / line_dist in
  distance_from_line

(** Normalize route with Douglas-Peucker algorithm.

    More info can be found here:
    https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
*)
let normalize_route ~(threshold : float) (points : float list list) =
  let rec normalize_aux start_idx end_idx points to_keep =
    let start_point = List.nth_exn points start_idx in
    let end_point = List.nth_exn points end_idx in

    (* loop through all points between start and end idx and find the point
       furthest from the line *)
    let max_idx, max_distance, _max_point =
      List.foldi
        ~init:(0, 0.0, [ 0.0; 0.0 ])
          (* keep track of current max idx, distance, point *)
        ~f:(fun i (curr_max_idx, curr_max_dist, curr_max_point) point ->
          let distance =
            if i > start_idx && i < end_idx then
              distance_from_line ~start:start_point ~end_:end_point ~point
            else 0.
          in
          if Float.(distance > curr_max_dist) then (i, distance, point)
          else (curr_max_idx, curr_max_dist, curr_max_point))
        points
    in

    if Float.(max_distance > threshold) then
      let to_keep = max_idx :: to_keep in
      let to_keep1 = normalize_aux start_idx max_idx points [] in
      let to_keep2 = normalize_aux max_idx end_idx points [] in
      to_keep @ to_keep1 @ to_keep2
    else to_keep
  in

  assert (List.length points > 2);
  let start_idx = 0 in
  let end_idx = List.length points - 1 in
  let to_keep = normalize_aux start_idx end_idx points [ start_idx; end_idx ] in
  let to_keep = List.dedup_and_sort ~compare:Int.compare to_keep in
  List.fold ~init:[]
    ~f:(fun acc point_idx -> List.nth_exn points point_idx :: acc)
    to_keep

(* NOTE: this is geohash specific table *)
let base32 =
  [
    "0";
    "1";
    "2";
    "3";
    "4";
    "5";
    "6";
    "7";
    "8";
    "9";
    "b";
    "c";
    "d";
    "e";
    "f";
    "g";
    "h";
    "j";
    "k";
    "m";
    "n";
    "p";
    "q";
    "r";
    "s";
    "t";
    "u";
    "v";
    "w";
    "x";
    "y";
    "z";
  ]

module GeoHashState = struct
  type t = {
    lat_min : float;
    lat_max : float;
    lng_min : float;
    lng_max : float;
    even_bit : bool;
    index : int;
    bit : int;
    hash : string list;
  }

  let make () =
    {
      lat_min = -90.;
      lat_max = 90.;
      lng_min = -180.;
      lng_max = 180.;
      even_bit = true;
      index = 0;
      bit = 0;
      hash = [];
    }

  let bisect_east_west_longitude t lng =
    let lng_mid = (t.lng_min +. t.lng_max) /. 2. in
    let upper_range = Float.(lng >= lng_mid) in
    {
      t with
      lng_min = (if upper_range then lng_mid else t.lng_min);
      index = (if upper_range then (t.index * 2) + 1 else t.index * 2);
      lng_max = (if upper_range then t.lng_max else lng_mid);
    }

  let bisect_north_south_latitude t lat =
    let lat_mid = (t.lat_min +. t.lat_max) /. 2. in
    let upper_range = Float.(lat >= lat_mid) in
    {
      t with
      lat_min = (if upper_range then lat_mid else t.lat_min);
      index = (if upper_range then (t.index * 2) + 1 else t.index * 2);
      lat_max = (if upper_range then t.lat_max else lat_mid);
    }

  let do_encode_step t lat lng =
    let t =
      if t.even_bit then bisect_east_west_longitude t lng
      else bisect_north_south_latitude t lat
    in

    let t = { t with bit = t.bit + 1 } in
    let t =
      if t.bit = 5 then
        {
          t with
          hash = t.hash @ [ List.nth_exn base32 t.index ];
          bit = 0;
          index = 0;
        }
      else t
    in

    { t with even_bit = not t.even_bit }
end

(** Create geohash from a gps point [latitude; longitude].
    - prevision determines the accuracy of the geohash i.e. 7=76meters,
      8=19meters

    Geohash wiki: https://en.wikipedia.org/wiki/Geohash

    To visualize geohashes: https://geohash.softeng.co/ezs42pke *)
let geohash_from_latlng ?(precision = 8) (point : float list) =
  assert (List.length point = 2);
  let lat = List.nth_exn point 0 in
  let lng = List.nth_exn point 1 in
  let geohash = GeoHashState.make () in

  (* TODO: this looks very ugly and not really functional style - how to improve it? *)
  let rec geohash_aux (state : GeoHashState.t) =
    if List.length state.hash < precision then
      geohash_aux (GeoHashState.do_encode_step state lat lng)
    else state
  in

  let geohash = geohash_aux geohash in
  String.concat geohash.hash

type direction = North | South | East | West [@@deriving eq]

let hash_neighbors =
  [
    ( North,
      [ "p0r21436x8zb9dcf5h7kjnmqesgutwvy"; "bc01fg45238967deuvhjyznpkmstqrwx" ]
    );
    ( South,
      [ "14365h7k9dcfesgujnmqp0r2twvyx8zb"; "238967debc01fg45kmstqrwxuvhjyznp" ]
    );
    ( East,
      [ "bc01fg45238967deuvhjyznpkmstqrwx"; "p0r21436x8zb9dcf5h7kjnmqesgutwvy" ]
    );
    ( West,
      [ "238967debc01fg45kmstqrwxuvhjyznp"; "14365h7k9dcfesgujnmqp0r2twvyx8zb" ]
    );
  ]

let hash_borders =
  [
    (North, [ "prxz"; "bcfguvyz" ]);
    (South, [ "028b"; "0145hjnp" ]);
    (East, [ "bcfguvyz"; "prxz" ]);
    (West, [ "0145hjnp"; "028b" ]);
  ]

(* NOTE: https://chatgpt.com/s/t_693b205c31948191b5ae5f811f7451d6 *)
let rec calculate_adjacent (hash : string) (dir : direction) =
  let hash_len = String.length hash in
  let last_char = String.nget hash (hash_len - 1) in

  (* Each geohash is in rectangular shape. Even length hashes are tall and odd
     length hashes are wide. The type_idx is holding information if the hash is
     even length or odd. Based on this information we index into map containing
     the chars for bordering hashes. *)
  let type_idx = hash_len % 2 in

  let dir_borders =
    List.Assoc.find_exn ~equal:equal_direction hash_borders dir
  in
  (* This includes all hashes at the direction border *)
  let dir_border = List.nth_exn dir_borders type_idx in

  let parent = String.slice hash 0 (hash_len - 1) in
  let parent =
    (* If the last char of the hash is at the border -> the neighbor is in another hash *)
    if String.contains dir_border last_char then calculate_adjacent parent dir
    else parent
  in

  let neighbors =
    List.Assoc.find_exn ~equal:equal_direction hash_neighbors dir
  in
  let neighbor_base = List.nth_exn neighbors type_idx in
  let neighbor_idx = String.index_exn neighbor_base last_char in
  let neighbor = List.nth_exn base32 neighbor_idx in
  parent ^ neighbor

let calculate_all_neighbors (hash : string) : string list =
  let north = calculate_adjacent hash North in
  let south = calculate_adjacent hash South in
  let east = calculate_adjacent hash East in
  let west = calculate_adjacent hash West in
  let north_east = calculate_adjacent north East in
  let north_west = calculate_adjacent north West in
  let south_east = calculate_adjacent south East in
  let south_west = calculate_adjacent south West in
  [ north; south; east; west; north_east; north_west; south_east; south_west ]

type route_hash8 = string list list [@@deriving yojson]

let hash_route_precise (streams : Streams.t) : route_hash8 option =
  let open Option.Let_syntax in
  let threshold = 0.02 in
  let%bind latlng_points = Streams.latlng_points streams in
  let points = normalize_route ~threshold latlng_points in
  Some
    (List.map
       ~f:(fun point ->
         let hash = geohash_from_latlng point in
         let neighbors = calculate_all_neighbors hash in
         hash :: neighbors)
       points)

(* TODO: any 2 consecutive same hashes should collapse to a single hash. This
   will make sure we have a more uniform processing ??  *)
(* TODO: maybe for coarse hashes i should use a bigger threshold to make sure
   that small variations in gps data does not add additional points in the
   hash? *)
let hash_route_coarse (streams : Streams.t) : string list option =
  let open Option.Let_syntax in
  (* TODO: both coarse and precise hashing should use the same normalized route
     so maybe this should be done somewhere else ? *)
  let threshold = 0.02 in
  let%bind latlng_points = Streams.latlng_points streams in
  let points = normalize_route ~threshold latlng_points in
  Some (List.map ~f:(geohash_from_latlng ~precision:7) points)

(* TODO: how to calculate the similarity for routes that have different amount of points ? *)

let match_cost (hash_a : string list) (hash_b : string list) =
  let main_hash_a = List.hd_exn hash_a in
  let neighbors_a = List.tl_exn hash_a in

  let main_hash_b = List.hd_exn hash_b in
  let neighbors_b = List.tl_exn hash_b in

  if String.(main_hash_a = main_hash_b) then 0.
  else if
    (* if point A appears in B neightbors OR point B appears in A neightbors *)
    List.count ~f:(fun h -> String.(main_hash_a = h)) neighbors_b > 0
    || List.count ~f:(fun h -> String.(main_hash_b = h)) neighbors_a > 0
  then 0.5
  else 1.

(** Calculate route similarity by using dynamic time warping. This technique
    deals with routes that can have different number of points. More info can be
    found here: https://en.wikipedia.org/wiki/Dynamic_time_warping *)
let calculate_route_similarity_new ~(route_a : route_hash8)
    ~(route_b : route_hash8) : float =
  (* TODO: should this be called - route_difference (cause it returns 0.0
     for equal routes and 1.0 for completely different routes ? *)
  let len_a = List.length route_a in
  let len_b = List.length route_b in
  assert (len_a > 0 && len_b > 0);
  let dtw =
    Array.make_matrix ~dimx:(len_a + 1) ~dimy:(len_b + 1) Float.infinity
  in
  dtw.(0).(0) <- 0.;

  for i = 1 to len_a do
    for j = 1 to len_b do
      let cost =
        match_cost (List.nth_exn route_a (i - 1)) (List.nth_exn route_b (j - 1))
      in
      dtw.(j).(j) <-
        cost
        +. Float.min
             (Float.min dtw.(i - 1).(j) dtw.(i).(j - 1))
             dtw.(i - 1).(j - 1)
    done
  done;
  dtw.(len_a).(len_b) /. float_of_int (max len_a len_b)

let calculate_route_similarity ~(base : route_hash8) ~(candidate : route_hash8)
    : float =
  (* TODO: should this count neighbor matches as 0.5 or same as main hash matches ? *)
  printf "base: %d candidate: %d" (List.length base) (List.length candidate);
  assert (List.length base = List.length candidate);
  let matches =
    List.foldi ~init:0.0
      ~f:(fun i acc hash ->
        let base_hash = List.hd_exn hash in
        let candidate_hash = List.nth_exn candidate i in
        let candidate_main_hash = List.hd_exn candidate_hash in
        let candidate_neighbor_hashes = List.tl_exn candidate_hash in
        if String.(base_hash = candidate_main_hash) then acc +. 1.0
        else if
          List.count
            ~f:(fun h -> String.(base_hash = h))
            candidate_neighbor_hashes
          > 0
        then acc +. 0.5
        else acc)
      base
  in
  let total_points = List.length base in
  matches /. float_of_int total_points

let%expect_test "calculate_adjacent" =
  let hash = "u9dp0n7e" in
  let north = calculate_adjacent hash North in
  printf "N:%s," north;
  let south = calculate_adjacent hash South in
  printf "S:%s," south;
  let east = calculate_adjacent hash East in
  printf "E:%s," east;
  let west = calculate_adjacent hash West in
  printf "W:%s," west;
  [%expect {| N:u9dp0n7s,S:u9dp0n7d,E:u9dp0n7g,W:u9dp0n77, |}]

let%expect_test "calculate_adjacent_top_rigth_corner" =
  let hash = "u9dp0n7z" in
  let north = calculate_adjacent hash North in
  printf "N:%s," north;
  let south = calculate_adjacent hash South in
  printf "S:%s," south;
  let east = calculate_adjacent hash East in
  printf "E:%s," east;
  let west = calculate_adjacent hash West in
  printf "W:%s," west;
  [%expect {| N:u9dp0neb,S:u9dp0n7y,E:u9dp0nkp,W:u9dp0n7x, |}]

let%expect_test "calculate_adjacent_bottom_left_corner" =
  let hash = "u9dp0n70" in
  let north = calculate_adjacent hash North in
  printf "N:%s," north;
  let south = calculate_adjacent hash South in
  printf "S:%s," south;
  let east = calculate_adjacent hash East in
  printf "E:%s," east;
  let west = calculate_adjacent hash West in
  printf "W:%s," west;
  [%expect {| N:u9dp0n71,S:u9dp0n5p,E:u9dp0n72,W:u9dp0n6b, |}]

let%expect_test "calculate_all_neighbors" =
  let hash = "u9dp0n70" in
  let neighbors = calculate_all_neighbors hash in
  List.iter ~f:(fun hash -> printf "%s," hash) neighbors;
  [%expect
    {| u9dp0n71,u9dp0n5p,u9dp0n72,u9dp0n6b,u9dp0n73,u9dp0n6c,u9dp0n5r,u9dp0n4z, |}]

let%expect_test "hash_route_precise" =
  let json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/5kloop_streams_16575000264.json"
  in
  let streams = Streams.t_of_yojson_smoothed json in
  let hash = Option.value_exn (hash_route_precise streams) in
  let json = yojson_of_route_hash8 hash in
  printf "%s" (Yojson.Safe.to_string json);
  [%expect
    {| [["u9dp0n7e","u9dp0n7s","u9dp0n7d","u9dp0n7g","u9dp0n77","u9dp0n7u","u9dp0n7k","u9dp0n7f","u9dp0n76"],["u9dp0n6q","u9dp0n6r","u9dp0n6m","u9dp0n6w","u9dp0n6n","u9dp0n6x","u9dp0n6p","u9dp0n6t","u9dp0n6j"],["u9dp0ndd","u9dp0nde","u9dp0nd9","u9dp0ndf","u9dp0nd6","u9dp0ndg","u9dp0nd7","u9dp0ndc","u9dp0nd3"],["u9dp0n8y","u9dp0n8z","u9dp0n8v","u9dp0n9n","u9dp0n8w","u9dp0n9p","u9dp0n8x","u9dp0n9j","u9dp0n8t"],["u99zpyxv","u99zpyxy","u99zpyxu","u9dp0n8j","u99zpyxt","u9dp0n8n","u99zpyxw","u9dp0n8h","u99zpyxs"],["u99zpyfe","u99zpyfs","u99zpyfd","u99zpyfg","u99zpyf7","u99zpyfu","u99zpyfk","u99zpyff","u99zpyf6"],["u99zpybd","u99zpybe","u99zpyb9","u99zpybf","u99zpyb6","u99zpybg","u99zpyb7","u99zpybc","u99zpyb3"],["u99zpw82","u99zpw83","u99zpw2r","u99zpw88","u99zpw80","u99zpw89","u99zpw81","u99zpw2x","u99zpw2p"],["u99zpqwc","u99zpqwf","u99zpqwb","u99zpqx1","u99zpqw9","u99zpqx4","u99zpqwd","u99zpqx0","u99zpqw8"],["u99zpqqw","u99zpqqx","u99zpqqt","u99zpqqy","u99zpqqq","u99zpqqz","u99zpqqr","u99zpqqv","u99zpqqm"],["u99zptep","u99zptg0","u99zpten","u99zpter","u99zptdz","u99zptg2","u99zptfb","u99zpteq","u99zptdy"],["u99zptm1","u99zptm4","u99zptm0","u99zptm3","u99zptkc","u99zptm6","u99zptkf","u99zptm2","u99zptkb"],["u99zptnw","u99zptnx","u99zptnt","u99zptny","u99zptnq","u99zptnz","u99zptnr","u99zptnv","u99zptnm"],["u99zptnb","u99zptnc","u99zpsyz","u99zptp0","u99zptn8","u99zptp1","u99zptn9","u99zpszp","u99zpsyx"],["u99zpu9x","u99zpuc8","u99zpu9w","u99zpu9z","u99zpu9r","u99zpucb","u99zpuc2","u99zpu9y","u99zpu9q"],["u99zpudq","u99zpudr","u99zpudm","u99zpudw","u99zpudn","u99zpudx","u99zpudp","u99zpudt","u99zpudj"],["u99zpvh2","u99zpvh3","u99zpuur","u99zpvh8","u99zpvh0","u99zpvh9","u99zpvh1","u99zpuux","u99zpuup"],["u99zpvnw","u99zpvnx","u99zpvnt","u99zpvny","u99zpvnq","u99zpvnz","u99zpvnr","u99zpvnv","u99zpvnm"],["u9dp0j2r","u9dp0j82","u9dp0j2q","u9dp0j2x","u9dp0j2p","u9dp0j88","u9dp0j80","u9dp0j2w","u9dp0j2n"],["u9dp0n6q","u9dp0n6r","u9dp0n6m","u9dp0n6w","u9dp0n6n","u9dp0n6x","u9dp0n6p","u9dp0n6t","u9dp0n6j"],["u9dp0n7s","u9dp0n7t","u9dp0n7e","u9dp0n7u","u9dp0n7k","u9dp0n7v","u9dp0n7m","u9dp0n7g","u9dp0n77"]] |}]

let%expect_test "hash_route_coarse" =
  let json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/5kloop_streams_16575000264.json"
  in
  let streams = Streams.t_of_yojson_smoothed json in
  let hash = Option.value_exn (hash_route_coarse streams) in
  printf "%s" (String.concat ~sep:"," hash);
  [%expect
    {| u9dp0n7,u9dp0n6,u9dp0nd,u9dp0n8,u99zpyx,u99zpyf,u99zpyb,u99zpw8,u99zpqw,u99zpqq,u99zpte,u99zptm,u99zptn,u99zptn,u99zpu9,u99zpud,u99zpvh,u99zpvn,u9dp0j2,u9dp0n6,u9dp0n7 |}]

(* let%expect_test "calculate_route_similarity" = *)
(*   let json1 = *)
(*     Yojson.Safe.from_file *)
(*       "/home/angel/Documents/ocaml/unto/5kloop_streams_16575000264.json" *)
(*   in *)
(*   let streams1 = Streams.t_of_yojson_smoothed json1 in *)
(*   let hash1 = Option.value_exn (hash_route_precise streams1) in *)
(*   let json2 = *)
(*     Yojson.Safe.from_file *)
(*       "/home/angel/Documents/ocaml/unto/5kloop2_16434068435.json" *)
(*   in *)
(*   let streams2 = Streams.t_of_yojson_smoothed json2 in *)
(*   let hash2 = Option.value_exn (hash_route_precise streams2) in *)
(*   let json3 = *)
(*     Yojson.Safe.from_file *)
(*       "/home/angel/Documents/ocaml/unto/4k_sim_loop_16487742395.json" *)
(*   in *)
(*   let streams3 = Streams.t_of_yojson_smoothed json3 in *)
(*   let hash3 = Option.value_exn (hash_route_precise streams3) in *)
(*   let json4 = *)
(*     Yojson.Safe.from_file *)
(*       "/home/angel/Documents/ocaml/unto/5k_not_loop_16463319760.json" *)
(*   in *)
(*   let streams4 = Streams.t_of_yojson_smoothed json4 in *)
(*   let hash4 = Option.value_exn (hash_route_precise streams4) in *)
(*   let similarity_1_2 = *)
(*     calculate_route_similarity ~base:hash1 ~candidate:hash2 *)
(*   in *)
(*   let similarity_1_3 = *)
(*     calculate_route_similarity ~base:hash1 ~candidate:hash3 *)
(*   in *)
(*   let similarity_1_4 = *)
(*     calculate_route_similarity ~base:hash1 ~candidate:hash4 *)
(*   in *)
(*   printf "Route 1<%f>2; Route 1<%f>3; Route 1<%f>4" similarity_1_2 *)
(*     similarity_1_3 similarity_1_4; *)
(*   [%expect {||}] *)

let%expect_test "calculate_route_similarity_dtw" =
  let route_a = [ [ "a"; "a1" ] ] in
  let route_b = [ [ "b"; "b1" ] ] in
  let similarity = calculate_route_similarity_new ~route_a ~route_b in
  printf "Different routes with same amount of points: %f" similarity;
  [%expect {| Different routes with same amount of points: 1.000000 |}];

  let similarity = calculate_route_similarity_new ~route_a ~route_b:route_a in
  printf "Same routes with same amount of points: %f" similarity;
  [%expect {| Same routes with same amount of points: 0.000000 |}];

  let route_ab = [ [ "a"; "b1" ] ] in
  let similarity = calculate_route_similarity_new ~route_a ~route_b:route_ab in
  printf
    "Same routes (same hash but different neighbors) with same amount of \
     points: %f"
    similarity;
  [%expect
    {| Same routes (same hash but different neighbors) with same amount of points: 0.000000 |}]
(* TODO: continue to add more tests here *)

let%expect_test "geohash_from_latlng" =
  let loop1_start = [ 54.70299; 25.317408 ] in
  let hash = geohash_from_latlng loop1_start in
  printf "%s" hash;
  [%expect {| u9dp0n7s |}]

let%expect_test "geohash_from_latlng2" =
  let loop1_start = [ 54.70365225960127; 25.319627533102445 ] in
  let hash = geohash_from_latlng loop1_start in
  printf "%s" hash;
  [%expect {| u9dp0nmp |}]

let%expect_test "normalize_route" =
  let json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/5kloop_streams_16575000264.json"
  in
  let streams = Streams.t_of_yojson_smoothed json in
  let threshold = 0.02 in
  let latlng_points = Streams.latlng_points streams in
  Option.iter latlng_points ~f:(fun points ->
      let points = normalize_route ~threshold points in
      List.iter
        ~f:(fun pt ->
          printf "[%f, %f] " (List.nth_exn pt 0) (List.nth_exn pt 1))
        points;
      printf "\nThreshold:%f Initial:%d Final:%d\n" threshold
        (List.length points) (List.length points));
  [%expect
    {|
    [54.702978, 25.317535] [54.703360, 25.315758] [54.704130, 25.316015] [54.704773, 25.313622] [54.704548, 25.312432] [54.705585, 25.305288] [54.705488, 25.302508] [54.703718, 25.291140] [54.703872, 25.288903] [54.703490, 25.288602] [54.699428, 25.294660] [54.697145, 25.297430] [54.696523, 25.299663] [54.695500, 25.299957] [54.693957, 25.303665] [54.693733, 25.304918] [54.695475, 25.307480] [54.696563, 25.310775] [54.698098, 25.312912] [54.703378, 25.315702] [54.702990, 25.317408]
    Threshold:0.020000 Initial:21 Final:21 |}]

let%expect_test "haversine_distance" =
  let loop1_start = [ 54.70299; 25.317408 ] in
  let orient_start = [ 54.719005; 25.253187 ] in
  let d = haversine_distance loop1_start orient_start in
  printf "%f" d;
  [%expect {| 4.493334 |}]

let%expect_test "distance_from_line_acute_triangle" =
  let line_start = [ 54.703534; 25.315630 ] in
  let line_end = [ 54.70476848383893; 25.316215439902518 ] in
  let point = [ 54.70365225960127; 25.319627533102445 ] in
  let d = distance_from_line ~start:line_start ~end_:line_end ~point in
  printf "%f" d;
  [%expect {| 0.244230 |}]

let%expect_test "distance_from_line_acute_obtuse" =
  let line_start = [ 54.703534; 25.315630 ] in
  let line_end = [ 54.70476848383893; 25.316215439902518 ] in
  let point = [ 54.70496050212048; 25.318387068033555 ] in
  let d = distance_from_line ~start:line_start ~end_:line_end ~point in
  printf "%f" d;
  [%expect {| 0.128917 |}]

let%expect_test "distance_from_line_acute_obtuse_other_side" =
  let line_start = [ 54.703534; 25.315630 ] in
  let line_end = [ 54.70476848383893; 25.316215439902518 ] in
  let point = [ 54.70295308022377; 25.30984663815214 ] in
  let d = distance_from_line ~start:line_start ~end_:line_end ~point in
  printf "%f" d;
  [%expect {| 0.341305 |}]

(* 5.13km usual loop from 2025/11/26 16575000264 *)
(* 4.89km similar but not the same as usual loop from 2025/11/17 16487742395 *)
(* 5.31km different than loop but same start end 2025/11/15 16463319760 *)
(* 5.08km usual loop from 2025/11/12 16434068435 *)
(* 8.03km different than loop different start 2025/12/05 16649027802 *)

(* let%expect_test "deserialize get_stream.json" = *)
(*   let json = *)
(*     Yojson.Safe.from_file *)
(*       "/home/angel/Documents/ocaml/unto/get_streams_test.json" *)
(*   in *)
(*   let streams = Streams.t_of_yojson_strava json in *)
(*   printf "%s" (Streams.show streams); *)
(*   [%expect *)
(*     {| *)
(*     [(TimeStream *)
(*         { type_ = "time"; *)
(*           data = *)
(*           [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; *)
(*             20]; *)
(*           smoothed = <opaque>; series_type = "distance"; original_size = 21; *)
(*           resolution = "high" }); *)
(*       (DistanceStream *)
(*          { type_ = "distance"; *)
(*            data = *)
(*            [0.; 2.8; 5.5; 8.3; 11.; 13.8; 16.5; 19.3; 22.; 27.; 32.; 36.; 39.; *)
(*              42.; 45.; 48.; 51.; 54.; 57.; 60.; 64.]; *)
(*            smoothed = <opaque>; series_type = "distance"; original_size = 21; *)
(*            resolution = "high" }); *)
(*       (LatLngStream *)
(*          { type_ = "latlng"; *)
(*            data = *)
(*            [[54.70304; 25.317412]; [54.703055; 25.317374]; *)
(*              [54.703069; 25.317336]; [54.703084; 25.317297]; *)
(*              [54.703099; 25.317259]; [54.703113; 25.317221]; *)
(*              [54.703128; 25.317183]; [54.703143; 25.317144]; *)
(*              [54.703158; 25.317106]; [54.703175; 25.31704]; *)
(*              [54.70319; 25.316983]; [54.703203; 25.316937]; [54.70321; 25.31689]; *)
(*              [54.703218; 25.316848]; [54.703228; 25.3168]; *)
(*              [54.703233; 25.316753]; [54.70324; 25.31671]; *)
(*              [54.703245; 25.316665]; [54.703255; 25.31662]; *)
(*              [54.703265; 25.316572]; [54.703273; 25.316523]]; *)
(*            smoothed = <opaque>; series_type = "distance"; original_size = 21; *)
(*            resolution = "high" }); *)
(*       (AltitudeStream *)
(*          { type_ = "altitude"; *)
(*            data = *)
(*            [115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 114.6; *)
(*              114.2; 114.; 113.8; 113.8; 113.4; 113.4; 113.2; 113.2; 112.8; 112.8; *)
(*              112.6]; *)
(*            smoothed = <opaque>; series_type = "distance"; original_size = 21; *)
(*            resolution = "high" }); *)
(*       (VelocityStream *)
(*          { type_ = "velocity_smooth"; *)
(*            data = *)
(*            [0.; 0.; 2.75; 2.75; 2.75; 2.75; 2.75; 2.75; 2.75; 3.2; 3.65; 3.9; *)
(*              3.95; 4.; 3.6; 3.2; 3.; 3.; 3.; 3.; 3.2]; *)
(*            smoothed = <opaque>; series_type = "distance"; original_size = 21; *)
(*            resolution = "high" }); *)
(*       (HeartRateStream *)
(*          { type_ = "heartrate"; *)
(*            data = *)
(*            [80; 79; 78; 77; 77; 75; 75; 75; 76; 78; 80; 80; 82; 84; 87; 89; 91; *)
(*              95; 97; 99; 100]; *)
(*            smoothed = <opaque>; series_type = "distance"; original_size = 21; *)
(*            resolution = "high" }); *)
(*       (CadenceStream *)
(*          { type_ = "cadence"; *)
(*            data = *)
(*            [84; 84; 84; 84; 84; 84; 84; 84; 84; 85; 86; 86; 86; 85; 84; 83; 82; *)
(*              82; 82; 82; 82]; *)
(*            smoothed = <opaque>; series_type = "distance"; original_size = 21; *)
(*            resolution = "high" }); *)
(*       (WattsStream *)
(*          { type_ = "watts"; *)
(*            data = *)
(*            [None; None; None; None; None; None; None; None; (Some 283); *)
(*              (Some 243); (Some 279); (Some 273); (Some 263); (Some 261); *)
(*              (Some 219); (Some 220); (Some 201); (Some 209); (Some 193); *)
(*              (Some 200); (Some 198)]; *)
(*            smoothed = <opaque>; series_type = "distance"; original_size = 21; *)
(*            resolution = "high" }); *)
(*       (TempStream *)
(*          { type_ = "temp"; *)
(*            data = *)
(*            [29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; *)
(*              29; 29; 29; 29]; *)
(*            smoothed = <opaque>; series_type = "distance"; original_size = 21; *)
(*            resolution = "high" }); *)
(*       (GradeStream *)
(*          { type_ = "grade_smooth"; *)
(*            data = *)
(*            [0.; 0.; 0.; 0.; 0.; 0.; 0.; -4.5; -6.5; -7.2; -8.2; -5.3; -6.2; -5.; *)
(*              -5.; -5.; -5.; -5.; -4.6; -4.6; -3.1]; *)
(*            smoothed = <opaque>; series_type = "distance"; original_size = 21; *)
(*            resolution = "high" }) *)
(*       ] *)
(*     |}] *)

(* let%expect_test "deserialize running activity" = *)
(*   let json = *)
(*     Yojson.Safe.from_file "/home/angel/Documents/ocaml/unto/one_activity.json" *)
(*   in *)
(*   let activities = StravaActivities.t_of_yojson json in *)
(*   printf "%s" (StravaActivities.show activities); *)
(*   [%expect *)
(*     {| *)
(*     [{ athlete = { id = 3504239 }; name = "Afternoon Run"; sport_type = "Run"; *)
(*        id = 14995177737; start_date_local = "2025-07-03T16:29:12Z"; *)
(*        timezone = "(GMT+02:00) Europe/Vilnius"; *)
(*        map = *)
(*        { id = "a14995177737"; *)
(*          summary_polyline = *)
(*          "wmemIkqzyCiAtDIv@k@bC}@jC_AxFWt@Pm@lA}GpBiGlAuEHq@Tk@t@eHD}@]oHQaA_@}@WwBOkBGIMw@aBlBu@^cAbAg@|@_AhAo@dBu@bAe@hAg@t@oAhAqDXc@a@g@]q@QCNJJXEnAjADv@f@~@FfDpBbHv@|Af@`Db@VHz@Nd@ZVKgAb@}GCaDL}@D{BRkAR[\\}APER_@b@}AVcBlAmCb@o@ZQhAJGO_@KS_@WmAsAmEo@oAGk@Wu@o@Gw@RKZBl@QcAWC]k@kBuBo@oBEcAJu@GMe@[YJoAcAe@_ACOFo@XuAAo@FoAHC?SLm@Ay@I_@a@u@_@KKLz@jAH^KrAGbB`@mBDo@Me@?]ZcBj@aAPGDPRJFIq@~Ag@zA[jB[r@Kl@c@Da@d@JPFAI@Rh@?RMu@@QKUd@OV{@Bo@GYIE\\Sh@sB\\Ed@k@Jq@R_@t@D^Vz@i@nA@T_ATUl@qAHgAf@OFR~@\\FF@ZRc@fAy@P?TRbAELr@VGNZ^JeAz@g@MWv@TQrAPNW|@uCp@wCPK`B`BvAbBt@f@Zk@`AqD\\aCpBkGBwAJSVGf@TbAo@n@aCj@n@|@d@l@TZCj@^nBWjAuALw@NBPa@XIH_@\\a@VWZOTy@VoAXyBt@aCh@aAJc@T_Ch@yCZo@Nu@Ng@VE|@zAUiCGU]WmA\\eCEQJe@fABc@E{@_AgDs@e@QmAEaAw@kCEo@y@m@c@q@gBIo@g@o@Ob@}A|@k@Za@l@G~@o@dCCDd@MfBBrAE|@m@lAu@bAc@lCBdBGp@@~@b@rAl@`@i@Sk@_AmARw@a@eAmAaAUWg@?Z[hAAn@Hf@Mb@?XVr@n@p@n@pAIt@Bb@GpB[t@SjAMPAj@S~@EzAIFAh@DrA_@|@Mx@cAfAI\\WRMVO~Ak@nBS`@BdAEVU\\Op@cA^Ya@]^E[o@FELFRVPVv@h@ZNf@YnBqAnEUh@K~@dBzC\\z@PwACYQACMLQ@_@a@RQj@UF_A_BKAKVeA|F_@pAYVm@LG\\]n@Qr@JhAEf@jBnBXl@@pEDJ|@j@LZJfAuAHw@bAoBz@M?[c@YMeCZTlCXpBs@{E}@eMk@}C^rBx@xGX~E" *)
(*          }; *)
(*        gear_id = (Some "g272465") } *)
(*       ] |}] *)

(* let%expect_test "deserialize crossfit activity" = *)
(*   let json = *)
(*     Yojson.Safe.from_file *)
(*       "/home/angel/Documents/ocaml/unto/crossfit_activity.json" *)
(*   in *)
(*   let activities = StravaActivities.t_of_yojson json in *)
(*   printf "%s" (StravaActivities.show activities); *)
(*   [%expect *)
(*     {| *)
(*     [{ athlete = { id = 3504239 }; name = "Lunch Crossfit"; *)
(*        sport_type = "Crossfit"; id = 15086886566; *)
(*        start_date_local = "2025-07-12T11:49:53Z"; *)
(*        timezone = "(GMT+03:00) Africa/Addis_Ababa"; *)
(*        map = { id = "a15086886566"; summary_polyline = "" }; gear_id = None } *)
(*       ] |}] *)

(* let%expect_test "process_streams" = *)
(*   let json = *)
(*     Yojson.Safe.from_file *)
(*       "/home/angel/Documents/ocaml/unto/streams_14995177737.json" *)
(*   in *)
(*   let streams = Streams.t_of_yojson_smoothed json in *)
(*   let stats = Streams.activity_stats streams in *)
(*   printf "%s" (Models.Stats.show stats); *)
(*   [%expect *)
(*     {| *)
(*     { data_points = 4889; moving_time = 4888; elapsed_time = 5562; *)
(*       distance = (Some 11033.); elev_gain = (Some 108); elev_loss = (Some 103); *)
(*       elev_high = (Some 140); elev_low = (Some 110); *)
(*       start_latlng = (Some (54.755563, 25.37736)); *)
(*       end_latlng = (Some (54.755553, 25.377283)); average_speed = (Some 2.258); *)
(*       max_speed = (Some 5.); average_cadence = (Some 75); *)
(*       max_cadence = (Some 99); average_temp = (Some 32); *)
(*       average_heartrate = (Some 169); max_heartrate = (Some 186); *)
(*       average_power = None; max_power = None } |}] *)

let%expect_test "altitude smoothing (exponential moving average)" =
  let data =
    [
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      114.6;
      114.2;
      114.0;
      113.8;
      113.8;
      113.4;
      113.4;
      113.2;
      113.2;
      112.8;
      112.8;
      112.6;
    ]
  in
  let ema = Utils.exponential_moving_average 0.2 data in
  List.iter ~f:(printf "%.2f ") ema;
  [%expect
    {| 115.20 115.20 115.20 115.20 115.20 115.20 115.20 115.20 115.20 115.08 114.90 114.72 114.54 114.39 114.19 114.03 113.87 113.73 113.55 113.40 113.24 |}]

let%expect_test "altitude smoothing (moving average)" =
  let data =
    [
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      114.6;
      114.2;
      114.0;
      113.8;
      113.8;
      113.4;
      113.4;
      113.2;
      113.2;
      112.8;
      112.8;
      112.6;
    ]
  in
  let ma = Utils.moving_average (module Utils.FloatOps) 5 data in
  List.iter ~f:(printf "%.2f ") ma;
  [%expect
    {| 115.20 115.20 115.20 115.20 115.14 115.05 114.95 114.82 114.69 114.53 114.36 114.18 114.00 113.78 113.56 113.38 113.30 113.22 113.15 113.06 113.00 |}]

let%expect_test "sportType of string" =
  let sport_str = "Run" in
  let sport = sportType_of_string sport_str in
  let success = match sport with Run -> true | _ -> false in
  printf "%b" success;
  [%expect {| true |}]

(* let%expect_test "activity_bug" = *)
(*   let json = *)
(*     Yojson.Safe.from_file *)
(*       "/home/angel/Documents/ocaml/unto/raw_streams_15666941870.json" *)
(*   in *)
(*   let streams = Streams.t_of_yojson_smoothed json in *)
(*   let stats = Streams.activity_stats streams in *)
(*   printf "%s" (Models.Stats.show stats); *)
(*   [%expect {| |}] *)
