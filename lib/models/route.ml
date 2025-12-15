open Core
open Streams
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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

module RouteHash = struct
  type t = string list list [@@deriving yojson]

  let of_streams (streams : Streams.t) : t option =
    let open Option.Let_syntax in
    let threshold = 0.02 in
    let%bind latlng_points = Streams.latlng_points streams in
    let points = normalize_route ~threshold latlng_points |> List.rev in
    Some
      (List.map
         ~f:(fun point ->
           let hash = geohash_from_latlng point in
           let neighbors = calculate_all_neighbors hash in
           hash :: neighbors)
         points)

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
      deals with routes that can have different number of points. Similarity of
      0.0 means the routes are identical whereas similarity of 1.0 means routes
      are completely diffrent. More info can be found here:
      https://en.wikipedia.org/wiki/Dynamic_time_warping *)
  let calculate_similarity ~(route_a : t) ~(route_b : t) : float =
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
          match_cost
            (List.nth_exn route_a (i - 1))
            (List.nth_exn route_b (j - 1))
        in
        dtw.(i).(j) <-
          cost
          +. Float.min
               (Float.min dtw.(i - 1).(j) dtw.(i).(j - 1))
               dtw.(i - 1).(j - 1)
      done
    done;
    1. -. (dtw.(len_a).(len_b) /. float_of_int (max len_a len_b))
end

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
  let hash = Option.value_exn (RouteHash.of_streams streams) in
  let json = RouteHash.yojson_of_t hash in
  printf "%s" (Yojson.Safe.to_string json);
  [%expect
    {| [["u9dp0n7s","u9dp0n7t","u9dp0n7e","u9dp0n7u","u9dp0n7k","u9dp0n7v","u9dp0n7m","u9dp0n7g","u9dp0n77"],["u9dp0n6q","u9dp0n6r","u9dp0n6m","u9dp0n6w","u9dp0n6n","u9dp0n6x","u9dp0n6p","u9dp0n6t","u9dp0n6j"],["u9dp0j2r","u9dp0j82","u9dp0j2q","u9dp0j2x","u9dp0j2p","u9dp0j88","u9dp0j80","u9dp0j2w","u9dp0j2n"],["u99zpvnw","u99zpvnx","u99zpvnt","u99zpvny","u99zpvnq","u99zpvnz","u99zpvnr","u99zpvnv","u99zpvnm"],["u99zpvh2","u99zpvh3","u99zpuur","u99zpvh8","u99zpvh0","u99zpvh9","u99zpvh1","u99zpuux","u99zpuup"],["u99zpudq","u99zpudr","u99zpudm","u99zpudw","u99zpudn","u99zpudx","u99zpudp","u99zpudt","u99zpudj"],["u99zpu9x","u99zpuc8","u99zpu9w","u99zpu9z","u99zpu9r","u99zpucb","u99zpuc2","u99zpu9y","u99zpu9q"],["u99zptnb","u99zptnc","u99zpsyz","u99zptp0","u99zptn8","u99zptp1","u99zptn9","u99zpszp","u99zpsyx"],["u99zptnw","u99zptnx","u99zptnt","u99zptny","u99zptnq","u99zptnz","u99zptnr","u99zptnv","u99zptnm"],["u99zptm1","u99zptm4","u99zptm0","u99zptm3","u99zptkc","u99zptm6","u99zptkf","u99zptm2","u99zptkb"],["u99zptep","u99zptg0","u99zpten","u99zpter","u99zptdz","u99zptg2","u99zptfb","u99zpteq","u99zptdy"],["u99zpqqw","u99zpqqx","u99zpqqt","u99zpqqy","u99zpqqq","u99zpqqz","u99zpqqr","u99zpqqv","u99zpqqm"],["u99zpqwc","u99zpqwf","u99zpqwb","u99zpqx1","u99zpqw9","u99zpqx4","u99zpqwd","u99zpqx0","u99zpqw8"],["u99zpw82","u99zpw83","u99zpw2r","u99zpw88","u99zpw80","u99zpw89","u99zpw81","u99zpw2x","u99zpw2p"],["u99zpybd","u99zpybe","u99zpyb9","u99zpybf","u99zpyb6","u99zpybg","u99zpyb7","u99zpybc","u99zpyb3"],["u99zpyfe","u99zpyfs","u99zpyfd","u99zpyfg","u99zpyf7","u99zpyfu","u99zpyfk","u99zpyff","u99zpyf6"],["u99zpyxv","u99zpyxy","u99zpyxu","u9dp0n8j","u99zpyxt","u9dp0n8n","u99zpyxw","u9dp0n8h","u99zpyxs"],["u9dp0n8y","u9dp0n8z","u9dp0n8v","u9dp0n9n","u9dp0n8w","u9dp0n9p","u9dp0n8x","u9dp0n9j","u9dp0n8t"],["u9dp0ndd","u9dp0nde","u9dp0nd9","u9dp0ndf","u9dp0nd6","u9dp0ndg","u9dp0nd7","u9dp0ndc","u9dp0nd3"],["u9dp0n6q","u9dp0n6r","u9dp0n6m","u9dp0n6w","u9dp0n6n","u9dp0n6x","u9dp0n6p","u9dp0n6t","u9dp0n6j"],["u9dp0n7e","u9dp0n7s","u9dp0n7d","u9dp0n7g","u9dp0n77","u9dp0n7u","u9dp0n7k","u9dp0n7f","u9dp0n76"]] |}]

let%expect_test "RouteHash.calculate_similarity" =
  let json1 =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/5kloop_streams_16575000264.json"
  in
  let streams1 = Streams.t_of_yojson_smoothed json1 in
  let hash1 = Option.value_exn (RouteHash.of_streams streams1) in
  let json2 =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/5kloop2_16434068435.json"
  in
  let streams2 = Streams.t_of_yojson_smoothed json2 in
  let hash2 = Option.value_exn (RouteHash.of_streams streams2) in
  let json3 =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/4k_sim_loop_16487742395.json"
  in
  let streams3 = Streams.t_of_yojson_smoothed json3 in
  let hash3 = Option.value_exn (RouteHash.of_streams streams3) in
  let json4 =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/5k_not_loop_16463319760.json"
  in
  let streams4 = Streams.t_of_yojson_smoothed json4 in
  let hash4 = Option.value_exn (RouteHash.of_streams streams4) in
  let similarity_1_2 =
    RouteHash.calculate_similarity ~route_a:hash1 ~route_b:hash2
  in
  let similarity_1_3 =
    RouteHash.calculate_similarity ~route_a:hash1 ~route_b:hash3
  in
  let similarity_1_4 =
    RouteHash.calculate_similarity ~route_a:hash1 ~route_b:hash4
  in
  printf "Route 1<%f>2; Route 1<%f>3; Route 1<%f>4" similarity_1_2
    similarity_1_3 similarity_1_4;
  [%expect {| Route 1<0.833333>2; Route 1<0.367647>3; Route 1<0.148148>4 |}]

let%expect_test "RouteHash.calculate_similarity_dtw" =
  let route_a = [ [ "a"; "a1" ] ] in
  let route_b = [ [ "b"; "b1" ] ] in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf "Different routes with same amount of points: %f" similarity;
  [%expect {| Different routes with same amount of points: 0.000000 |}];

  let similarity = RouteHash.calculate_similarity ~route_a ~route_b:route_a in
  printf "Same routes with same amount of points: %f" similarity;
  [%expect {| Same routes with same amount of points: 1.000000 |}];

  let route_a = [ [ "a"; "a1" ] ] in
  let route_b = [ [ "a"; "b1" ] ] in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf
    "Same routes (same hash but different neighbors) with same amount of \
     points: %f"
    similarity;
  [%expect
    {| Same routes (same hash but different neighbors) with same amount of points: 1.000000 |}];

  let route_a = [ [ "a"; "a1" ] ] in
  let route_b = [ [ "b"; "a" ] ] in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf
    "Same routes (different hash but same neighbors) with same amount of \
     points: %f"
    similarity;
  [%expect
    {| Same routes (different hash but same neighbors) with same amount of points: 0.500000 |}];

  let route_a = [ [ "a"; "b" ] ] in
  let route_b = [ [ "b"; "b1" ] ] in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf
    "Same routes (different hash but same neighbors) with same amount of \
     points: %f"
    similarity;
  [%expect
    {| Same routes (different hash but same neighbors) with same amount of points: 0.500000 |}];

  let route_a = [ [ "a"; "a1" ]; [ "b"; "b1" ]; [ "c"; "c1" ] ] in
  let route_b =
    [ [ "a"; "a1" ]; [ "b"; "b1" ]; [ "b"; "b1" ]; [ "c"; "c1" ] ]
  in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf
    "Same routes (same hash) with different amount of points (one point is \
     duplicated): %f"
    similarity;
  [%expect
    {| Same routes (same hash) with different amount of points (one point is duplicated): 1.000000 |}];

  let route_a = [ [ "a"; "a1" ]; [ "b"; "b1" ]; [ "c"; "c1" ] ] in
  let route_b =
    [
      [ "x"; "x1" ]; [ "a"; "a1" ]; [ "b"; "b1" ]; [ "c"; "c1" ]; [ "x"; "x1" ];
    ]
  in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf
    "Same routes (same hash) with different amount of points (noise points in \
     the beginning and end of route): %f"
    similarity;
  [%expect
    {| Same routes (same hash) with different amount of points (noise points in the beginning and end of route): 0.600000 |}];

  let route_a =
    [
      [ "x"; "x1" ]; [ "a"; "a1" ]; [ "b"; "b1" ]; [ "c"; "c1" ]; [ "x"; "x1" ];
    ]
  in
  let route_b = [ [ "a"; "a1" ]; [ "b"; "b1" ]; [ "c"; "c1" ] ] in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf
    "Same routes (same hash) with different amount of points (noise points in \
     the beginning and end of route) - reversed: %f"
    similarity;
  [%expect
    {| Same routes (same hash) with different amount of points (noise points in the beginning and end of route) - reversed: 0.600000 |}];

  let route_a = [ [ "a"; "a1" ]; [ "b"; "b1" ]; [ "c"; "c1" ] ] in
  let route_b = [ [ "c"; "c1" ]; [ "b"; "b1" ]; [ "a"; "a1" ] ] in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf "Same routes (same hash) with same amount of points - reversed: %f"
    similarity;
  [%expect
    {| Same routes (same hash) with same amount of points - reversed: 0.333333 |}];

  let route_a = [ [ "a"; "a1" ]; [ "a"; "a1" ]; [ "a"; "a1" ] ] in
  let route_b = [ [ "a"; "a1" ] ] in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf
    "Same routes (same hash) with different amount of points - repeated: %f"
    similarity;
  [%expect
    {| Same routes (same hash) with different amount of points - repeated: 1.000000 |}];

  let route_a = [ [ "a"; "a1" ] ] in
  let route_b = [ [ "a"; "a1" ]; [ "a"; "a1" ]; [ "a"; "a1" ] ] in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf
    "Same routes (same hash) with different amount of points - repeated  \
     (a<>b): %f"
    similarity;
  [%expect
    {| Same routes (same hash) with different amount of points - repeated  (a<>b): 1.000000 |}];

  let route_a =
    [ [ "a"; "a1" ]; [ "b"; "b1" ]; [ "a"; "a1" ]; [ "b"; "b1" ] ]
  in
  let route_b =
    [ [ "a"; "a1" ]; [ "a"; "a1" ]; [ "a"; "a1" ]; [ "a"; "a1" ] ]
  in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf "Alternating mismatch: %f" similarity;
  [%expect {| Alternating mismatch: 0.500000 |}];

  let route_a = [ [ "a"; "a1" ]; [ "b"; "b1" ] ] in
  let route_b = [ [ "a"; "a1" ]; [ "x"; "x1" ]; [ "b"; "b1" ] ] in
  let similarity = RouteHash.calculate_similarity ~route_a ~route_b in
  printf "Same route with extra point in the middle: %f" similarity;
  [%expect {| Same route with extra point in the middle: 0.666667 |}]

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
