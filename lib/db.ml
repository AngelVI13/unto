open Core
open Db_ops
module DB = DbOps (Sqlgg_sqlite3)

type t = { filename : string; handle : Sqlite3.db }

let explain msg db = printf "%s : %s\n" msg (Sqlite3.errmsg db)

let create filename =
  let handle = Sqlite3.db_open filename in
  let _ = DB.create_athletes handle in
  let _ = DB.create_activities handle in
  let _ = DB.create_stats handle in
  let _ = DB.create_laps handle in
  let _ = DB.create_splits handle in
  { filename; handle }

let add_test_split { handle; _ } =
  let _ =
    DB.add_split handle ~id:None ~activity_id:12345L ~start:0L ~len:15L
      ~split_index:7L ~stats_id:13L
  in
  ()

let add_test_activity { handle; _ } id =
  let _ =
    DB.add_activity handle ~id ~athlete_id:1L
      ~name:(sprintf "run %d" (Int64.to_int_exn id))
      ~sport_type:"Run" ~start_date:"date" ~timezone:"EEST" ~map_id:"asasdsad"
      ~map_summary_polyline:"asda1221" ~stats_id:14L
  in
  ()

let to_int64_option (i : int option) =
  match i with None -> None | Some value -> Some (Int64.of_int value)

let add_athlete_if_not_exist { handle; _ }
    (athlete : Strava_models.StravaAthlete.t) =
  let athlete_ids = ref [] in
  DB.list_athlete_ids handle (fun ~id -> athlete_ids := id :: !athlete_ids);

  if List.exists ~f:(Int64.equal (Int64.of_int athlete.id)) !athlete_ids then ()
  else
    ignore
      (DB.add_athlete handle ~id:(Int64.of_int athlete.id)
         ~firstname:athlete.firstname ~lastname:athlete.lastname
         ~city:athlete.city ~state:athlete.state ~country:athlete.country
         ~sex:athlete.sex ~created_at:athlete.created_at ~weight:athlete.weight)

let add_stats (t : t) (stats : Stats.t) (activity_id : int) =
  let _ =
    DB.add_stats t.handle ~id:None ~activity_id:(Int64.of_int activity_id)
      ~data_points:(Int64.of_int stats.data_points)
      ~moving_time:(Int64.of_int stats.moving_time)
      ~elapsed_time:(Int64.of_int stats.elapsed_time)
      ~distance:stats.distance
      ~elev_gain:(to_int64_option stats.elev_gain)
      ~elev_loss:(to_int64_option stats.elev_loss)
      ~elev_high:(to_int64_option stats.elev_high)
      ~elev_low:(to_int64_option stats.elev_low)
      ~start_lat:(Stats.start_lat stats) ~start_lng:(Stats.start_lng stats)
      ~end_lat:(Stats.end_lat stats) ~end_lng:(Stats.end_lng stats)
      ~average_speed:stats.average_speed ~max_speed:stats.max_speed
      ~average_cadence:(to_int64_option stats.average_cadence)
      ~max_cadence:(to_int64_option stats.max_cadence)
      ~average_temp:(to_int64_option stats.average_temp)
      ~average_heartrate:(to_int64_option stats.average_heartrate)
      ~max_heartrate:(to_int64_option stats.max_heartrate)
      ~average_power:(to_int64_option stats.average_power)
      ~max_power:(to_int64_option stats.max_power)
  in
  (* explain (sprintf "add_stats %d" activity_id) t.handle; *)
  let stats_id = ref (Int64.of_int (-1)) in
  DB.stats_id_for_activity t.handle ~activity_id:(Int64.of_int activity_id)
    (fun ~id -> stats_id := id);
  (* explain *)
  (*   (sprintf "get_stats_id %d -> %d" activity_id (Int64.to_int_exn !stats_id)) *)
  (*   t.handle; *)
  !stats_id

let add_activity (t : t) (activity : Activity.t) (athlete_id : int) =
  let stats_id = add_stats t activity.stats activity.id in
  let _ =
    DB.add_activity t.handle ~id:(Int64.of_int activity.id)
      ~athlete_id:(Int64.of_int athlete_id) ~name:activity.name
      ~sport_type:(Strava_models.show_sportType activity.sport_type)
      ~start_date:activity.start_date ~timezone:activity.timezone
      ~map_id:activity.map_id
      ~map_summary_polyline:activity.map_summary_polyline ~stats_id
  in
  (* explain (sprintf "add_activity %d" activity.id) t.handle; *)
  (* TODO: add storing of streams, laps and splits and their stats *)
  ()

let all_activities { handle; _ } =
  let activities = ref [] in
  let _ =
    DB.list_activities handle (fun ~id ->
        printf "Found activity %d in db\n" (Int64.to_int_exn id);
        activities := Int64.to_int_exn id :: !activities)
  in
  !activities

let load filename =
  match Sys_unix.file_exists filename with
  | `Yes ->
      printf "Opening existing db %s\n" filename;
      { handle = Sqlite3.db_open filename; filename }
  | _ ->
      printf "Creating new db file %s\n" filename;
      create filename

let close db = Or_error.try_with (fun () -> Sqlite3.db_close db.handle)
