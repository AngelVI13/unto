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

let add_activity { handle; _ } (activity : Activity.t) =
  let _ =
    DB.add_stats handle ~id:None ~activity_id:(Int64.of_int activity.id)
      ~data_points:(Int64.of_int activity.stats.data_points)
      ~moving_time:(Int64.of_int activity.stats.moving_time)
      ~elapsed_time:(Int64.of_int activity.stats.elapsed_time)
      ~distance:activity.stats.distance
      ~elev_gain:(to_int64_option activity.stats.elev_gain)
      ~elev_loss:(to_int64_option activity.stats.elev_loss)
      ~elev_high:(to_int64_option activity.stats.elev_high)
      ~elev_low:(to_int64_option activity.stats.elev_low)
      ~start_lat:(Stats.start_lat activity.stats)
      ~start_lng:(Stats.start_lng activity.stats)
      ~end_lat:(Stats.end_lat activity.stats)
      ~end_lng:(Stats.end_lng activity.stats)
      ~average_speed:activity.stats.average_speed
      ~max_speed:activity.stats.max_speed
      ~average_cadence:(to_int64_option activity.stats.average_cadence)
      ~max_cadence:(to_int64_option activity.stats.max_cadence)
      ~average_temp:(to_int64_option activity.stats.average_temp)
      ~average_heartrate:(to_int64_option activity.stats.average_heartrate)
      ~max_heartrate:(to_int64_option activity.stats.max_heartrate)
      ~average_power:(to_int64_option activity.stats.average_power)
      ~max_power:(to_int64_option activity.stats.max_power)
  in
  let stats_id = ref (Int64.of_int (-1)) in
  DB.last_stats_id handle ~activity_id:(Int64.of_int activity.id) (fun ~id ->
      stats_id := id);
  let _ = (handle, activity) in
  ()

let all_activities { handle; _ } =
  let activities = ref [] in
  let _ =
    DB.list_activities handle (fun ~id ->
        activities := Int64.to_int_exn id :: !activities)
  in
  !activities

let load filename =
  match Sys_unix.file_exists filename with
  | `Yes -> { handle = Sqlite3.db_open filename; filename }
  | _ -> create filename

let close db = Or_error.try_with (fun () -> Sqlite3.db_close db.handle)
