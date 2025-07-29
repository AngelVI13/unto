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
