open Core
open Db_ops
module DB = DbOps (Sqlgg_sqlite3)

type t = { filename : string; handle : Sqlite3.db }

let explain msg db = printf "%s : %s\n" msg (Sqlite3.errmsg db)

let create filename =
  let handle = Sqlite3.db_open filename in
  let _ = DB.create_athletes handle in
  explain "create_athletes" handle;
  (* let z = DbOps *)
  { filename; handle }

let load filename =
  match Sys_unix.file_exists filename with
  | `Yes -> { handle = Sqlite3.db_open filename; filename }
  | _ -> create filename

let close db = Or_error.try_with (fun () -> Sqlite3.db_close db.handle)
