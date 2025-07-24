type t = { filename : string; db : Sqlite3.db }

let create filename =
  let db = Sqlite3.db_open filename in
  { filename; db }

let load filename =
  if Sys.file_exists filename then { db = Sqlite3.db_open filename; filename }
  else create filename
