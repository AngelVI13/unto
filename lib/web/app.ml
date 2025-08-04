open Core

let hello who = sprintf "Hello, %s" who

let run (db : Db.t) =
  let _ = db in
  Dream.run @@ Dream.logger
  @@ Dream.router [ Dream.get "/" (fun _ -> Dream.html (hello "world")) ]
