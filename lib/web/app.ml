open Core

let handle_training_log ~db request =
  let _ = request in
  (* TODO: can;t figure out how to call one template function from another,
     maybe switch to dream's default templating engine?? *)
  let athlete = Db.get_athlete db in
  athlete |> Template.render |> Dream.html

let run (db : Db.t) =
  Dream.run @@ Dream.logger
  @@ Dream.router [ Dream.get "/" (handle_training_log ~db) ]
