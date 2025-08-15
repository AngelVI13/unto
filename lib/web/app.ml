open Core

let handle_training_log ~db request =
  let _ = request in
  let athlete = Db.get_athlete db in
  athlete |> Page.page |> Dream_html.respond

let run (db : Db.t) =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         (* NOTE: this might work but i need a script to execute live reloading, maybe try air (go) *)
         Dream_html.Livereload.route;
         Dream.get "/" (handle_training_log ~db);
         Dream.get "/static/**" (Dream.static "./lib/web/static");
       ]
