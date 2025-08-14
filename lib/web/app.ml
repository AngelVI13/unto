open Core

let handle_training_log ~db request =
  let _ = request in
  let athlete = Db.get_athlete db in
  athlete |> Page.page |> Dream_html.respond

let run (db : Db.t) =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (handle_training_log ~db);
         Dream.get "/static/**" (Dream.static "./lib/web/static");
       ]
