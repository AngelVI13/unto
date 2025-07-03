open Core

let command_obtain_access_token auth_client =
  Command.basic ~summary:"Obtains access token from a provided auth code"
    (let%map_open.Command auth_code =
       flag "--auth-code" (required string)
         ~doc:"Auth code obtained from authorizing the app in strava"
     and filename =
       flag "-o"
         (optional_with_default "new_tokens.json" Filename_unix.arg_type)
         ~doc:"Filename where to store the obtained tokens"
     in
     fun () -> Unto.Auth.obtain_access_token auth_client auth_code filename)

let command_pull_activities auth_client =
  let _ = auth_client in
  Command.basic ~summary:"Pull latest N activities from your strava"
    (let%map_open.Command n =
       flag "-n"
         (optional_with_default 10 int)
         ~doc:"number of activities to pull"
     and auth_filename =
       flag "-t"
         (optional_with_default "tokens.json" Filename_unix.arg_type)
         ~doc:"Filename where to read access and refresh tokens from"
     in
     fun () ->
       let token = Unto.Auth.access_token auth_client auth_filename in
       Unto.Strava.pull_activities token n)

let command auth_client =
  Command.group ~summary:"CLI utility to download data from strava"
    [
      ("obtain-access-token", command_obtain_access_token auth_client);
      ("pull-activities", command_pull_activities auth_client);
    ]

let () =
  Dotenv.export () |> ignore;
  let client_id = Sys.getenv_exn "CLIENT_ID" in
  let client_secret = Sys.getenv_exn "CLIENT_SECRET" in
  let auth_client = Unto.Auth.AuthClient.make client_id client_secret in

  Command_unix.run (command auth_client)

(* NOTE: these are most up to date info, use this *)
(* { *)
(*   "token_type": "Bearer", *)
(*   "access_token": "4dc11f841441620870a939556ccefae113462ce5", *)
(*   "expires_at": 1751459774, *)
(*   "expires_in": 21600, *)
(*   "refresh_token": "008d9cc19832a94865162dda3bd1341af856df72" *)
(* } *)

(* let auth_code = "975f18d50ca248e8dc3308648d5ff8a9c8296010" in *)
(* let out = Unto.Strava._obtain_access_token auth_code in *)
(* let token = "632921be33fae622c1939838b4dc15842dd56177" in *)
(* let refresh_token = "008d9cc19832a94865162dda3bd1341af856df72" in *)
(* let out = Unto.Strava._athlete_info token in *)

(* print_endline out; *)
(* let out = Unto.Strava._list_activities token in *)

(* print_endline out *)
(* let activity_id = 14960443945 in *)
(* let out = Unto.Strava.get_streams token activity_id in *)

(* print_endline out *)
(* let token = "5873bbbc433d6d8a886e96c69d3629358ff3afbf" in *)

(* let activity_id = 14883225124 in *)
(* let out = Unto.Strava.get_streams token activity_id in *)
(* let out = Unto.Strava.refresh_token refresh_token in *)
(* print_endline out *)
