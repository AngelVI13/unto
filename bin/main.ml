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
