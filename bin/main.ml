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
     fun () ->
       Or_error.ok_exn
         (Strava.Auth.obtain_access_token auth_client auth_code filename))

let command_pull_activities auth_client =
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
       let auth =
         Or_error.ok_exn
           (Strava.Auth.load_and_refresh_tokens auth_client auth_filename)
       in
       Strava.Api.pull_activities auth.tokens.access_token n)

let command_pull_streams auth_client =
  Command.basic ~summary:"Pull streams for a given activity and save to file"
    (let%map_open.Command id = flag "--id" (required int) ~doc:"activity id"
     and auth_filename =
       flag "-t"
         (optional_with_default "tokens.json" Filename_unix.arg_type)
         ~doc:"Filename where to read access and refresh tokens from"
     in
     fun () ->
       let auth =
         Or_error.ok_exn
           (Strava.Auth.load_and_refresh_tokens auth_client auth_filename)
       in
       ignore
         (Or_error.ok_exn
            (Strava.Api.pull_streams_aux auth.tokens.access_token id)))

let command_download auth_client =
  Command.basic ~summary:"Download activities"
    (let%map_open.Command auth_filename =
       flag "-t"
         (optional_with_default "tokens.json" Filename_unix.arg_type)
         ~doc:"Filename where to read access and refresh tokens from"
     and n =
       flag "-n"
         (optional_with_default 10 int)
         ~doc:"number of activities to test"
     in
     fun () ->
       let auth =
         Or_error.ok_exn
           (Strava.Auth.load_and_refresh_tokens auth_client auth_filename)
       in
       Or_error.ok_exn
         (Strava.Api.process_activities auth.tokens.access_token n))

let command_pull_laps auth_client =
  Command.basic ~summary:"Pull laps for a given activity and save to file"
    (let%map_open.Command id = flag "--id" (required int) ~doc:"activity id"
     and auth_filename =
       flag "-t"
         (optional_with_default "tokens.json" Filename_unix.arg_type)
         ~doc:"Filename where to read access and refresh tokens from"
     in
     fun () ->
       let auth =
         Or_error.ok_exn
           (Strava.Auth.load_and_refresh_tokens auth_client auth_filename)
       in
       Or_error.ok_exn (Strava.Api.pull_laps auth.tokens.access_token id))

let command_user_info auth_client =
  Command.basic ~summary:"Get user info"
    (let%map_open.Command auth_filename =
       flag "-t"
         (optional_with_default "tokens.json" Filename_unix.arg_type)
         ~doc:"Filename where to read access and refresh tokens from"
     in
     fun () ->
       let auth =
         Or_error.ok_exn
           (Strava.Auth.load_and_refresh_tokens auth_client auth_filename)
       in
       Or_error.ok_exn (Strava.Api.process_user auth.tokens.access_token))

let command_zones auth_client =
  Command.basic ~summary:"Get user zones info"
    (let%map_open.Command auth_filename =
       flag "-t"
         (optional_with_default "tokens.json" Filename_unix.arg_type)
         ~doc:"Filename where to read access and refresh tokens from"
     in
     fun () ->
       let auth =
         Or_error.ok_exn
           (Strava.Auth.load_and_refresh_tokens auth_client auth_filename)
       in
       Or_error.ok_exn (Strava.Api.process_zones auth.tokens.access_token))

let command_test auth_client =
  Command.basic ~summary:"Test things"
    (let%map_open.Command n =
       flag "-n"
         (optional_with_default 10 int)
         ~doc:"number of activities to pull"
     and auth_filename =
       flag "-t"
         (optional_with_default "tokens.json" Filename_unix.arg_type)
         ~doc:"Filename where to read access and refresh tokens from"
     and start_page =
       flag "-s"
         (optional_with_default 1 int)
         ~doc:
           "Activity page to start from. This can be used to skip the first N \
            pages of activities."
     in
     fun () ->
       let auth =
         Or_error.ok_exn
           (Strava.Auth.load_and_refresh_tokens auth_client auth_filename)
       in
       let filename =
         "/home/angel/Documents/ocaml/unto/route_activities.json"
       in
       (* Strava.Api.route_testing_download auth filename start_page n; *)
       let _ = (start_page, auth, n) in
       Strava.Api.route_testing_searching filename;
       ())

let command_turso_testing () =
  Command.basic ~summary:"Test turso traits"
    (let%map_open.Command _ =
       flag "-db-url" (optional_with_default "app.db" string) ~doc:"DB url"
     in
     fun () ->
       (* ignore (Or_error.ok_exn (Db.test11 "unto2"))) *)
       ignore (Or_error.ok_exn (Db.test8 ())))

let command_turso_create_users () =
  Command.basic ~summary:"Create main users DB in turso"
    (let%map_open.Command app_name =
       flag "-app-name" (required string) ~doc:"Application name"
     in
     fun () ->
       let _ = app_name in
       ignore (Db.test12 ()))

let command_update_db auth_client =
  Command.basic ~summary:"Update db with latest N activities"
    (let%map_open.Command auth_filename =
       flag "-t"
         (optional_with_default "tokens.json" Filename_unix.arg_type)
         ~doc:"Filename where to read access and refresh tokens from"
     and n =
       flag "-n"
         (optional_with_default 10 int)
         ~doc:"number of activities to fetch"
     and start_page =
       flag "-s"
         (optional_with_default 1 int)
         ~doc:
           "Activity page to start from. This can be used to skip the first N \
            pages of activities."
     in
     fun () ->
       let test_db_hostname = Sys.getenv_exn "TURSO_TEST_DB_HOSTNAME" in
       let test_db_token = Sys.getenv_exn "TURSO_TEST_DB_TOKEN" in
       let db = Db.make ~hostname:test_db_hostname ~token:test_db_token in
       let present_activities = Db.all_activities db in
       Fun.protect
         ~finally:(fun () ->
           printf "Closing the db\n";
           ignore @@ Db.close db)
         (fun () ->
           let auth =
             Or_error.ok_exn
               (Strava.Auth.load_and_refresh_tokens auth_client auth_filename)
           in
           let athlete =
             Or_error.ok_exn
               (Strava.Api.fetch_athlete ~token:auth.tokens.access_token)
           in
           (* TODO: this should check if athlete details are the same and if
             not it should update athlete details *)
           Db.add_athlete_if_not_exist db athlete;
           let new_activities =
             Or_error.ok_exn
               (Strava.Api.fetch_activities ~token:auth.tokens.access_token
                  ~num_activities:n ~start_page ~exclude:present_activities ())
           in
           ignore
             (List.map
                ~f:(fun activity ->
                  printf "adding activity to db %d\n" activity.id;
                  Db.add_activity db activity athlete.id)
                new_activities)))

let command_run_app auth_client =
  Command.basic ~summary:"Run the web app"
    (let%map_open.Command auth_filename =
       flag "-t"
         (optional_with_default "tokens.json" Filename_unix.arg_type)
         ~doc:"Filename where to read access and refresh tokens from"
     in
     fun () ->
       let auth_tokens =
         Or_error.ok_exn (Strava.Auth.AuthTokens.of_filename auth_filename)
       in
       let strava_auth =
         Strava.Auth.Auth.make ~client:auth_client ~tokens:auth_tokens
           ~filename:auth_filename
       in
       let users_db_hostname = Sys.getenv_exn "TURSO_USERS_HOSTNAME" in
       let users_db_token = Sys.getenv_exn "TURSO_USERS_TOKEN" in
       (* TODO: this should check if db exists *)
       let db = Db.make ~hostname:users_db_hostname ~token:users_db_token in
       Db.create_tables db;
       Db.log_db_conn db;
       Fun.protect
         ~finally:(fun () ->
           printf "Closing the db\n";
           ignore @@ Db.close db)
         (fun () -> Web.App.run ~db ~strava_auth))

let command auth_client =
  Command.group ~summary:"CLI utility to download data from strava"
    [
      ("obtain-access-token", command_obtain_access_token auth_client);
      ("pull-activities", command_pull_activities auth_client);
      ("pull-streams", command_pull_streams auth_client);
      ("pull-laps", command_pull_laps auth_client);
      ("download", command_download auth_client);
      ("user-info", command_user_info auth_client);
      ("zones", command_zones auth_client);
      ("test", command_test auth_client);
      ("turso", command_turso_testing ());
      ("turso-create", command_turso_create_users ());
      ("update", command_update_db auth_client);
      ("run-app", command_run_app auth_client);
    ]

let () =
  Dotenv.export () |> ignore;
  let client_id = Sys.getenv_exn "CLIENT_ID" in
  let client_secret = Sys.getenv_exn "CLIENT_SECRET" in
  let auth_client = Strava.Auth.AuthClient.make client_id client_secret in

  Command_unix.run (command auth_client)
