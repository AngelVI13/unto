open Core
module Time_ns = Time_ns_unix

let stored_pass = "$2y$16$Nzi5uZoqsxrwQ20kMg4Fneht2PFTKg6LThzDr5.iOJ1tT3XE/6Q8a"
let logged_in_field = "logged_in"
let logged_in_success = "true"

let last_monday ~zone =
  let today = Time_ns.now () |> Time_ns.to_date ~zone in
  let day_of_week = Date.day_of_week today in
  let days_since_monday =
    match day_of_week with
    | Day_of_week.Mon -> 0
    | other -> Day_of_week.(num_days ~from:Mon ~to_:other)
  in
  Date.add_days today (-days_since_monday)

let group_activities ~(start_date : Date.t)
    (activities : Models.Activity.t list) =
  List.init 7 ~f:(fun offset ->
      let day_date = Date.add_days start_date offset in
      let day_activities =
        List.filter
          ~f:(fun activity ->
            let activity_time = Time_ns.of_string activity.start_date in
            let activity_date =
              Time_ns.to_date ~zone:Timezone.utc activity_time
            in
            Date.equal day_date activity_date)
          activities
        |> List.sort ~compare:(fun act1 act2 ->
               String.compare act1.start_date act2.start_date)
      in
      day_activities)

(* TODO: All activities from beginning until 2016-09-19  are completely wrong -> DELTE THEM *)
let handle_training_log ~db request =
  let monday =
    match Dream.query request "monday" with
    | None -> last_monday ~zone:Timezone.utc
    (* TODO: if this is not monday then get latest monday before the date *)
    (* TODO: this is also missing error handling if timestamp fails to be parsed *)
    | Some timestamp -> Utils.iso8601_to_date timestamp
  in
  let weeks_activities = Db.get_weeks_activities db ~start_date:monday in
  let grouped_activities =
    group_activities ~start_date:monday weeks_activities
  in
  let athlete = Db.get_athlete db in
  let page = Training_log.page monday athlete grouped_activities in
  Dream_html.respond page

let handle_activity ~db request =
  let activity_id = Dream.param request "id" |> Int.of_string_opt in
  let activity =
    Option.bind activity_id ~f:(fun activity_id ->
        Db.get_activity db ~activity_id)
  in

  (* If activity exists and has laps -> show laps by default *)
  let split_select =
    match activity with
    | None -> Activity_splits.Splits
    | Some activity -> (
        match List.length activity.laps with
        | 0 | 1 -> Activity_splits.Splits
        | _ -> Activity_splits.Laps)
  in

  let athlete = Db.get_athlete db in
  let page = Activity.activity_page ~athlete ~activity ~split_select in
  Dream_html.respond page

let handle_activity_map ~db request =
  let activity_id = Dream.param request "id" |> Int.of_string_opt in
  let activity =
    Option.bind activity_id ~f:(fun activity_id ->
        Db.get_activity db ~activity_id)
    |> Option.value_exn
  in

  let page = Activity.activity_map ~full_load:true ~activity () in
  Dream_html.respond page

let handle_activity_graph ~db request =
  let activity_id = Dream.param request "id" |> Int.of_string_opt in
  let activity =
    Option.bind activity_id ~f:(fun activity_id ->
        Db.get_activity db ~activity_id)
    |> Option.value_exn
  in

  let page = Activity.activity_graphs_card ~full_load:true activity in
  Dream_html.respond page

let handle_activity_select ~db request =
  let activity_id = Dream.param request "id" |> Int.of_string_opt in
  let activity =
    Option.bind activity_id ~f:(fun activity_id ->
        Db.get_activity db ~activity_id)
    |> Option.value_exn
  in

  let split_select =
    Dream.param request "select" |> Activity_splits.splitLapSelector_of_string
  in
  let page = Activity.activity_laps_splits_card ~activity ~split_select in
  Dream_html.respond page

let update_activities ~(db : Db.t) ~(strava_auth : Strava.Auth.Auth.t) =
  let open Or_error.Let_syntax in
  let%bind _ = Strava.Auth.refresh_tokens strava_auth in

  let present_activities = Db.all_activities db in
  let%bind athlete =
    match Db.get_num_athletes db with
    | 0 ->
        let%bind athlete =
          Strava.Api.fetch_athlete ~token:strava_auth.tokens.access_token
        in
        Db.add_athlete_if_not_exist db athlete;
        Ok athlete
    | 1 -> Ok (Option.value_exn (Db.get_athlete db))
    | _ -> assert false
  in
  let%bind new_activities =
    Strava.Api.fetch_activities ~token:strava_auth.tokens.access_token
      ~num_activities:100 ~start_page:1 ~exclude:present_activities
  in
  ignore
    (List.map
       ~f:(fun activity ->
         Dream.log "adding activity to db %d" activity.id;
         Db.add_activity db activity athlete.id)
       new_activities);
  Ok (List.length new_activities)

let handle_update ~db ~(strava_auth : Strava.Auth.Auth.t) request =
  let _ = request in
  let num_new_activities =
    match update_activities ~db ~strava_auth with
    | Error _ -> -1
    | Ok num_activities -> num_activities
  in

  let page =
    Header.update_icon ~updated_items_num:(Some num_new_activities) ()
  in
  Dream_html.respond page

let handle_login request =
  let csrf_token = Dream.csrf_token request in
  let page = Login.page csrf_token in
  Dream_html.respond page

let handle_login_post request =
  let open Lwt.Syntax in
  let* form = Dream.form request in
  match form with
  | `Ok [ ("password", pass) ]
    when Bcrypt.verify pass (Bcrypt.hash_of_string stored_pass) ->
      let* () =
        Dream.set_session_field request logged_in_field logged_in_success
      in
      Dream.redirect request (Helpers.string_of_path Paths.index)
  | _ -> Dream.html "Invalid password"

let require_login handler request =
  match Dream.session_field request logged_in_field with
  | Some value when String.equal value logged_in_success -> handler request
  | _ -> Dream.redirect request (Helpers.string_of_path Paths.login)

let handle_logout request =
  let open Lwt.Syntax in
  let* () = Dream.invalidate_session request in
  Dream.redirect request (Helpers.string_of_path Paths.login)

(* TODO: activity fails to download streams 113217900 *)
(* processing activity=113217900 2014-02-12T16:00:00Z *)
(*   downloading streams *)
(*   downloading laps *)
(*   error while downloading/parsing streams: ("Yojson__Safe.Util.Type_error(\"Expected array, got object\", _)") *)
(* processing activity=103428817 2014-01-02T00:09:30Z *)
(*   downloading streams *)
(*   downloading laps *)
(*   error while downloading/parsing streams: ("Yojson__Safe.Util.Type_error(\"Expected array, got object\", _)") *)
let run ~(db : Db.t) ~(strava_auth : Strava.Auth.Auth.t) =
  (* NOTE: For production, be sure to obtain a real certificate, for example, from
     Let's Encrypt. Pass the certificate to Dream.run with ~certificate_file and
     ~key_file. *)
  Dream.run ~interface:"0.0.0.0" ~port:8080
  @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream_html.get Paths.index (require_login (handle_training_log ~db));
         Dream_html.get Paths.login handle_login;
         Dream_html.post Paths.login handle_login_post;
         Dream_html.get Paths.logout handle_logout;
         Dream_html.get Paths.update
           (require_login (handle_update ~db ~strava_auth));
         Dream_html.get Paths.activity (require_login (handle_activity ~db));
         Dream_html.get Paths.activity_map
           (require_login (handle_activity_map ~db));
         Dream_html.get Paths.activity_graph
           (require_login (handle_activity_graph ~db));
         Dream_html.get Paths.activity_select
           (require_login (handle_activity_select ~db));
         Static.routes;
       ]

let%expect_test "fetch weeks activities from db" =
  let db = Db.load "/home/angel/Documents/ocaml/unto/app.db" in

  let monday = last_monday ~zone:Timezone.utc in
  let weeks_activities = Db.get_weeks_activities db ~start_date:monday in

  ignore (Db.close db);

  List.iter
    ~f:(fun activity ->
      printf "%s %s %s\n" activity.name
        (Models.Strava_models.show_sportType activity.sport_type)
        activity.start_date)
    weeks_activities;
  [%expect
    {|
    Opening existing db /home/angel/Documents/ocaml/unto/app.db
    Lunch Run Run 2025-08-16T12:21:44Z
    Lunch Crossfit Crossfit 2025-08-15T12:30:43Z
    Lunch Ride Ride 2025-08-15T11:08:34Z
    Afternoon Run Run 2025-08-14T16:40:25Z
    Morning Run Run 2025-08-13T07:29:04Z
    Afternoon Run Run 2025-08-12T17:36:14Z
    Lunch Ride Ride 2025-08-11T12:09:21Z |}];

  let grouped = group_activities ~start_date:monday weeks_activities in

  List.iter
    ~f:(fun day_activities ->
      printf "%d: " (List.length day_activities);
      List.iter
        ~f:(fun activity ->
          printf "%s %s %s |" activity.name
            (Models.Strava_models.show_sportType activity.sport_type)
            activity.start_date)
        day_activities;
      printf "\n")
    grouped;
  [%expect
    {|
    1: Lunch Ride Ride 2025-08-11T12:09:21Z |
    1: Afternoon Run Run 2025-08-12T17:36:14Z |
    1: Morning Run Run 2025-08-13T07:29:04Z |
    1: Afternoon Run Run 2025-08-14T16:40:25Z |
    2: Lunch Crossfit Crossfit 2025-08-15T12:30:43Z |Lunch Ride Ride 2025-08-15T11:08:34Z |
    1: Lunch Run Run 2025-08-16T12:21:44Z |
    0: |}]
