open Core
module Time_ns = Time_ns_unix

module User = struct
  type t = {
    db : Db.t;
    created_at : Date.t;
    activity_cache : (int, Models.Activity.t) Hashtbl.t;
    athlete : Models.Strava_models.StravaAthlete.t option;
  }

  (* TODO: finish this *)
  let make ~db =
    let today = Time_ns.now () |> Time_ns.to_date ~zone:Timezone.utc in
    let athlete = Db.get_athlete db in
    {
      db;
      activity_cache = Hashtbl.create (module Int);
      created_at = today;
      athlete;
    }

  let add_activity t (activity : Models.Activity.t) (athlete_id : int) =
    Hashtbl.set t.activity_cache ~key:activity.id ~data:activity;
    Db.add_activity t.db activity athlete_id

  let get_activity t activity_id =
    match Hashtbl.find t.activity_cache activity_id with
    | None -> (
        let activity = Db.get_activity t.db ~activity_id in
        match activity with
        | None -> None
        | Some a ->
            Hashtbl.set t.activity_cache ~key:activity_id ~data:a;
            Some a)
    | Some a -> Some a
end

module State = struct
  type t = {
    (* TODO: invalidate cache if it becomes possible to edit activities *)
    (* TODO: this caching solution works when we only have 1 user - Figure out how to attach this to a user session *)
    (* TODO: cache the user as well cause now we keep asking the db for the athlete info *)
    (* TODO: in theory we don;t care about how big this map grows because
       fly.io will stop the container during inactivity. If this changes then
         add logic to reset hashtabl at certain length. *)
    app_users : Db.t;
    users : (string, User.t) Hashtbl.t;
  }

  let make app_users = { app_users; users = Hashtbl.create (module String) }
end

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

let first_of_month ~zone =
  let today = Time_ns.now () |> Time_ns.to_date ~zone in
  Date.add_days today (-Date.day today + 1)

let group_activities ~(num_days : int) ~(start_date : Date.t)
    (activities : Models.Activity.t list) =
  List.init num_days ~f:(fun offset ->
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
let handle_training_log ~(state : State.t) request =
  let monday =
    match Dream.query request "monday" with
    | None -> last_monday ~zone:Timezone.utc
    (* TODO: if this is not monday then get latest monday before the date *)
    (* TODO: this is also missing error handling if timestamp fails to be parsed *)
    | Some timestamp -> Utils.iso8601_to_date timestamp
  in
  let weeks_activities = Db.get_weeks_activities state.db ~start_date:monday in
  let grouped_activities =
    group_activities ~num_days:7 ~start_date:monday weeks_activities
  in
  let athlete = Db.get_athlete state.db in
  let page = Training_log.page monday athlete grouped_activities in
  Dream_html.respond page

let handle_activity ~(state : State.t) request =
  let activity_id = Dream.param request "id" |> Int.of_string_opt in
  let activity =
    Option.bind activity_id ~f:(fun activity_id ->
        State.get_activity state activity_id)
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

  let athlete = Db.get_athlete state.db in
  let page = Activity.activity_page ~athlete ~activity ~split_select in
  Dream_html.respond page

(* TODO: handle_activity_map and handle_activity_graph both make a db request to an activity that we already fetched. *)
(* add an activity cache to the server or eliminate those separate requests *)
let handle_activity_map ~(state : State.t) request =
  let activity_id = Dream.param request "id" |> Int.of_string_opt in
  let activity =
    Option.bind activity_id ~f:(fun activity_id ->
        State.get_activity state activity_id)
    |> Option.value_exn
  in

  let page = Activity.activity_map ~full_load:true ~activity () in
  Dream_html.respond page

let handle_activity_graph ~(state : State.t) request =
  let activity_id = Dream.param request "id" |> Int.of_string_opt in
  let activity =
    Option.bind activity_id ~f:(fun activity_id ->
        State.get_activity state activity_id)
    |> Option.value_exn
  in

  let page = Activity.activity_graphs_card ~full_load:true activity in
  Dream_html.respond page

let handle_activity_select ~(state : State.t) request =
  let activity_id = Dream.param request "id" |> Int.of_string_opt in
  let activity =
    Option.bind activity_id ~f:(fun activity_id ->
        State.get_activity state activity_id)
    |> Option.value_exn
  in

  let split_select =
    Dream.param request "select" |> Activity_splits.splitLapSelector_of_string
  in
  let page = Activity.activity_laps_splits_card ~activity ~split_select in
  Dream_html.respond page

let update_activities ~(state : State.t) ~(strava_auth : Strava.Auth.Auth.t) =
  let open Or_error.Let_syntax in
  let%bind _ = Strava.Auth.refresh_tokens strava_auth in

  let present_activities = Db.all_activities state.db in
  let%bind athlete =
    match Db.get_num_athletes state.db with
    | 0 ->
        let%bind athlete =
          Strava.Api.fetch_athlete ~token:strava_auth.tokens.access_token
        in
        Db.add_athlete_if_not_exist state.db athlete;
        Ok athlete
    | 1 -> Ok (Option.value_exn (Db.get_athlete state.db))
    | _ -> assert false
  in
  let%bind new_activities =
    Strava.Api.fetch_activities ~token:strava_auth.tokens.access_token
      ~num_activities:1 ~start_page:1 ~exclude:present_activities
  in
  ignore
    (List.map
       ~f:(fun activity ->
         Dream.log "adding activity to db %d" activity.id;
         State.add_activity state activity athlete.id)
       new_activities);
  Ok (List.length new_activities)

let handle_update ~state ~(strava_auth : Strava.Auth.Auth.t) request =
  let _ = request in
  let num_new_activities =
    match update_activities ~state ~strava_auth with
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

let handle_calendar ~(state : State.t) request =
  let first_of_month =
    match Dream.query request "month" with
    | None -> first_of_month ~zone:Timezone.utc
    (* TODO: if this is not first of the month then calculate it here *)
    (* TODO: this is also missing error handling if timestamp fails to be parsed *)
    | Some timestamp -> Utils.iso8601_to_date timestamp
  in
  let last_of_month = Date.add_days (Date.add_months first_of_month 1) (-1) in
  let weeks_activities =
    Db.get_months_activities state.db ~start_date:first_of_month
      ~end_date:last_of_month
  in
  let grouped_activities =
    group_activities
      ~num_days:
        (Date.days_in_month ~year:(Date.year first_of_month)
           ~month:(Date.month first_of_month))
      ~start_date:first_of_month weeks_activities
  in

  let athlete = Db.get_athlete state.db in
  let page = Calendar.page first_of_month athlete grouped_activities in
  Dream_html.respond page

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
  (* NOTE: rotate cookie secret about once per year, you can use the code bellow to generate it  *)
  (* let secret = Dream.to_base64url (Dream.random 32) in *)
  let secret = "Ut_HWsViPaH7usYjrG0qHjTH4aB2_jjL4QP-961GBW4" in
  let state = State.make ~db in
  Dream.run ~interface:"0.0.0.0" ~port:8080
  @@ Dream.logger @@ Dream.set_secret secret
  @@ Dream.cookie_sessions ~lifetime:86400.0 (* lifetime of 1 day *)
  @@ Dream.router
       [
         Dream_html.get Paths.index (require_login (handle_training_log ~state));
         Dream_html.get Paths.calendar (require_login (handle_calendar ~state));
         Dream_html.get Paths.login handle_login;
         Dream_html.post Paths.login handle_login_post;
         Dream_html.get Paths.logout handle_logout;
         Dream_html.get Paths.update
           (require_login (handle_update ~state ~strava_auth));
         Dream_html.get Paths.activity (require_login (handle_activity ~state));
         Dream_html.get Paths.activity_map
           (require_login (handle_activity_map ~state));
         Dream_html.get Paths.activity_graph
           (require_login (handle_activity_graph ~state));
         Dream_html.get Paths.activity_select
           (require_login (handle_activity_select ~state));
         Static.routes;
       ]
