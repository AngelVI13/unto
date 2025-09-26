open Core
module Time_ns = Time_ns_unix

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

  let split_select =
    match Dream.query request "split_select" with
    | None -> Activity_splits.Splits
    | Some s -> Activity_splits.splitLapSelector_of_string s
  in

  let athlete = Db.get_athlete db in
  let page = Activity.activity_page ~athlete ~activity ~split_select in
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
let run (db : Db.t) =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream_html.Livereload.route;
         Dream_html.get Paths.index (handle_training_log ~db);
         Dream_html.get Paths.activity (handle_activity ~db);
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
