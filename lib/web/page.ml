open Core
module Time_ns = Time_ns_unix

let head_elems () =
  let open Dream_html in
  let open HTML in
  [
    Dream_html.Livereload.script;
    meta [ http_equiv `content_type; content "text/html; charset=UTF-8" ];
    meta [ charset "UTF-8" ];
    meta [ name "viewport"; content "width=device-width, initial-scale=1.0" ];
    title [] "Unto";
    link
      [ rel "stylesheet"; type_ "text/css"; href "/static/styles/styles.css" ];
  ]

let header_ (athlete_name : string) =
  let open Dream_html in
  let open HTML in
  header
    [ class_ "headerMargin" ]
    [
      div
        [ class_ "headerTabs" ]
        [
          h1 [ class_ "active" ] [ a [ href "/" ] [ txt "Training Log" ] ];
          h1 [ class_ "inactive" ] [ a [ href "" ] [ txt "Dashboards" ] ];
        ];
      div
        [ class_ "headerSettings" ]
        [
          span
            [ class_ "icon-container" ]
            [
              a
                [ href "" ]
                [
                  img [ class_ "header-img"; src "/static/assets/account.png" ];
                ];
            ];
          span [ class_ "icon-container athlete-txt" ] [ txt "%s" athlete_name ];
          span
            [ class_ "icon-container" ]
            [
              a
                [ href "" ]
                [
                  img [ class_ "header-img"; src "/static/assets/settings.png" ];
                ];
            ];
        ];
    ]

let week_table_header (monday_date : Date.t) =
  let today = Time_ns.now () |> Time_ns.to_date ~zone:Timezone.utc in
  let open Dream_html in
  let open HTML in
  let days_of_the_week =
    [
      "Monday";
      "Tuesday";
      "Wednesday";
      "Thursday";
      "Friday";
      "Saturday";
      "Sunday";
    ]
  in
  let divs =
    List.mapi
      ~f:(fun i name ->
        let date = Date.add_days monday_date i in
        let today_class = if Date.equal today date then "cardToday" else "" in
        div [ class_ "dayOfTheWeek card %s" today_class ] [ txt "%s" name ])
      days_of_the_week
  in
  div [ class_ "days" ] divs

let activity_header (activity : Models.Activity.t) =
  let open Dream_html in
  let open HTML in
  let icon_color, img_src =
    match activity.sport_type with
    | Run -> ("gold", "/static/assets/running.png")
    | Ride -> ("coral", "/static/assets/cycling.png")
    | Crossfit -> ("greenyellow", "/static/assets/crosstrain.png")
    | _ -> ("", "")
  in
  let icon_background = sprintf "background: %s;" icon_color in
  div
    [ class_ "activityHeader" ]
    [
      span
        [ class_ "icon-container"; style_ "%s" icon_background ]
        [ img [ class_ "icon-img"; src "%s" img_src ] ];
      span
        [ class_ "activityType" ]
        [ txt "%s" (Models.Strava_models.show_sportType activity.sport_type) ];
    ]

let activity_stat ~stat_name ~stat_description ~stat_icon_path ~stat_value =
  let open Dream_html in
  let open HTML in
  div
    [ class_ "activityCardStat" ]
    [
      span
        [ class_ "activityCardStatName statNameText" ]
        [ txt "%s: " stat_name ];
      span
        [ class_ "activityCardStatName statNameIcon" ]
        [
          img
            [
              class_ "stat-icon-img";
              title_ stat_description;
              src stat_icon_path;
            ];
        ];
      span [ class_ "activityCardStatValue" ] [ txt "%s" stat_value ];
    ]

let format_activity_duration duration_secs =
  let secs = duration_secs mod 60 in
  let mins = duration_secs / 60 in
  let mins_left = mins mod 60 in
  let hours = mins / 60 in
  sprintf "%d:%02d:%02d" hours mins_left secs

let duration_stat moving_time = format_activity_duration moving_time
let avg_hr_stat avg = Int.to_string avg

let distance_stat distance =
  let distance =
    Float.round_significant ~significant_digits:3 (distance /. 1000.0)
  in
  sprintf "%.2f" distance

let pace_stat avg =
  let secs_per_km = Int.of_float (Float.round_down (1000.0 /. avg)) in
  let secs = secs_per_km mod 60 in
  let mins = secs_per_km / 60 in
  sprintf "%02d:%02d" mins secs

let speed_stat avg =
  sprintf "%.1f" Float.(round_significant ~significant_digits:3 (avg * 3.6))

let calories_stat (athlete : Models.Strava_models.StravaAthlete.t)
    (avg_hr : int) (duration : int) =
  let mins = Float.of_int duration /. 60.0 in

  (* TODO: get these 2 values from user. Multiplier is used to scale down
     calories so they better match with suunto numbers. You should start with
     multiplier 1 and then take the suunto calories and divide them by the
     calories from Unto and then this is your multiplier. For example 320
     (suunto) / 400 (unto) -> 0.80 multiplier *)
  let age = Float.of_int 31 in
  let multiplier = 0.80 in

  let calories =
    match athlete.sex with
    | "M" ->
        mins
        *. ((0.6309 *. Float.of_int avg_hr)
           +. (0.1988 *. athlete.weight) +. (0.2017 *. age) -. 55.0969)
        /. 4.184
    | "F" ->
        mins
        *. ((0.4472 *. Float.of_int avg_hr)
           +. (0.1263 *. athlete.weight) +. (0.0074 *. age) -. 20.4022)
        /. 4.184
    | _ -> assert false
  in
  Int.to_string (Float.to_int (Float.round_nearest (calories *. multiplier)))

let activity_stats ~sport_type
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (stats : Models.Stats.t) =
  let duration =
    Some
      (activity_stat ~stat_name:"Duration"
         ~stat_description:"Duration (hh:mm:ss)"
         ~stat_icon_path:"/static/assets/duration.png"
         ~stat_value:(duration_stat stats.moving_time))
  in
  let heartrate, calories =
    match stats.average_heartrate with
    | None -> (None, None)
    | Some avg ->
        let hr =
          Some
            (activity_stat ~stat_name:"Heartrate"
               ~stat_description:"Avg Heartrate (bpm)"
               ~stat_icon_path:"/static/assets/heartrate.png"
               ~stat_value:(avg_hr_stat avg))
        in
        let calories =
          match athlete with
          | None -> None
          | Some athl ->
              Some
                (activity_stat ~stat_name:"Calories"
                   ~stat_description:"Calories"
                   ~stat_icon_path:"/static/assets/calories.png"
                   ~stat_value:(calories_stat athl avg stats.moving_time))
        in
        (hr, calories)
  in

  let distance =
    match stats.distance with
    | None -> None
    | Some distance ->
        Some
          (activity_stat ~stat_name:"Distance" ~stat_description:"Distance (km)"
             ~stat_icon_path:"/static/assets/distance.png"
             ~stat_value:(distance_stat distance))
  in

  let speed_pace =
    match stats.average_speed with
    | None -> None
    | Some avg -> (
        match sport_type with
        | Models.Strava_models.Run | Models.Strava_models.TrailRun
        | Models.Strava_models.VirtualRun ->
            Some
              (activity_stat ~stat_name:"Pace" ~stat_description:"Pace (min/km)"
                 ~stat_icon_path:"/static/assets/pace.png"
                 ~stat_value:(pace_stat avg))
        | _ ->
            Some
              (activity_stat ~stat_name:"Speed" ~stat_description:"Speed (kph)"
                 ~stat_icon_path:"/static/assets/speed.png"
                 ~stat_value:(speed_stat avg)))
  in
  List.filter_opt [ duration; heartrate; distance; speed_pace; calories ]

let activity_stats_div (athlete : Models.Strava_models.StravaAthlete.t option)
    (activity : Models.Activity.t) =
  let open Dream_html in
  let open HTML in
  div
    [ class_ "activityCardStats" ]
    (activity_stats ~sport_type:activity.sport_type athlete activity.stats)

let activity_div (athlete : Models.Strava_models.StravaAthlete.t option)
    (activity : Models.Activity.t) =
  let open Dream_html in
  let open HTML in
  div
    [ class_ "activity card" ]
    [ activity_header activity; activity_stats_div athlete activity ]

let week_table_activities
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list list) =
  let open Dream_html in
  let open HTML in
  let days =
    List.mapi
      ~f:(fun i day_activities ->
        let activity_divs = List.map ~f:(activity_div athlete) day_activities in
        div
          [ class_ (if i mod 2 = 0 then "day dayWithColor" else "day") ]
          [ div [ class_ "activities" ] activity_divs ])
      activities
  in
  div [ class_ "days scrollable calendarHeight" ] days

let nav_buttons (monday_date : Date.t) =
  let next_monday = Date.add_days monday_date 7 in
  let next_monday_str = Utils.iso8601_of_date next_monday in
  let prev_monday = Date.add_days monday_date (-7) in
  let prev_monday_str = Utils.iso8601_of_date prev_monday in
  let open Dream_html in
  let open HTML in
  div
    [ class_ "navHeader" ]
    [
      div
        [ class_ "navButtons" ]
        [
          span [] [ a [ href "/?monday=%s" prev_monday_str ] [ txt "Prev" ] ];
          span [] [ a [ href "/?monday=%s" next_monday_str ] [ txt "Next" ] ];
        ];
      div
        [ class_ "navDates" ]
        [
          span []
            [
              txt "(%s - %s)"
                (Date.to_string monday_date)
                Date.(to_string (add_days monday_date 6));
            ];
        ];
    ]

let training_log (monday_date : Date.t)
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list list) =
  let open Dream_html in
  let open HTML in
  let athlete_name =
    match athlete with None -> "Unknown" | Some athl -> athl.firstname
  in
  html
    [ lang "en" ]
    [
      head [] (head_elems ());
      body []
        [
          header_ athlete_name;
          nav_buttons monday_date;
          week_table_header monday_date;
          week_table_activities athlete activities;
        ];
    ]
