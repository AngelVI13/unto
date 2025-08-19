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

(* <div class="day dayWithColor"> *)
(*   <div class="activities"> *)
(*     <div class="activity card"> *)
(*       <div class="activityCardStats"> *)
(*         <div class="activityCardStat"> *)
(*           <span class="activityCardStatName statNameText">Time: </span> *)
(*           <span class="activityCardStatName statNameIcon"> *)
(*             <img *)
(*               class="stat-icon-img" *)
(*               title="Duration" *)
(*               src="./duration.png" *)
(*             /> *)
(*           </span> *)
(*           <span class="activityCardStatValue">35:12</span> *)
(*         </div> *)
(*         <div class="activityCardStat"> *)
(*           <span class="activityCardStatName statNameText" *)
(*             >Pace (/km): *)
(*           </span> *)
(*           <span class="activityCardStatName statNameIcon"> *)
(*             <img *)
(*               class="stat-icon-img" *)
(*               title="Pace (min/km)" *)
(*               src="./pace.png" *)
(*             /> *)
(*           </span> *)
(*           <span class="activityCardStatValue">6:45</span> *)
(*         </div> *)
(*         <div class="activityCardStat"> *)
(*           <span class="activityCardStatName statNameText" *)
(*             >Elev. (m): *)
(*           </span> *)
(*           <span class="activityCardStatName statNameIcon"> *)
(*             <img *)
(*               class="stat-icon-img" *)
(*               title="Elevation gain & loss (m)" *)
(*               src="./elevation.png" *)
(*             /> *)
(*           </span> *)
(*           <span class="activityCardStatValue">+120/-55</span> *)
(*         </div> *)
(*       </div> *)
(*     </div> *)
(*   </div> *)
(* </div> *)

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

(*         <div class="activityCardStat"> *)
(*           <span class="activityCardStatName statNameText">Time: </span> *)
(*           <span class="activityCardStatName statNameIcon"> *)
(*             <img *)
(*               class="stat-icon-img" *)
(*               title="Duration" *)
(*               src="./duration.png" *)
(*             /> *)
(*           </span> *)
(*           <span class="activityCardStatValue">35:12</span> *)
(*         </div> *)
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

let activity_stats ~sport_type (stats : Models.Stats.t) =
  let duration =
    Some
      (activity_stat ~stat_name:"Time" ~stat_description:"Duration"
         ~stat_icon_path:"/static/assets/duration.png"
         ~stat_value:(format_activity_duration stats.moving_time))
  in
  let heartrate =
    match stats.average_heartrate with
    | None -> None
    | Some avg ->
        Some
          (activity_stat ~stat_name:"Heartrate"
             ~stat_description:"Beats per min"
             ~stat_icon_path:"/static/assets/heartrate.png"
             ~stat_value:(Int.to_string avg))
  in

  let speed_pace =
    match stats.average_speed with
    | None -> None
    | Some avg -> (
        match sport_type with
        | Models.Strava_models.Run | Models.Strava_models.TrailRun
        | Models.Strava_models.VirtualRun ->
            let secs_per_km = Int.of_float (Float.round_down (1000.0 /. avg)) in
            let secs = secs_per_km mod 60 in
            let mins = secs_per_km / 60 in
            Some
              (activity_stat ~stat_name:"Pace" ~stat_description:"Mins per km"
                 ~stat_icon_path:"/static/assets/pace.png"
                 ~stat_value:(sprintf "%02d:%02d" mins secs))
        | _ ->
            Some
              (activity_stat ~stat_name:"Speed" ~stat_description:"km/hr"
                 ~stat_icon_path:"/static/assets/speed.png"
                 ~stat_value:
                   (sprintf "%.1f"
                      Float.(
                        round_significant ~significant_digits:3 (avg * 3.6)))))
  in
  List.filter_opt [ duration; heartrate; speed_pace ]

let activity_stats_div (activity : Models.Activity.t) =
  let open Dream_html in
  let open HTML in
  div
    [ class_ "activityCardStats" ]
    (activity_stats ~sport_type:activity.sport_type activity.stats)

let activity_div (activity : Models.Activity.t) =
  let open Dream_html in
  let open HTML in
  div
    [ class_ "activity card" ]
    [ activity_header activity; activity_stats_div activity ]

let week_table_activities (activities : Models.Activity.t list list) =
  let open Dream_html in
  let open HTML in
  let days =
    List.mapi
      ~f:(fun i day_activities ->
        let activity_divs = List.map ~f:activity_div day_activities in
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
  let athlete =
    match athlete with None -> "Unknown" | Some athl -> athl.firstname
  in
  html
    [ lang "en" ]
    [
      head [] (head_elems ());
      body []
        [
          header_ athlete;
          nav_buttons monday_date;
          week_table_header monday_date;
          week_table_activities activities;
        ];
    ]
