open Core
open Dream_html
open HTML

let week_table_header (monday_date : Date.t) =
  let today = Time_ns.now () |> Time_ns.to_date ~zone:Timezone.utc in
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
        div
          [ class_ "dayOfTheWeek card %s" today_class ]
          [
            span
              [ class_ "dayOfTheWeekShort" ]
              [ txt "%s" (Str.first_chars name 1) ];
            span
              [ class_ "dayOfTheWeekMedium" ]
              [ txt "%s" (Str.first_chars name 3) ];
            span [ class_ "dayOfTheWeekLong" ] [ txt "%s" name ];
          ])
      days_of_the_week
  in
  div [ class_ "days" ] divs

let activity_stats ~sport_type
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (stats : Models.Stats.t) =
  let duration = Some (Helpers.duration_stat_node stats.moving_time) in
  let heartrate, calories =
    match stats.average_heartrate with
    | None -> (None, None)
    | Some avg ->
        let hr = Some (Helpers.hr_stat_node avg) in
        let calories =
          match athlete with
          | None -> None
          | Some athl ->
              Some
                (Helpers.calories_stat_node ~athlete:athl ~avg_hr:avg
                   ~duration:stats.moving_time ())
        in
        (hr, calories)
  in

  let distance, elevation =
    match stats.distance with
    | None -> (None, None)
    | Some distance ->
        let distance = Some (Helpers.distance_stat_node distance) in
        (* NOTE: elevation data can be available for all activities that are
           recorded with a watch with a barometer but we only want to show
           elevation gain/loss data in the case of activities with recorded
           distance (runs, bike rides etc.) and not for gym or other related
           activities *)
        let elevation =
          match (stats.elev_gain, stats.elev_loss) with
          | None, None -> None
          | Some gain, Some loss ->
              Some (Helpers.elev_gain_loss_stat_node gain loss)
          | _, _ -> assert false
        in
        (distance, elevation)
  in

  let speed_pace =
    match stats.average_speed with
    | None -> None
    | Some avg -> Some (Helpers.speed_pace_stat_node sport_type avg)
  in

  List.filter_opt
    [ duration; heartrate; distance; speed_pace; elevation; calories ]

let activity_stats_div (athlete : Models.Strava_models.StravaAthlete.t option)
    (activity : Models.Activity.t) =
  div
    [ class_ "activityCardStats" ]
    (activity_stats ~sport_type:activity.sport_type athlete activity.stats)

let activity_div (athlete : Models.Strava_models.StravaAthlete.t option)
    (activity : Models.Activity.t) =
  div
    [
      class_ "activity card";
      onclick "location.href='/activity/%d';" activity.id;
      style_ "cursor: pointer;";
    ]
    [ Helpers.activity_header activity; activity_stats_div athlete activity ]

let week_table_activities
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list list) =
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
  div
    [ class_ "navHeader" ]
    [
      div
        [ class_ "navButtons" ]
        [
          span []
            [
              a
                [ path_attr href Paths.training_log prev_monday_str ]
                [ txt "Prev" ];
            ];
          span []
            [
              a
                [ path_attr href Paths.training_log next_monday_str ]
                [ txt "Next" ];
            ];
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

let head_elems () =
  [
    meta [ http_equiv `content_type; content "text/html; charset=UTF-8" ];
    meta [ charset "UTF-8" ];
    meta [ name "viewport"; content "width=device-width, initial-scale=1.0" ];
    link
      [
        rel "icon";
        type_ "image/png";
        path_attr href Static.Assets.Images.favicon_small_png;
      ];
    title [] "Unto";
    (* NOTE: htmx version 2.0.7 *)
    script [ path_attr src Static.Assets.Js.Htmx.Dist.htmx_min_js ] "";
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href Static.Assets.Css.common_css;
      ];
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href Static.Assets.Css.header_css;
      ];
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href Static.Assets.Css.summary_css;
      ];
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href Static.Assets.Css.training_log_css;
      ];
  ]

let page (monday_date : Date.t)
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list list) =
  let athlete_name =
    match athlete with None -> "Unknown" | Some athl -> athl.firstname
  in
  html
    [ lang "en" ]
    [
      head [] (head_elems ());
      body []
        [
          Header.header_ ~selected:Header.TrainingLog ~athlete_name ();
          nav_buttons monday_date;
          week_table_header monday_date;
          week_table_activities athlete activities;
          Summary.component ~type_:Summary.Week athlete activities;
        ];
    ]
