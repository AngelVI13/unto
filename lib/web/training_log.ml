open Core

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

let activity_header (activity : Models.Activity.t) =
  let open Dream_html in
  let open HTML in
  let icon_color, img_src =
    match activity.sport_type with
    | Run -> ("gold", "/static/assets/running.png")
    | Ride -> ("coral", "/static/assets/cycling.png")
    | Crossfit -> ("greenyellow", "/static/assets/crosstrain.png")
    | _ -> ("gray", "/static/assets/unknown.png")
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

let duration_stat_value moving_time = format_activity_duration moving_time

let duration_stat duration =
  activity_stat ~stat_name:"Duration" ~stat_description:"Duration (hh:mm:ss)"
    ~stat_icon_path:"/static/assets/duration.png"
    ~stat_value:(duration_stat_value duration)

let avg_hr_stat_value avg = Int.to_string avg

let avg_hr_stat avg =
  activity_stat ~stat_name:"Heartrate" ~stat_description:"Avg Heartrate (bpm)"
    ~stat_icon_path:"/static/assets/heartrate.png"
    ~stat_value:(avg_hr_stat_value avg)

let distance_stat_value distance =
  let distance =
    Float.round_significant ~significant_digits:3 (distance /. 1000.0)
  in
  sprintf "%.2f" distance

let distance_stat distance =
  activity_stat ~stat_name:"Distance" ~stat_description:"Distance (km)"
    ~stat_icon_path:"/static/assets/distance.png"
    ~stat_value:(distance_stat_value distance)

let pace_stat_value avg =
  let secs_per_km = Int.of_float (Float.round_down (1000.0 /. avg)) in
  let secs = secs_per_km mod 60 in
  let mins = secs_per_km / 60 in
  sprintf "%02d:%02d" mins secs

let speed_stat_value avg =
  sprintf "%.1f" Float.(round_significant ~significant_digits:3 (avg * 3.6))

let speed_pace_stat activity_type avg =
  match activity_type with
  | Models.Strava_models.Run | Models.Strava_models.TrailRun
  | Models.Strava_models.VirtualRun ->
      activity_stat ~stat_name:"Pace" ~stat_description:"Pace (min/km)"
        ~stat_icon_path:"/static/assets/pace.png"
        ~stat_value:(pace_stat_value avg)
  | _ ->
      activity_stat ~stat_name:"Speed" ~stat_description:"Speed (kph)"
        ~stat_icon_path:"/static/assets/speed.png"
        ~stat_value:(speed_stat_value avg)

let calculate_calories ~(athlete : Models.Strava_models.StravaAthlete.t)
    ?(multiplier = 0.80) ~(avg_hr : int) ~(duration : int) () =
  let mins = Float.of_int duration /. 60.0 in

  (* TODO: get these 2 values from user. Multiplier is used to scale down
     calories so they better match with suunto numbers. You should start with
     multiplier 1 and then take the suunto calories and divide them by the
     calories from Unto and then this is your multiplier. For example 320
     (suunto) / 400 (unto) -> 0.80 multiplier *)
  (* TODO: those values should get stored for the activity ? Or better, we
     should store the user's birthyear so we can calculate his age for each
     activity so then we don't have to store the calories data to stats *)
  let age = Float.of_int 31 in

  (* NOTE: this formula was taken from here:
    https://www.omnicalculator.com/sports/calories-burned-by-heart-rate *)
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
  Float.to_int (Float.round_nearest (calories *. multiplier))

let calories_stat_value ~(athlete : Models.Strava_models.StravaAthlete.t)
    ?(multiplier = 0.80) ~(avg_hr : int) ~(duration : int) () =
  calculate_calories ~athlete ~multiplier ~avg_hr ~duration ()

let calories_stat_from_total calories =
  activity_stat ~stat_name:"Calories" ~stat_description:"Calories"
    ~stat_icon_path:"/static/assets/calories.png"
    ~stat_value:(Int.to_string calories)

let calories_stat ~(athlete : Models.Strava_models.StravaAthlete.t)
    ~(avg_hr : int) ~(duration : int) () =
  calories_stat_from_total (calories_stat_value ~athlete ~avg_hr ~duration ())

let elevation_stat elev_gain elev_loss =
  activity_stat ~stat_name:"Elevation"
    ~stat_description:"Elevation gain & loss (m)"
    ~stat_icon_path:"/static/assets/elevation.png"
    ~stat_value:(sprintf "+%d/-%d" elev_gain elev_loss)

let activity_stats ~sport_type
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (stats : Models.Stats.t) =
  let duration = Some (duration_stat stats.moving_time) in
  let heartrate, calories =
    match stats.average_heartrate with
    | None -> (None, None)
    | Some avg ->
        let hr = Some (avg_hr_stat avg) in
        let calories =
          match athlete with
          | None -> None
          | Some athl ->
              Some
                (calories_stat ~athlete:athl ~avg_hr:avg
                   ~duration:stats.moving_time ())
        in
        (hr, calories)
  in

  let distance, elevation =
    match stats.distance with
    | None -> (None, None)
    | Some distance ->
        let distance = Some (distance_stat distance) in
        (* NOTE: elevation data can be available for all activities that are
           recorded with a watch with a barometer but we only want to show
           elevation gain/loss data in the case of activities with recorded
           distance (runs, bike rides etc.) and not for gym or other related
           activities *)
        let elevation =
          match (stats.elev_gain, stats.elev_loss) with
          | None, None -> None
          | Some gain, Some loss -> Some (elevation_stat gain loss)
          | _, _ -> assert false
        in
        (distance, elevation)
  in

  let speed_pace =
    match stats.average_speed with
    | None -> None
    | Some avg -> Some (speed_pace_stat sport_type avg)
  in

  List.filter_opt
    [ duration; heartrate; distance; speed_pace; elevation; calories ]

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

let week_stat ~stat_label ~stat_value =
  let open Dream_html in
  let open HTML in
  div
    [ class_ "weekTotalStat" ]
    [
      span [ class_ "weekStatName" ] [ txt "%s" stat_label ];
      span [] [ txt "%s" stat_value ];
    ]

module Totals = struct
  type t = {
    duration : int;
    calories : int;
    distance : float option;
    elev_gain : int option;
    elev_loss : int option;
  }

  let empty =
    {
      duration = 0;
      calories = 0;
      distance = None;
      elev_gain = None;
      elev_loss = None;
    }

  let add_elev_data (elev_gain : int option) (elev_loss : int option) (t : t) =
    {
      t with
      elev_gain =
        (match t.elev_gain with
        | None -> elev_gain
        | Some gain -> (
            match elev_gain with
            | None -> Some gain
            | Some act_gain -> Some (gain + act_gain)));
      elev_loss =
        (match t.elev_loss with
        | None -> elev_loss
        | Some loss -> (
            match elev_loss with
            | None -> Some loss
            | Some act_loss -> Some (loss + act_loss)));
    }

  let add_distance (distance : float option) (t : t) =
    {
      t with
      distance =
        (match t.distance with
        | None -> distance
        | Some dist -> (
            match distance with
            | None -> Some dist
            | Some act_dist -> Some (dist +. act_dist)));
    }

  let add_activity (athlete : Models.Strava_models.StravaAthlete.t option)
      (acc : t) (activity : Models.Activity.t) =
    let duration = activity.stats.moving_time in

    let calories =
      match activity.stats.average_heartrate with
      | None -> 0
      | Some avg_hr -> (
          match athlete with
          | None -> 0
          | Some athlete -> calculate_calories ~athlete ~avg_hr ~duration ())
    in
    {
      acc with
      duration = acc.duration + duration;
      calories = acc.calories + calories;
    }
    |> add_elev_data activity.stats.elev_gain activity.stats.elev_loss
    |> add_distance activity.stats.distance

  let of_activities athlete activities =
    List.fold ~init:empty ~f:(add_activity athlete) activities
end

let week_activity_stat (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list) =
  let open Dream_html in
  let open HTML in
  let hd = List.hd_exn activities in
  let totals = Totals.of_activities athlete activities in

  let duration = Some (duration_stat totals.duration) in
  let calories = Some (calories_stat_from_total totals.calories) in
  let distance, elevation =
    match totals.distance with
    | None -> (None, None)
    | Some distance ->
        let distance = Some (distance_stat distance) in
        let elevation =
          match (totals.elev_gain, totals.elev_loss) with
          | None, None -> None
          | Some gain, Some loss -> Some (elevation_stat gain loss)
          | _, _ -> assert false
        in
        (distance, elevation)
  in
  let stats = [ duration; distance; elevation; calories ] in
  let stats = List.filter_opt stats in
  div
    [ class_ "summaryContainer" ]
    [ activity_header hd; div [] [ div [ class_ "weekTotals" ] stats ] ]

let week_total_summary (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list) =
  let open Dream_html in
  let open HTML in
  (* TODO: training load is just calories/10 -> its not but calories are part of it *)
  let totals = Totals.of_activities athlete activities in
  let duration = duration_stat_value totals.duration in
  div
    [ class_ "totalsSummaryContainer" ]
    [
      h2 [] [ txt "Week Stats" ];
      div []
        [
          div
            [ class_ "weekTotals" ]
            [
              (* TODO: add averages somehow ? *)
              week_stat ~stat_label:"Duration: " ~stat_value:duration;
              week_stat ~stat_label:"Calories: "
                ~stat_value:(Int.to_string totals.calories);
            ];
        ];
    ]

let week_summary (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list list) =
  let open Dream_html in
  let open HTML in
  let activities = List.concat activities in
  let activities_grouped =
    activities
    (* group by sport type *)
    |> List.sort_and_group ~compare:(fun act1 act2 ->
           Models.Strava_models.compare_sportType act1.sport_type
             act2.sport_type)
    (* sort by duration of group *)
    |> List.sort ~compare:(fun act_group1 act_group2 ->
           let totals1 = Totals.of_activities athlete act_group1 in
           let totals2 = Totals.of_activities athlete act_group2 in
           Int.compare totals1.duration totals2.duration)
    (* longest duration group first *)
    |> List.rev
  in
  let all_sumarries =
    [
      week_total_summary athlete activities;
      div [ class_ "verticalLine" ] [];
      div [ class_ "horizontalLine" ] [];
    ]
    @ List.map ~f:(week_activity_stat athlete) activities_grouped
  in
  div [ class_ "stats card statsGrid" ] all_sumarries

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
      [ rel "stylesheet"; type_ "text/css"; href "/static/styles/common.css" ];
    link
      [ rel "stylesheet"; type_ "text/css"; href "/static/styles/header.css" ];
    link
      [
        rel "stylesheet";
        type_ "text/css";
        href "/static/styles/training_log.css";
      ];
  ]

let page (monday_date : Date.t)
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
          Header.header_ athlete_name;
          nav_buttons monday_date;
          week_table_header monday_date;
          week_table_activities athlete activities;
          week_summary athlete activities;
        ];
    ]
