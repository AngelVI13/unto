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

let activity_header (activity : Models.Activity.t) =
  let icon_background, img_src = Helpers.activity_icon_and_color activity in
  div
    [ class_ "activityHeader" ]
    [
      span
        [ class_ "icon-container"; style_ "%s" icon_background ]
        [ img [ class_ "icon-img"; path_attr src img_src ] ];
      span
        [ class_ "activityType" ]
        [ txt "%s" (Models.Strava_models.show_sportType activity.sport_type) ];
    ]

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
    [ activity_header activity; activity_stats_div athlete activity ]

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
          | Some athlete ->
              Helpers.calculate_calories ~athlete ~avg_hr ~duration ())
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
  let hd = List.hd_exn activities in
  let totals = Totals.of_activities athlete activities in

  let duration = Some (Helpers.duration_stat_node totals.duration) in
  let calories = Some (Helpers.calories_stat_from_total_node totals.calories) in
  let distance, elevation =
    match totals.distance with
    | None -> (None, None)
    | Some distance ->
        let distance = Some (Helpers.distance_stat_node distance) in
        let elevation =
          match (totals.elev_gain, totals.elev_loss) with
          | None, None -> None
          | Some gain, Some loss ->
              Some (Helpers.elev_gain_loss_stat_node gain loss)
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
  (* TODO: training load is just calories/10 -> its not but calories are part of it *)
  let totals = Totals.of_activities athlete activities in
  let duration = Helpers.duration_stat_value totals.duration in
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
          Header.header_ athlete_name;
          nav_buttons monday_date;
          week_table_header monday_date;
          week_table_activities athlete activities;
          week_summary athlete activities;
        ];
    ]
