open Core
open Dream_html
open HTML

let nav_buttons (first_of_month : Date.t) =
  let next_month = Date.add_months first_of_month 1 in
  let next_month_str = Utils.iso8601_of_date next_month in
  let prev_month = Date.add_months first_of_month (-1) in
  let prev_month_str = Utils.iso8601_of_date prev_month in
  div
    [ class_ "navHeader" ]
    [
      div
        [ class_ "navButtons" ]
        [
          span []
            [
              a
                [ path_attr href Paths.calendar_url prev_month_str ]
                [ txt "Prev" ];
            ];
          span []
            [
              a
                [ path_attr href Paths.calendar_url next_month_str ]
                [ txt "Next" ];
            ];
        ];
      div
        [ class_ "navDates" ]
        [
          span []
            [
              txt "(%s - %s)"
                (Date.to_string first_of_month)
                Date.(to_string (add_days next_month (-1)));
            ];
        ];
    ]

(* NOTE: Day_of_week starts from Sunday (i.e. Sunday==0) => this remaps to sane
   defaults *)
let day_of_week_to_int = function
  | Day_of_week.Mon -> 0
  | Day_of_week.Tue -> 1
  | Day_of_week.Wed -> 2
  | Day_of_week.Thu -> 3
  | Day_of_week.Fri -> 4
  | Day_of_week.Sat -> 5
  | Day_of_week.Sun -> 6

let days_in_month (date : Date.t) =
  Date.days_in_month ~year:(Date.year date) ~month:(Date.month date)

let num_weeks first_of_month =
  let days_in_month = days_in_month first_of_month in
  let weekdays_before_first =
    day_of_week_to_int (Date.day_of_week first_of_month)
    - day_of_week_to_int Mon
  in

  let total_days = weekdays_before_first + days_in_month in
  Float.(to_int (round_up (of_int total_days / 7.0)))

let activity_header (activity : Models.Activity.t) =
  let icon_background, img_src = Helpers.activity_icon_and_color activity in
  (* TODO: currently these styles are not in calendar.css ->
    figure out what is needed and reuse it *)
  div
    [ class_ "activityHeader" ]
    [
      span
        [ class_ "icon-container"; style_ "%s" icon_background ]
        [ img [ class_ "icon-img"; path_attr src img_src ] ];
    ]

let activity_div (activity : Models.Activity.t) =
  (* TODO: currently these styles are not in calendar.css ->
    figure out what is needed and reuse it *)
  div
    [
      class_ "calendarActivity";
      onclick "location.href='/activity/%d';" activity.id;
      style_ "cursor: pointer;";
    ]
    [ activity_header activity ]

let calendar first_of_month (activities : Models.Activity.t list list) =
  let num_weeks = num_weeks first_of_month in
  let month_start_idx = day_of_week_to_int (Date.day_of_week first_of_month) in
  let month_days = days_in_month first_of_month in
  let days =
    List.init (num_weeks * 7) ~f:(fun i ->
        let day_idx = i - month_start_idx in
        let day_num = Date.add_days first_of_month day_idx in
        let day_activities =
          if day_idx < 0 || day_idx >= month_days then []
          else List.nth_exn activities day_idx
        in
        let activity_divs = List.map ~f:activity_div day_activities in
        (* TODO: add .calendarToday class to highlight today  *)
        let extra_css_class =
          if day_idx < 0 || day_idx >= month_days then "calendarInactive"
          else ""
        in
        div
          [ class_ "card calendarDay %s" extra_css_class ]
          [
            span [ class_ "calendarDayNum" ] [ txt "%d" (Date.day day_num) ];
            div [ class_ "calendarDayActivities" ] activity_divs;
          ])
  in
  let daysOfWeek =
    [
      ("Monday", "Mon", "M");
      ("Tuesday", "Tue", "T");
      ("Wednesday", "Wed", "W");
      ("Thursday", "Thu", "T");
      ("Friday", "Fri", "F");
      ("Saturday", "Sat", "S");
      ("Sunday", "Sun", "S");
    ]
    |> List.map ~f:(fun (long, short, xshort) ->
           div
             [ class_ "calendarDayOfWeek" ]
             [
               span [ class_ "calendarBigTxt" ] [ txt "%s" long ];
               span [ class_ "calendarMediumTxt" ] [ txt "%s" short ];
               span [ class_ "calendarSmallTxt" ] [ txt "%s" xshort ];
             ])
  in
  let days = daysOfWeek @ days in
  div [ class_ "calendar" ] days

let main_container first_of_month
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list list) =
  div
    [ class_ "mainContainer" ]
    [
      calendar first_of_month activities;
      Summary.component ~type_:Summary.Month athlete activities;
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
        path_attr href Static.Assets.Css.calendar_css;
      ];
  ]

let page (first_of_month : Date.t)
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
          Header.header_ ~selected:Header.Calendar ~athlete_name ();
          nav_buttons first_of_month;
          main_container first_of_month athlete activities;
          (* week_table_header monday_date; *)
          (* week_table_activities athlete activities; *)
          (* week_summary athlete activities; *)
        ];
    ]
