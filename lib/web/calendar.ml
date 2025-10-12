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

let num_weeks first_of_month =
  let days_in_month =
    Date.days_in_month ~year:(Date.year first_of_month)
      ~month:(Date.month first_of_month)
  in
  let weekdays_before_first =
    Date.day_of_week first_of_month |> Day_of_week.to_int
  in
  (* TODO: finish this. (weekdays_before_first + days_in_month) / 7 and round
     up will give you weeks in month *)
  let _ = (weekdays_before_first, days_in_month) in
  0

let calendar first_of_month =
  let num_weeks = num_weeks first_of_month in
  Dream.log "Num weeks in %s: %d" (Date.to_string first_of_month) num_weeks;
  div [ class_ "calendar" ] []

let main_container first_of_month =
  div
    [ class_ "mainContainer" ]
    [ calendar first_of_month; div [ class_ "summary" ] [] ]

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
        path_attr href Static.Assets.Css.calendar_css;
      ];
  ]

let page (first_of_month : Date.t)
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list list) =
  let athlete_name =
    match athlete with None -> "Unknown" | Some athl -> athl.firstname
  in
  let _ = activities in
  html
    [ lang "en" ]
    [
      head [] (head_elems ());
      body []
        [
          Header.header_ ~selected:Header.Calendar ~athlete_name ();
          nav_buttons first_of_month;
          main_container first_of_month;
          (* week_table_header monday_date; *)
          (* week_table_activities athlete activities; *)
          (* week_summary athlete activities; *)
        ];
    ]
