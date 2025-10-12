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

let calendar first_of_month =
  let _ = first_of_month in
  div
    [ class_ "mainContainer" ]
    [ div [ class_ "calendar" ] []; div [ class_ "summary" ] [] ]

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
          calendar first_of_month;
          (* week_table_header monday_date; *)
          (* week_table_activities athlete activities; *)
          (* week_summary athlete activities; *)
        ];
    ]
