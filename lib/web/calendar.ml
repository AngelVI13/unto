open Core
open Dream_html
open HTML

(* TODO: make this work with months not mondays *)
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

let page (athlete : Models.Strava_models.StravaAthlete.t option)
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
          (* nav_buttons monday_date; *)
          (* week_table_header monday_date; *)
          (* week_table_activities athlete activities; *)
          (* week_summary athlete activities; *)
        ];
    ]
