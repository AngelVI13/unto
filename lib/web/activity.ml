open Dream_html
open HTML

let activity_details (activity : Models.Activity.t option) =
  match activity with
  | None -> div [] [ txt "no such activity" ]
  | Some activity -> div [] [ txt "%s: %s" activity.name activity.start_date ]

let head_elems () =
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
      [ rel "stylesheet"; type_ "text/css"; href "/static/styles/activity.css" ];
  ]

let activity_page (athlete : Models.Strava_models.StravaAthlete.t option)
    (activity : Models.Activity.t option) =
  let athlete_name =
    match athlete with None -> "Unknown" | Some athl -> athl.firstname
  in
  html
    [ lang "en" ]
    [
      head [] (head_elems ());
      body [] [ Header.header_ athlete_name; activity_details activity ];
    ]
