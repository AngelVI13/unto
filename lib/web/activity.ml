open Dream_html
open HTML

(* TODO: this is the same as in training_log.ml -> so move to utils or sth *)
let activity_header (activity : Models.Activity.t) =
  let icon_background, img_src = Helpers.activity_icon_and_color activity in
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

let activity_details (activity : Models.Activity.t) =
  div [] [ txt "%s: %s" activity.name activity.start_date ]

let activity_grid (activity : Models.Activity.t option) =
  match activity with
  | None -> div [] [ txt "no such activity" ]
  | Some activity ->
      div
        [ class_ "activityGrid" ]
        [ activity_header activity; activity_details activity ]

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
      body [] [ Header.header_ athlete_name; activity_grid activity ];
    ]
