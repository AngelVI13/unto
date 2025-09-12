open Core
open Dream_html
open HTML
module Time_ns = Time_ns_unix

let activity_start_time (timestamp : string) =
  let time = Time_ns.of_string timestamp in
  let time = Time_ns.to_sec_string ~zone:Timezone.utc time in
  sprintf "%s" time

let activity_details ~gmaps_key (activity : Models.Activity.t) =
  let icon_background, img_src = Helpers.activity_icon_and_color activity in

  (* NOTE: the map is done with Google Maps Static API *)
  (* More info here: https://stackoverflow.com/a/53377017 *)
  (* and here: https://developers.google.com/maps/documentation/maps-static *)
  (* TODO: build this URL with URL builder *)
  let map_url =
    "https://maps.googleapis.com/maps/api/staticmap?size=400x400&path=weight:3%7Ccolor:orange%7C"
  in
  let map_url = map_url ^ sprintf "enc:%s" activity.map_summary_polyline in
  let map_url = map_url ^ "&key=" ^ gmaps_key in
  div
    [ class_ "card activityHeader" ]
    [
      div
        [ class_ "activityNameAndIcon" ]
        [
          div
            [ class_ "big-icon-container"; style_ "%s" icon_background ]
            [ img [ class_ "big-icon-img"; src "%s" img_src ] ];
          div
            [ class_ "activityType" ]
            [
              txt "%s" (Models.Strava_models.show_sportType activity.sport_type);
            ];
        ];
      div
        [ class_ "activityTime" ]
        [ txt "%s: %s" activity.name (activity_start_time activity.start_date) ];
      div [] [ img [ class_ "mapImg"; src "%s" map_url ] ];
    ]

let activity_stats (activity : Models.Activity.t) =
  let _ = activity in
  div [] [ txt "Activity Stats" ]

let activity_graphs (activity : Models.Activity.t) =
  let _ = activity in
  div [] [ txt "Activity Graphs" ]

let activity_grid ~gmaps_key (activity : Models.Activity.t option) =
  match activity with
  | None -> div [] [ txt "no such activity" ]
  | Some activity ->
      div
        [ class_ "activityGrid" ]
        [
          activity_details ~gmaps_key activity;
          activity_stats activity;
          activity_graphs activity;
        ]

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

let activity_page ~gmaps_key
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (activity : Models.Activity.t option) =
  let athlete_name =
    match athlete with None -> "Unknown" | Some athl -> athl.firstname
  in
  html
    [ lang "en" ]
    [
      head [] (head_elems ());
      body [] [ Header.header_ athlete_name; activity_grid ~gmaps_key activity ];
    ]
