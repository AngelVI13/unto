open Core
open Dream_html
open HTML
module Time_ns = Time_ns_unix

let activity_start_time (timestamp : string) =
  let time = Time_ns.of_string timestamp in
  let time = Time_ns.to_sec_string ~zone:Timezone.utc time in
  sprintf "%s" time

(* TODO: this is very very hacky. I can attach a script section instead and load a js file but how to pass attributes to the js?? *)
let activity_map_script (locations : float list list) : string =
  let latlng_str =
    List.fold ~init:""
      ~f:(fun acc latlng ->
        acc
        ^ sprintf "[%f, %f]," (List.nth_exn latlng 0) (List.nth_exn latlng 1))
      locations
  in
  let pretxt =
    {|
var map = L.map('map');
L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 19,
    attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
}).addTo(map);

// create a red polyline from an array of LatLng points
var latlngs = [
  |}
  in
  let posttext =
    {|
];

var polyline = L.polyline(latlngs, {color: 'red'}).addTo(map);

// zoom the map to the polyline
map.fitBounds(polyline.getBounds());
  |}
  in
  pretxt ^ latlng_str ^ posttext

let activity_details (activity : Models.Activity.t) =
  let icon_background, img_src = Helpers.activity_icon_and_color activity in

  let locations =
    [
      [ 54.755563; 25.37736 ];
      [ 54.755556; 25.377336 ];
      [ 54.75555; 25.377311 ];
      [ 54.755543; 25.377287 ];
      [ 54.755536; 25.377263 ];
      [ 54.755529; 25.377239 ];
      [ 54.755523; 25.377214 ];
      [ 54.755516; 25.37719 ];
      [ 54.755509; 25.377166 ];
      [ 54.755503; 25.37714 ];
      [ 54.755492; 25.377107 ];
      [ 54.755492; 25.377107 ];
      [ 54.755478; 25.377047 ];
      [ 54.755468; 25.377017 ];
      [ 54.75546; 25.376985 ];
      [ 54.755445; 25.376953 ];
    ]
  in
  let map_script = activity_map_script locations in
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
      div [ id "map" ] [ (* img [ class_ "mapImg"; src "%s" map_url ]  *) ];
      (* script [ src "%s" "/static/js/activity_map.js" ] ""; *)
      script [] "%s" map_script;
    ]

let activity_stats (activity : Models.Activity.t) =
  let _ = activity in
  div [] [ txt "Activity Stats" ]

let activity_graphs (activity : Models.Activity.t) =
  let _ = activity in
  div [] [ txt "Activity Graphs" ]

let activity_grid (activity : Models.Activity.t option) =
  match activity with
  | None -> div [] [ txt "no such activity" ]
  | Some activity ->
      div
        [ class_ "activityGrid" ]
        [
          activity_details activity;
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
      [
        rel "stylesheet";
        type_ "text/css";
        href "/static/leaflet/dist/leaflet.css";
      ];
    script [ src "%s" "/static/leaflet/dist/leaflet.js" ] "";
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
