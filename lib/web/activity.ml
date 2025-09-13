open Core
open Dream_html
open HTML
module Time_ns = Time_ns_unix

let activity_start_time (timestamp : string) =
  let time = Time_ns.of_string timestamp in
  let time = Time_ns.to_sec_string ~zone:Timezone.utc time in
  sprintf "%s" time

(* TODO: this is very very hacky. I can attach a script section instead and
   load a js file but how to pass attributes to the js?? *)
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

let activity_map (locations : float list list option) =
  match locations with
  | None -> null []
  | Some locs ->
      let map_script = activity_map_script locs in
      null [ div [ id "map" ] []; script [] "%s" map_script ]

(* TODO: in cases of crosstrain or elliptical this card stays same size
   as stats card and it makes each div from it to have a lot of spacing
   around *)
let activity_details_card (activity : Models.Activity.t) =
  let icon_background, img_src = Helpers.activity_icon_and_color activity in

  (* this is list of lat lng points (to draw a map) if they exist for the activity *)
  let locations =
    List.find_map
      ~f:(fun stream ->
        match stream with LatLngStream s -> Some s.data | _ -> None)
      activity.streams
  in
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
      activity_map locations;
    ]

let activity_stats (athlete : Models.Strava_models.StravaAthlete.t option)
    (stats : Models.Stats.t) =
  let duration = Some (Helpers.duration_stat_node stats.moving_time) in
  let calories =
    match stats.average_heartrate with
    | None -> None
    | Some avg ->
        let calories =
          match athlete with
          | None -> None
          | Some athl ->
              Some
                (Helpers.calories_stat_node ~athlete:athl ~avg_hr:avg
                   ~duration:stats.moving_time ())
        in
        calories
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

  List.filter_opt [ duration; distance; elevation; calories ]

let activity_stats_table (activity : Models.Activity.t) =
  let hr_row =
    match (activity.stats.max_heartrate, activity.stats.average_heartrate) with
    | None, None -> None
    | Some hr_max, Some hr_avg ->
        Some
          (Helpers.Stat.row Helpers.hr_stat ~min_value:"-"
             ~max_value:(Helpers.hr_stat_value hr_max)
             ~avg_value:(Helpers.hr_stat_value hr_avg))
    | _, _ -> assert false
  in

  let speed_pace_row =
    match (activity.stats.max_speed, activity.stats.average_speed) with
    | None, None -> None
    | Some speed_max, Some speed_avg ->
        let node =
          match activity.sport_type with
          | Models.Strava_models.Run | Models.Strava_models.TrailRun
          | Models.Strava_models.VirtualRun ->
              Helpers.Stat.row Helpers.pace_stat ~min_value:"-"
                ~max_value:(Helpers.pace_stat_value speed_max)
                ~avg_value:(Helpers.pace_stat_value speed_avg)
          | _ ->
              Helpers.Stat.row Helpers.speed_stat ~min_value:"-"
                ~max_value:(Helpers.speed_stat_value speed_max)
                ~avg_value:(Helpers.speed_stat_value speed_avg)
        in
        Some node
    | _, _ -> assert false
  in

  let power_row =
    match (activity.stats.max_power, activity.stats.average_power) with
    | None, None -> None
    | Some pwr_max, Some pwr_avg ->
        Some
          (Helpers.Stat.row Helpers.power_stat ~min_value:"-"
             ~max_value:(Helpers.power_stat_value pwr_max)
             ~avg_value:(Helpers.power_stat_value pwr_avg))
    | _, _ -> assert false
  in

  let cadence_row =
    match (activity.stats.max_cadence, activity.stats.average_cadence) with
    | None, None -> None
    | Some cad_max, Some cad_avg ->
        Some
          (Helpers.Stat.row Helpers.cadence_stat ~min_value:"-"
             ~max_value:(Helpers.cadence_stat_value cad_max)
             ~avg_value:(Helpers.cadence_stat_value cad_avg))
    | _, _ -> assert false
  in

  let elevation_row =
    match (activity.stats.elev_high, activity.stats.elev_low) with
    | None, None -> None
    | Some elev_max, Some elev_min ->
        Some
          (Helpers.Stat.row Helpers.elevation_stat
             ~min_value:(Helpers.elevation_stat_value elev_min)
             ~max_value:(Helpers.elevation_stat_value elev_max)
             ~avg_value:"-")
    | _, _ -> assert false
  in

  let nodes =
    List.filter_opt
      [ hr_row; speed_pace_row; power_row; cadence_row; elevation_row ]
  in
  (* speed/pace, hr, power, elev,  *)
  div
    [ class_ "statsTable" ]
    [
      table []
        [
          thead []
            [
              tr []
                [
                  th [] [ txt "" ];
                  th [] [ txt "Avg" ];
                  th [] [ txt "Max" ];
                  th [] [ txt "Min" ];
                ];
            ];
          tbody [] nodes;
        ];
    ]

let activity_stats_card (athlete : Models.Strava_models.StravaAthlete.t option)
    (activity : Models.Activity.t) =
  let nodes =
    activity_stats athlete activity.stats @ [ activity_stats_table activity ]
  in
  div [ class_ "card activityCardStats" ] nodes

let activity_graphs (activity : Models.Activity.t) =
  let _ = activity in
  div [] [ txt "Activity Graphs" ]

let activity_grid (athlete : Models.Strava_models.StravaAthlete.t option)
    (activity : Models.Activity.t option) =
  match activity with
  | None -> div [] [ txt "no such activity" ]
  | Some activity ->
      div
        [ class_ "activityGrid" ]
        [
          activity_details_card activity;
          activity_stats_card athlete activity;
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
      body [] [ Header.header_ athlete_name; activity_grid athlete activity ];
    ]
