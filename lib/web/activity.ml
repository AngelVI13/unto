open Core
open Dream_html
open HTML
module Time_ns = Time_ns_unix

let activity_start_time (timestamp : string) =
  let time = Time_ns.of_string timestamp in
  let time = Time_ns.to_sec_string ~zone:Timezone.utc time in
  sprintf "%s" time

let activity_map (locations : float list list option) =
  match locations with
  | None -> null []
  | Some locs ->
      let map_script = Activity_map.activity_map_script locs in
      null [ div [ id "map" ] []; script [] "%s" map_script ]

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
            [ img [ class_ "big-icon-img"; path_attr src img_src ] ];
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

let activity_stats_card (athlete : Models.Strava_models.StravaAthlete.t option)
    (activity : Models.Activity.t) =
  let nodes =
    [
      Activity_stats.activity_base_stats_table athlete activity.stats;
      Activity_stats.activity_stats_table activity;
    ]
  in
  div [ class_ "card activityCardStats" ] nodes

let activity_graphs (activity : Models.Activity.t) =
  Activity_graph.Graph.script_of_activity activity

let activity_graphs_card (activity : Models.Activity.t) =
  div
    [ class_ "card" ]
    [
      div
        [ class_ "graphContainer" ]
        [
          canvas [ id "streamsChart" ] [];
          script [] "%s" (activity_graphs activity);
        ];
    ]

(* TODO: make it look good on mobile *)
(* TODO: maybe split/lap switching should be done with htmx so we don't reload the whole page *)
(* TODO: similarly with htmx maybe we just load page without data and display the data as it appears *)
let activity_laps_splits_card ~(activity : Models.Activity.t)
    ~(split_select : Activity_splits.splitLapSelector) =
  let sport_type = activity.sport_type in
  let stats =
    match split_select with
    | Laps -> List.map ~f:(fun lap -> lap.stats) activity.laps
    | Splits -> List.map ~f:(fun split -> split.stats) activity.splits
  in
  let href_path = path_attr href Paths.activity_split_select activity.id in

  div
    [ class_ "card activitySplitsStats" ]
    [
      div
        [ class_ "splitSelectBtns" ]
        [
          span
            [
              (if Activity_splits.equal_splitLapSelector split_select Splits
               then class_ "active"
               else class_ "inactive");
            ]
            [
              a
                [ href_path (Activity_splits.show_splitLapSelector Splits) ]
                [ txt "Splits" ];
            ];
          span
            [
              (if Activity_splits.equal_splitLapSelector split_select Laps then
                 class_ "active"
               else class_ "inactive");
            ]
            [
              a
                [ href_path (Activity_splits.show_splitLapSelector Laps) ]
                [ txt "Laps" ];
            ];
        ];
      Activity_splits.activity_splits_table ~sport_type ~split_select stats;
    ]

let activity_grid ~(athlete : Models.Strava_models.StravaAthlete.t option)
    ~(activity : Models.Activity.t option)
    ~(split_select : Activity_splits.splitLapSelector) =
  match activity with
  | None -> div [] [ txt "no such activity" ]
  | Some activity ->
      div
        [ class_ "activityGrid" ]
        [
          activity_details_card activity;
          activity_stats_card athlete activity;
          activity_laps_splits_card ~activity ~split_select;
          activity_graphs_card activity;
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
        rel "icon";
        type_ "image/x-icon";
        path_attr href Static.Assets.Images.favicon_small_ico;
      ];
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href Static.Assets.Js.Leaflet.Dist.leaflet_css;
      ];
    (* NOTE: leaflet.js version 1.9.4 *)
    script [ path_attr src Static.Assets.Js.Leaflet.Dist.leaflet_js ] "";
    link
      [
        rel "stylesheet";
        type_ "text/css";
        path_attr href
          Static.Assets.Js.Leaflet_ResetView.Dist.l_Control_ResetView_min_css;
      ];
    (* NOTE: Leaflet.ResetView version 1.9.2 *)
    script
      [
        path_attr src
          Static.Assets.Js.Leaflet_ResetView.Dist.l_Control_ResetView_min_js;
      ]
      "";
    (* NOTE: chart.js version 4.5.0 *)
    script [ path_attr src Static.Assets.Js.Chart_js.Dist.chart_umd_min_js ] "";
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
        path_attr href Static.Assets.Css.activity_css;
      ];
  ]

let activity_page ~(athlete : Models.Strava_models.StravaAthlete.t option)
    ~(activity : Models.Activity.t option)
    ~(split_select : Activity_splits.splitLapSelector) =
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
          activity_grid ~athlete ~activity ~split_select;
        ];
    ]
