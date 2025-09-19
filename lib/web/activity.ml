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

var routeCenter = map.getCenter();
var routeZoom = map.getZoom();

L.control.resetView({
    position: "topleft",
    title: "Reset view",
    latlng: routeCenter,
    zoom: routeZoom,
}).addTo(map);
  |}
  in
  pretxt ^ latlng_str ^ posttext

let activity_map (locations : float list list option) =
  match locations with
  | None -> null []
  | Some locs ->
      let map_script = activity_map_script locs in
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

let activity_base_stats_table
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (stats : Models.Stats.t) =
  let duration =
    Some
      (Helpers.Stat.value_row Helpers.duration_stat
         ~value:(Helpers.duration_stat_value stats.moving_time))
  in
  let calories =
    match stats.average_heartrate with
    | None -> None
    | Some avg ->
        let calories =
          match athlete with
          | None -> None
          | Some athl ->
              Some
                (Helpers.Stat.value_row Helpers.calories_stat
                   ~value:
                     (Int.to_string
                        (Helpers.calories_stat_value ~athlete:athl ~avg_hr:avg
                           ~duration:stats.moving_time ())))
        in
        calories
  in

  let distance, elevation =
    match stats.distance with
    | None -> (None, None)
    | Some distance ->
        let distance =
          Some
            (Helpers.Stat.value_row Helpers.distance_stat
               ~value:(Helpers.distance_stat_value distance))
        in
        (* NOTE: elevation data can be available for all activities that are
           recorded with a watch with a barometer but we only want to show
           elevation gain/loss data in the case of activities with recorded
           distance (runs, bike rides etc.) and not for gym or other related
           activities *)
        let elevation =
          match (stats.elev_gain, stats.elev_loss) with
          | None, None -> None
          | Some gain, Some loss ->
              Some
                (Helpers.Stat.value_row Helpers.elev_gain_loss_stat
                   ~value:(Helpers.elev_gain_loss_stat_value gain loss))
          | _, _ -> assert false
        in
        (distance, elevation)
  in

  let nodes = List.filter_opt [ duration; distance; elevation; calories ] in
  div
    [ class_ "baseStatsTable" ]
    [
      table []
        [
          thead [] [ tr [] [ th [] [ txt "" ]; th [] [ txt "Value" ] ] ];
          tbody [] nodes;
        ];
    ]

let activity_stats_table (activity : Models.Activity.t) =
  let hr_row =
    match (activity.stats.max_heartrate, activity.stats.average_heartrate) with
    | None, None -> None
    | Some hr_max, Some hr_avg ->
        Some
          (Helpers.Stat.min_max_avg_row Helpers.hr_stat ~min_value:"-"
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
              Helpers.Stat.min_max_avg_row Helpers.pace_stat ~min_value:"-"
                ~max_value:(Helpers.pace_stat_value speed_max)
                ~avg_value:(Helpers.pace_stat_value speed_avg)
          | _ ->
              Helpers.Stat.min_max_avg_row Helpers.speed_stat ~min_value:"-"
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
          (Helpers.Stat.min_max_avg_row Helpers.power_stat ~min_value:"-"
             ~max_value:(Helpers.power_stat_value pwr_max)
             ~avg_value:(Helpers.power_stat_value pwr_avg))
    | _, _ -> assert false
  in

  let cadence_row =
    match (activity.stats.max_cadence, activity.stats.average_cadence) with
    | None, None -> None
    | Some cad_max, Some cad_avg ->
        Some
          (Helpers.Stat.min_max_avg_row Helpers.cadence_stat ~min_value:"-"
             ~max_value:(Helpers.cadence_stat_value cad_max)
             ~avg_value:(Helpers.cadence_stat_value cad_avg))
    | _, _ -> assert false
  in

  let elevation_row =
    match (activity.stats.elev_high, activity.stats.elev_low) with
    | None, None -> None
    | Some elev_max, Some elev_min ->
        Some
          (Helpers.Stat.min_max_avg_row Helpers.elevation_stat
             ~min_value:(Helpers.elevation_stat_value elev_min)
             ~max_value:(Helpers.elevation_stat_value elev_max)
             ~avg_value:"-")
    | _, _ -> assert false
  in

  let nodes =
    List.filter_opt
      [ hr_row; speed_pace_row; power_row; cadence_row; elevation_row ]
  in
  div
    [ class_ "minMaxAvgStatsTable" ]
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
    [
      activity_base_stats_table athlete activity.stats;
      activity_stats_table activity;
    ]
  in
  div [ class_ "card activityCardStats" ] nodes

module GraphData = struct
  type t = {
    is_xaxis : bool;
    is_altitude : bool;
    label : string;
    data : string;
    data_labels : string option;
    color : string;
    display : bool;
    fill : bool;
    extra_dataset_fields : string list;
    extra_scale_fields : string list;
  }

  let empty () =
    {
      is_xaxis = false;
      is_altitude = false;
      label = "";
      data = "";
      data_labels = None;
      color = "";
      display = false;
      fill = false;
      extra_dataset_fields = [];
      extra_scale_fields = [];
    }

  let of_stream ~(sport_type : Models.Strava_models.sportType)
      (stream : Models.Streams.StreamType.t) =
    let _ = sport_type in
    match stream with
    | Models.Streams.StreamType.TimeStream s ->
        let data = List.init (List.length s.data) ~f:(fun v -> v) in
        let data_labels =
          List.fold ~init:""
            ~f:(fun acc v ->
              sprintf "%s'%s'," acc (Helpers.format_activity_duration v))
            data
        in
        let data =
          List.fold ~init:"" ~f:(fun acc v -> acc ^ sprintf "%d," v) data
        in
        let t = empty () in
        {
          t with
          is_xaxis = true;
          label = "Time";
          data;
          data_labels = Some data_labels;
          display = true;
          extra_scale_fields =
            [
              {| ticks: {
          // forces step size to be 50 units
          stepSize: 300
            },|};
            ];
        }
    | Models.Streams.StreamType.AltitudeStream s ->
        let smoothing_window = 5 in
        let data =
          Utils.moving_average (module Utils.FloatOps) smoothing_window s.data
        in
        let data =
          List.fold ~init:""
            ~f:(fun acc v ->
              let alt = Float.to_int (Float.round_down v) in
              acc ^ sprintf "%d," alt)
            data
        in
        let t = empty () in
        {
          t with
          is_altitude = true;
          label = "Altitude";
          data;
          color = "gray";
          display = true;
          fill = true;
        }
    | Models.Streams.StreamType.HeartRateStream s ->
        (* TODO: should i keep this smoothing, it makes the data a bit more readable *)
        let smoothing_window = 5 in
        let data =
          Utils.moving_average (module Utils.IntOps) smoothing_window s.data
        in
        let data =
          List.fold ~init:"" ~f:(fun acc v -> acc ^ sprintf "%d," v) data
        in
        let t = empty () in
        {
          t with
          label = "Heartrate";
          data;
          color = "#ff6384";
          display = true;
          extra_dataset_fields =
            [ "cubicInterpolationMode: 'monotone',"; "tension: 0.8," ];
          extra_scale_fields = [ sprintf "min: %d," 70; sprintf "max: %d," 200 ];
        }
    | Models.Streams.StreamType.VelocityStream s ->
        let smoothing_window = 15 in
        let data =
          Utils.moving_average (module Utils.FloatOps) smoothing_window s.data
        in
        let data =
          List.fold ~init:"" ~f:(fun acc v -> acc ^ sprintf "%f," v) data
        in
        let t = empty () in
        { t with label = "Velocity"; data; color = "#36a2eb"; display = false }
    | _ -> assert false

  let dataset (t : t) =
    sprintf
      {|
        {
          label: '%s',
          fill: %s,
          data: [%s],
          borderWidth: 1,
          borderColor: '%s',
          pointStyle: false,
          yAxisID: '%s',
          %s
        },
      |}
      t.label (Bool.to_string t.fill) t.data t.color t.label
      (String.concat ~sep:"\n" t.extra_dataset_fields)

  let scale ?(to_left = false) (t : t) =
    let position = if to_left then "left" else "right" in
    let grid_lines =
      if not to_left then
        {|
        // grid line settings
        grid: {
          drawOnChartArea: false, // only want the grid lines for one axis to show up
        },
    |}
      else ""
    in
    sprintf
      {|
      '%s': {
        type: 'linear',
        display: %s,
        position: '%s',
        %s
        %s
      },
      |}
      t.label (Bool.to_string t.display) position
      (String.concat ~sep:"\n" t.extra_scale_fields)
      grid_lines
end

module Graph = struct
  type t = {
    show_altitude : bool;
    x_axis : GraphData.t;
    lines : GraphData.t list;
  }

  let empty () =
    { show_altitude = false; x_axis = GraphData.empty (); lines = [] }

  let add_stream ~(sport_type : Models.Strava_models.sportType) (acc : t)
      (stream : Models.Streams.StreamType.t) =
    let line = GraphData.of_stream ~sport_type stream in
    match stream with
    | Models.Streams.StreamType.TimeStream _ -> { acc with x_axis = line }
    | Models.Streams.StreamType.VelocityStream _ ->
        { acc with show_altitude = true; lines = line :: acc.lines }
    | _ -> { acc with lines = line :: acc.lines }

  let of_streams ~(sport_type : Models.Strava_models.sportType)
      (streams : Models.Streams.StreamType.t list) =
    let streams =
      List.filter
        ~f:(fun stream ->
          match stream with
          (* TODO: support these as well *)
          (* | Models.Streams.StreamType.WattsStream  _  *)
          | Models.Streams.StreamType.VelocityStream _
          | Models.Streams.StreamType.TimeStream _
          | Models.Streams.StreamType.AltitudeStream _
          | Models.Streams.StreamType.HeartRateStream _ ->
              true
          | _ -> false)
        streams
    in
    let graph =
      List.fold ~init:(empty ()) ~f:(add_stream ~sport_type) streams
    in
    let lines =
      List.filter
        ~f:(fun line ->
          if graph.show_altitude then true
          else if (not graph.show_altitude) && line.is_altitude then false
          else true)
        graph.lines
    in
    (* TODO: in cases when we only have heart rate date we have to fix the
       y_axis to something reasonable otherwise it looks like an ECG line  *)
    (* TODO: have fixed values of x_axis i.e. every 5 mins or sth like that *)
    { graph with lines }

  let x_axis_labels (t : t) = Option.value_exn t.x_axis.data_labels

  let datasets (t : t) =
    t.lines |> List.map ~f:(fun line -> GraphData.dataset line) |> String.concat

  let scales (t : t) =
    t.lines
    |> List.mapi ~f:(fun i line -> GraphData.scale ~to_left:(i = 0) line)
    |> String.concat

  let script_of_activity (activity : Models.Activity.t) =
    let graph = of_streams ~sport_type:activity.sport_type activity.streams in
    sprintf
      {|
  const ctx = document.getElementById('streamsChart');

  new Chart(ctx, {
    type: 'line',
    data: {
      labels: [%s],
      datasets: [%s]
    },
    options: {
      responsive: true,
      plugins: {
        title: {
          display: true,
          text: 'Chart.js Line Chart'
        },
      },
      stacked: true,
      interaction: {
        mode: 'index',
        intersect: false
      },
      scales: { %s },
    }
  });
    |}
      (x_axis_labels graph) (datasets graph) (scales graph)
end

let activity_graphs (activity : Models.Activity.t) =
  Graph.script_of_activity activity

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

let activity_laps_splits_card (activity : Models.Activity.t) =
  let _ = activity in
  div [ class_ "card" ] [ txt "Activity laps and splits" ]

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
          activity_graphs_card activity;
          activity_laps_splits_card activity;
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
        href "/static/assets/favicon_32x32.ico";
      ];
    link
      [
        rel "stylesheet";
        type_ "text/css";
        href "/static/leaflet/dist/leaflet.css";
      ];
    script [ src "%s" "/static/leaflet/dist/leaflet.js" ] "";
    link
      [
        rel "stylesheet";
        type_ "text/css";
        href "/static/Leaflet.ResetView/dist/L.Control.ResetView.min.css";
      ];
    script
      [ src "%s" "/static/Leaflet.ResetView/dist/L.Control.ResetView.min.js" ]
      "";
    script [ src "%s" "/static/chart.js/dist/chart.umd.min.js" ] "";
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
