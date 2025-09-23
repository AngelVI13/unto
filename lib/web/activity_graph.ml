open Core

type graphType =
  | UnknownGraph
  | TimeGraph
  | HeartrateGraph
  | VelocityGraph
  | AltitudeGraph
  | DistanceGraph
  | PowerGraph
  | TemperatureGraph
[@@deriving show { with_path = false }, eq]

module GraphData = struct
  type t = {
    graph_type : graphType;
    label : string;
    data : string;
    data_len : int option;
    data_labels : string option;
    color : string;
    display : bool;
    fill : bool;
    (* The bigger the order value the further in the background the graph appears *)
    order : int;
    unit : string;
    extra_dataset_fields : string list;
    extra_scale_fields : string list;
  }

  let empty () =
    {
      graph_type = UnknownGraph;
      label = "";
      data = "";
      data_len = None;
      data_labels = None;
      color = "";
      display = false;
      fill = false;
      order = 0;
      unit = "-";
      extra_dataset_fields = [];
      extra_scale_fields = [];
    }

  let gray = "rgba(128, 128, 128)"
  let red = "rgba(255, 99, 132)"
  let blue = "rgba(54, 162, 235)"
  let yellow = "rgb(255, 205, 86)"
  let purple = "rgb(153, 102, 255)"

  let of_stream ~(activity : Models.Activity.t)
      (stream : Models.Streams.StreamType.t) =
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
          graph_type = TimeGraph;
          label = "Time";
          data;
          data_len = Some (List.length s.data);
          data_labels = Some data_labels;
          display = true;
        }
    | Models.Streams.StreamType.AltitudeStream s ->
        let smoothing_window = 5 in
        let data =
          Utils.moving_average (module Utils.FloatOps) smoothing_window s.data
        in
        let data =
          List.fold ~init:"" ~f:(fun acc v -> acc ^ sprintf "%f," v) data
        in
        let grace = 30 in
        let suggested_min = Option.value_exn activity.stats.elev_high - grace in
        let suggested_max = Option.value_exn activity.stats.elev_high + grace in

        let t = empty () in
        {
          t with
          graph_type = AltitudeGraph;
          label = "Altitude";
          data;
          color = gray;
          display = true;
          fill = true;
          order = 100;
          unit = Helpers.elevation_stat.unit;
          extra_scale_fields =
            [
              sprintf "suggestedMin: %d," suggested_min;
              sprintf "suggestedMax: %d," suggested_max;
            ];
        }
    | Models.Streams.StreamType.HeartRateStream s ->
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
          graph_type = HeartrateGraph;
          label = "Heartrate";
          data;
          color = red;
          display = true;
          fill = true;
          order = 1;
          unit = Helpers.hr_stat.unit;
          extra_dataset_fields = [];
          extra_scale_fields =
            [ sprintf "suggestedMin: %d," 90; sprintf "suggestedMax: %d," 190 ];
        }
    | Models.Streams.StreamType.VelocityStream s ->
        let smoothing_window = 15 in
        let smoothed_data =
          Utils.moving_average (module Utils.FloatOps) smoothing_window s.data
        in
        let data =
          List.fold ~init:""
            ~f:(fun acc v -> acc ^ sprintf "%f," v)
            smoothed_data
        in
        let format_fn, unit =
          match Helpers.velocity_type_of_activity_type activity.sport_type with
          | Helpers.PaceVelocity ->
              (Helpers.pace_stat_value, Helpers.pace_stat.unit)
          | Helpers.SpeedVelocity ->
              (Helpers.speed_stat_value, Helpers.speed_stat.unit)
        in
        let data_labels =
          Some
            (List.fold ~init:""
               ~f:(fun acc v -> acc ^ sprintf "'%s'," (format_fn v))
               smoothed_data)
        in
        let t = empty () in
        {
          t with
          graph_type = VelocityGraph;
          label = "Velocity";
          data;
          data_labels;
          fill = true;
          order = 2;
          unit;
          color = blue;
          display = false;
          extra_scale_fields = [ "grace: '20%'," ];
        }
    | Models.Streams.StreamType.DistanceStream s ->
        let data =
          List.fold ~init:"" ~f:(fun acc v -> acc ^ sprintf "%f," v) s.data
        in
        let data_labels =
          Some
            (List.fold ~init:""
               ~f:(fun acc v ->
                 acc ^ sprintf "'%s'," (Helpers.distance_stat_value v))
               s.data)
        in
        let t = empty () in
        {
          t with
          graph_type = DistanceGraph;
          label = "Distance";
          data;
          data_labels;
          display = false;
        }
    | Models.Streams.StreamType.WattsStream s ->
        let smoothing_window = 5 in
        let data = Utils.option_moving_average smoothing_window s.data in
        let data =
          List.fold ~init:""
            ~f:(fun acc v ->
              let v =
                match v with None -> "null" | Some value -> sprintf "%d" value
              in
              acc ^ sprintf "%s," v)
            data
        in

        let max_power = Option.value_exn activity.stats.max_power in
        let suggested_max = Float.of_int max_power *. 1.3 |> Float.to_int in
        let t = empty () in
        {
          t with
          graph_type = PowerGraph;
          label = "Power";
          data;
          color = yellow;
          display = false;
          fill = true;
          order = 3;
          unit = Helpers.power_stat.unit;
          extra_scale_fields = [ sprintf "suggestedMax: %d," suggested_max ];
        }
    | Models.Streams.StreamType.TempStream s ->
        let data = s.data in
        let data =
          List.fold ~init:"" ~f:(fun acc v -> acc ^ sprintf "%d," v) data
        in
        let t = empty () in
        {
          t with
          graph_type = TemperatureGraph;
          label = "Temperature";
          data;
          color = purple;
          display = false;
          fill = true;
          order = 4;
          unit = Helpers.temperature_stat.unit;
          extra_scale_fields = [ "grace: '20%'," ];
        }
    | _ -> assert false

  let dataset (t : t) =
    let fill_color =
      String.substr_replace_first ~pattern:")" ~with_:", 0.2)" t.color
    in
    sprintf
      {|
        {
          label: '%s',
          fill: %s,
          data: [%s],
          normalized: true,
          borderWidth: 1,
          borderColor: '%s',
          backgroundColor: '%s',
          pointStyle: false,
          yAxisID: '%s',
          hidden: %s,
          order: %d,
          %s
        },
      |}
      t.label (Bool.to_string t.fill) t.data t.color fill_color t.label
      (Bool.to_string (not t.display))
      t.order
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
    (* NOTE: display here controls if the scales are shown and not the dataset itself *)
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

  let x_scale (t : t) =
    assert (equal_graphType t.graph_type TimeGraph);

    sprintf
      {|
        x: {
          type: 'linear',
          display: true,
          max: %d,
          ticks: {
            callback: function(value, index, values) {
              return timeLabels[value];
            },
          },
        },
      |}
      (Option.value_exn t.data_len)
end

module Graph = struct
  type t = {
    show_altitude : bool;
    x_axis : GraphData.t;
    lines : GraphData.t list;
    extra_lines : GraphData.t list;
  }

  let empty () =
    {
      show_altitude = false;
      x_axis = GraphData.empty ();
      lines = [];
      extra_lines = [];
    }

  let add_stream ~(activity : Models.Activity.t) (acc : t)
      (stream : Models.Streams.StreamType.t) =
    let line = GraphData.of_stream ~activity stream in
    match stream with
    | Models.Streams.StreamType.TimeStream _ -> { acc with x_axis = line }
    | Models.Streams.StreamType.DistanceStream _ ->
        { acc with extra_lines = line :: acc.extra_lines }
    | Models.Streams.StreamType.VelocityStream _ ->
        { acc with show_altitude = true; lines = line :: acc.lines }
    | _ -> { acc with lines = line :: acc.lines }

  let of_streams ~(activity : Models.Activity.t)
      (streams : Models.Streams.StreamType.t list) =
    let streams =
      List.filter
        ~f:(fun stream ->
          match stream with
          | Models.Streams.StreamType.VelocityStream _
          | Models.Streams.StreamType.AltitudeStream _ ->
              Option.is_some activity.stats.average_speed
          | Models.Streams.StreamType.TimeStream _
          | Models.Streams.StreamType.DistanceStream _
          | Models.Streams.StreamType.WattsStream _
          | Models.Streams.StreamType.TempStream _
          | Models.Streams.StreamType.HeartRateStream _ ->
              true
          | _ -> false)
        streams
    in
    let graph = List.fold ~init:(empty ()) ~f:(add_stream ~activity) streams in
    {
      graph with
      lines =
        graph.lines
        |> List.sort ~compare:(fun l1 l2 -> Int.compare l1.order l2.order);
    }

  (* NOTE: this is passed to Chart.labels but in reality is the raw int seconds *)
  let x_axis_labels (t : t) = t.x_axis.data

  (* NOTE: this contains all formatted data to be shown by tooltip *)
  let x_axis_formatted (t : t) = Option.value_exn t.x_axis.data_labels

  let velocity_formatted (t : t) =
    match
      List.find
        ~f:(fun line -> equal_graphType line.graph_type VelocityGraph)
        t.lines
    with
    | None -> ""
    | Some line ->
        sprintf "const velocityLabels = [%s];"
          (Option.value_exn line.data_labels)

  let distance_formatted (t : t) =
    match
      List.find
        ~f:(fun line -> equal_graphType line.graph_type DistanceGraph)
        t.extra_lines
    with
    | None -> ""
    | Some line ->
        sprintf "const distanceLabels = [%s];"
          (Option.value_exn line.data_labels)

  let datasets (t : t) =
    t.lines |> List.map ~f:(fun line -> GraphData.dataset line) |> String.concat

  let y_scales (t : t) =
    t.lines
    |> List.mapi ~f:(fun i line -> GraphData.scale ~to_left:(i = 0) line)
    |> String.concat

  let x_scale (t : t) = GraphData.x_scale t.x_axis

  let unit_definitions (t : t) =
    t.lines
    |> List.map ~f:(fun line ->
           sprintf "const %sUnit = '%s';" line.label line.unit)
    |> String.concat ~sep:"\n"

  let data_definitions (t : t) =
    sprintf
      {|
  const timeLabels = [%s];
  %s
  %s
      |}
      (x_axis_formatted t) (velocity_formatted t) (distance_formatted t)

  let tooltip_footer (t : t) =
    let distance_line =
      List.find
        ~f:(fun line -> equal_graphType line.graph_type DistanceGraph)
        t.extra_lines
    in
    match distance_line with
    | None -> ""
    | Some _ ->
        {| footer: function(tooltipItems) {
          const idx = tooltipItems[0].dataIndex;
          return distanceLabels[idx] + "km";
       },
    |}

  (* TODO: add scale callback to show the speed/pace correctly *)
  let script_of_activity (activity : Models.Activity.t) =
    let graph = of_streams ~activity activity.streams in
    sprintf
      {|
  %s
  %s
  const formatTime = (tooltipItem) => {
    return timeLabels[tooltipItem[0].dataIndex];
  };
  const formatLabel = (tooltipItem) => {
    let label = tooltipItem.dataset.label || '';

    if (label) {
        label += ': ';
    }

    const idx = tooltipItem.dataIndex;
    if (tooltipItem.dataset.label === "Velocity") {
      return label + velocityLabels[idx] + VelocityUnit;
    } else if (tooltipItem.dataset.label === "Heartrate") {
      return label + tooltipItem.formattedValue + HeartrateUnit;
    } else if (tooltipItem.dataset.label === "Altitude") {
      const val = tooltipItem.raw.toFixed(1);
      return label + val + AltitudeUnit;
    } else if (tooltipItem.dataset.label === "Power") {
      return label + tooltipItem.formattedValue + PowerUnit;
    } else if (tooltipItem.dataset.label === "Temperature") {
      return label + tooltipItem.formattedValue + TemperatureUnit;
    }

    return label + tooltipItem.formattedValue;
  };
  const ctx = document.getElementById('streamsChart');

  new Chart(ctx, {
    type: 'line',
    data: {
      labels: [%s],
      datasets: [%s]
    },
    options: {
      responsive: true,
      stacked: false,
      interaction: {
        mode: 'index',
        intersect: false
      },
      plugins: {
        tooltip: {
          callbacks: {
            title: formatTime,
            label: formatLabel,
            %s
          },
        },
      },
      scales: {
        %s
        %s
      },
    }
  });
    |}
      (data_definitions graph) (unit_definitions graph) (x_axis_labels graph)
      (datasets graph) (tooltip_footer graph) (x_scale graph) (y_scales graph)
end
