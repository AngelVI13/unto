open! Core
open Dream_html
open HTML

let split_stat_headers ~(sport_type : Models.Strava_models.sportType)
    (stats : Models.Stats.t) =
  let split_nr = Some (txt "#") in
  let duration = Some (Helpers.Stat.stat_icon Helpers.duration_stat) in

  let open Option.Monad_infix in
  let distance =
    stats.distance >>| fun _ -> Helpers.Stat.stat_icon Helpers.distance_stat
  in

  let speed_pace =
    stats.average_speed >>| fun _ ->
    match Helpers.velocity_type_of_activity_type sport_type with
    | Helpers.PaceVelocity -> Helpers.Stat.stat_icon Helpers.pace_stat
    | Helpers.SpeedVelocity -> Helpers.Stat.stat_icon Helpers.speed_stat
  in

  let heartrate =
    stats.average_heartrate >>| fun _ -> Helpers.Stat.stat_icon Helpers.hr_stat
  in

  let gain_loss =
    Option.both stats.elev_gain stats.max_speed >>| fun _ ->
    Helpers.Stat.stat_icon Helpers.elev_gain_loss_stat
  in

  let power =
    stats.average_power >>| fun _ -> Helpers.Stat.stat_icon Helpers.power_stat
  in

  let cadence =
    stats.average_cadence >>| fun _ ->
    Helpers.Stat.stat_icon Helpers.cadence_stat
  in

  (* NOTE: the order of stats has to match the one in split_stat_values *)
  let columns =
    [
      split_nr;
      duration;
      distance;
      speed_pace;
      heartrate;
      power;
      gain_loss;
      cadence;
    ]
    |> List.filter_opt
    |> List.map ~f:(fun stat -> th [] [ stat ])
  in
  columns

let split_stat_values ~(activity : Models.Activity.t) (index : int)
    (stats : Models.Stats.t) =
  let sport_type = activity.sport_type in

  let make_bar_row ~color ~percent value =
    (* NOTE: reduce the color alpha *)
    let color =
      String.substr_replace_first ~pattern:")" ~with_:", 0.4)" color
    in
    td []
      [
        div
          [
            class_ "bar"; style_ "width: %d%s; background: %s" percent "%" color;
          ]
          [];
        span [] [ txt "%s" value ];
      ]
  in

  let make_txt_row value = td [] [ txt "%s" value ] in

  let split_idx = Some (make_txt_row @@ sprintf "%d" (index + 1)) in

  let duration =
    Some (make_txt_row @@ Helpers.duration_stat_value stats.moving_time)
  in

  let open Option.Monad_infix in
  let distance =
    stats.distance >>| Helpers.distance_stat_value >>| make_txt_row
  in

  let speed_pace =
    stats.average_speed >>| fun avg_val ->
    let max_val = Option.value_exn stats.max_speed in
    let to_string =
      match Helpers.velocity_type_of_activity_type sport_type with
      | Helpers.PaceVelocity -> Helpers.pace_stat_value
      | Helpers.SpeedVelocity -> Helpers.speed_stat_value
    in
    let value = sprintf "%s (%s)" (to_string avg_val) (to_string max_val) in

    let activity_max = Option.value_exn activity.stats.average_speed in
    let percent =
      Float.(to_int (round_nearest (activity_max / avg_val * 100.0) - 50.0))
    in
    make_bar_row ~color:Activity_graph.GraphData.blue ~percent value
  in

  (* TODO: add bars for heartrate and power and maybe elevation or sth? *)
  let heartrate =
    stats.average_heartrate >>| fun avg_val ->
    let max_val = Option.value_exn stats.max_heartrate in
    let to_string = Helpers.hr_stat_value in
    make_txt_row @@ sprintf "%s (%s)" (to_string avg_val) (to_string max_val)
  in

  let power =
    stats.average_power >>| fun avg_val ->
    let max_val = Option.value_exn stats.max_power in
    let to_string = Helpers.power_stat_value in
    make_txt_row @@ sprintf "%s (%s)" (to_string avg_val) (to_string max_val)
  in

  let gain_loss =
    Option.both stats.elev_gain stats.max_speed >>| fun (elev_gain, _) ->
    let elev_loss = Option.value_exn stats.elev_loss in
    make_txt_row @@ Helpers.elev_gain_loss_stat_value elev_gain elev_loss
  in

  let cadence =
    stats.average_cadence >>| fun avg_val ->
    let max_val = Option.value_exn stats.max_cadence in
    let to_string = Helpers.cadence_stat_value in
    make_txt_row @@ sprintf "%s (%s)" (to_string avg_val) (to_string max_val)
  in

  let values =
    [
      split_idx;
      duration;
      distance;
      speed_pace;
      heartrate;
      power;
      gain_loss;
      cadence;
    ]
    |> List.filter_opt
  in
  tr [] values

type splitLapSelector = Laps | Splits
[@@deriving show { with_path = false }, sexp, eq]

let splitLapSelector_of_string s = splitLapSelector_of_sexp (Sexp.of_string s)

(* NOTE: this whole file works the same but for laps *)
let activity_splits_table ~(activity : Models.Activity.t)
    ~(split_select : splitLapSelector) (stats : Models.Stats.t list) =
  match List.length stats with
  | 0 -> txt "No %s present" (show_splitLapSelector split_select)
  | _ ->
      let sport_type = activity.sport_type in
      let nodes = List.mapi ~f:(split_stat_values ~activity) stats in
      div
        [ class_ "splitsTable" ]
        [
          table
            [ id "splitsTable" ]
            [
              thead []
                [ tr [] (split_stat_headers ~sport_type (List.hd_exn stats)) ];
              tbody [] nodes;
            ];
        ]

let activity_splits_table_formatting =
  {|
    const table = document.getElementById("splitsTable");
    console.log(table);
    const cells = Array.from(table.querySelectorAll(".heartrateRow")); // 2nd column
    console.log(cells);
    const values = cells.map(td => 
    parseInt(td.textContent.split(" (")[0]));
    
    const min = Math.min(...values);
    const max = Math.max(...values);

    cells.forEach((td, i) => {
      const val = values[i];
      const rawPercent = ((val - min) / (max - min)) * 100;
      const percent = Math.max(rawPercent, 10);

      // Create bar div
      const bar = document.createElement("div");
      bar.className = "bar";
      bar.style.width = percent + "%";
      bar.style.background = "rgba(255, 99, 132, 0.5)";

      // Wrap text
      const span = document.createElement("span");
      span.textContent = td.textContent;

      td.textContent = "";
      td.appendChild(bar);
      td.appendChild(span);
    });
  |}
