open Core
open Dream_html
open HTML

module SplitAvgRange = struct
  type t = {
    min_velocity : float option;
    max_velocity : float option;
    min_heartrate : int option;
    max_heartrate : int option;
    min_power : int option;
    max_power : int option;
  }

  let empty () =
    {
      min_velocity = None;
      max_velocity = None;
      min_heartrate = None;
      max_heartrate = None;
      min_power = None;
      max_power = None;
    }

  let percent_of_int ~min_ ~max_ value =
    Float.(
      to_int
        (round_nearest
           ((of_int value - of_int min_) / (of_int max_ - of_int min_) * 100.0)))

  let percent_of_float ~min_ ~max_ value =
    Float.(to_int (round_nearest ((value - min_) / (max_ - min_) * 100.0)))

  let add_stats (acc : t) (stats : Models.Stats.t) =
    let acc =
      match stats.average_speed with
      | None -> acc
      | Some avg -> (
          match (acc.min_velocity, acc.max_velocity) with
          | None, None ->
              { acc with min_velocity = Some avg; max_velocity = Some avg }
          | Some min_, Some max_ ->
              {
                acc with
                min_velocity = Some (Float.min min_ avg);
                max_velocity = Some (Float.max max_ avg);
              }
          | _ -> assert false)
    in
    let acc =
      match stats.average_heartrate with
      | None -> acc
      | Some avg -> (
          match (acc.min_heartrate, acc.max_heartrate) with
          | None, None ->
              { acc with min_heartrate = Some avg; max_heartrate = Some avg }
          | Some min_, Some max_ ->
              {
                acc with
                min_heartrate = Some (Int.min min_ avg);
                max_heartrate = Some (Int.max max_ avg);
              }
          | _ -> assert false)
    in
    let acc =
      match stats.average_power with
      | None -> acc
      | Some avg -> (
          match (acc.min_power, acc.max_power) with
          | None, None ->
              { acc with min_power = Some avg; max_power = Some avg }
          | Some min_, Some max_ ->
              {
                acc with
                min_power = Some (Int.min min_ avg);
                max_power = Some (Int.max max_ avg);
              }
          | _ -> assert false)
    in

    acc

  let expand_ranges ?(percent = 10) (t : t) =
    let multiplier = Float.(of_int percent / 100.0) in
    let min_multiplier = 1.0 -. multiplier in
    let max_multiplier = 1.0 +. multiplier in
    let t =
      match (t.min_velocity, t.max_velocity) with
      | None, None -> t
      | Some min_, Some max_ ->
          {
            t with
            min_velocity = Some (min_ *. min_multiplier);
            max_velocity = Some (max_ *. max_multiplier);
          }
      | _ -> assert false
    in

    let t =
      match (t.min_heartrate, t.max_heartrate) with
      | None, None -> t
      | Some min_, Some max_ ->
          {
            t with
            min_heartrate = Some Float.(to_int (of_int min_ *. min_multiplier));
            max_heartrate = Some Float.(to_int (of_int max_ *. max_multiplier));
          }
      | _ -> assert false
    in

    let t =
      match (t.min_power, t.max_power) with
      | None, None -> t
      | Some min_, Some max_ ->
          {
            t with
            min_power = Some Float.(to_int (of_int min_ *. min_multiplier));
            max_power = Some Float.(to_int (of_int max_ *. max_multiplier));
          }
      | _ -> assert false
    in
    t
end

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

let make_bar_row ~color ~percent ~avg_percent value =
  let percent = Int.max percent 5 in
  let percent = Int.min percent 100 in
  (* NOTE: reduce the color alpha *)
  let fill_color =
    String.substr_replace_first ~pattern:")" ~with_:", 0.4)" color
  in
  let avg_line_color =
    String.substr_replace_first ~pattern:")" ~with_:", 0.8)" color
  in
  td []
    [
      div
        [
          class_ "bar";
          style_ "width: %d%s; background: %s;" percent "%" fill_color;
        ]
        [];
      div
        [
          class_ "avg-line";
          style_ "left: %d%s; background: %s;" avg_percent "%" avg_line_color;
        ]
        [];
      span [] [ txt "%s" value ];
    ]

let split_stat_values ~(sport_type : Models.Strava_models.sportType)
    ~(avg_stats : Models.Stats.t) ~(stats_ranges : SplitAvgRange.t)
    (index : int) (stats : Models.Stats.t) =
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

    let avg_min = Option.value_exn stats_ranges.min_velocity in
    let avg_max = Option.value_exn stats_ranges.max_velocity in
    let percent =
      SplitAvgRange.percent_of_float ~min_:avg_min ~max_:avg_max avg_val
    in
    let avg_percent =
      Option.value_exn avg_stats.average_speed
      |> SplitAvgRange.percent_of_float ~min_:avg_min ~max_:avg_max
    in
    make_bar_row ~color:Activity_graph.GraphData.blue ~percent value
      ~avg_percent
  in

  let heartrate =
    stats.average_heartrate >>| fun avg_val ->
    let max_val = Option.value_exn stats.max_heartrate in
    let to_string = Helpers.hr_stat_value in
    let value = sprintf "%s (%s)" (to_string avg_val) (to_string max_val) in

    let avg_min = Option.value_exn stats_ranges.min_heartrate in
    let avg_max = Option.value_exn stats_ranges.max_heartrate in
    let percent =
      SplitAvgRange.percent_of_int ~min_:avg_min ~max_:avg_max avg_val
    in
    let avg_percent =
      Option.value_exn avg_stats.average_heartrate
      |> SplitAvgRange.percent_of_int ~min_:avg_min ~max_:avg_max
    in
    make_bar_row ~color:Activity_graph.GraphData.red ~percent value ~avg_percent
  in

  let power =
    stats.average_power >>| fun avg_val ->
    let max_val = Option.value_exn stats.max_power in
    let to_string = Helpers.power_stat_value in
    let value = sprintf "%s (%s)" (to_string avg_val) (to_string max_val) in

    let avg_min = Option.value_exn stats_ranges.min_power in
    let avg_max = Option.value_exn stats_ranges.max_power in
    let percent =
      SplitAvgRange.percent_of_int ~min_:avg_min ~max_:avg_max avg_val
    in
    let avg_percent =
      Option.value_exn avg_stats.average_power
      |> SplitAvgRange.percent_of_int ~min_:avg_min ~max_:avg_max
    in
    make_bar_row ~color:Activity_graph.GraphData.yellow ~percent value
      ~avg_percent
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
  let sport_type = activity.sport_type in
  match List.length stats with
  | 0 -> txt "No %s present" (show_splitLapSelector split_select)
  | _ ->
      let stats_ranges =
        List.fold ~init:(SplitAvgRange.empty ()) ~f:SplitAvgRange.add_stats
          stats
        |> SplitAvgRange.expand_ranges ~percent:10
      in

      let nodes =
        List.mapi
          ~f:
            (split_stat_values ~sport_type ~avg_stats:activity.stats
               ~stats_ranges)
          stats
      in
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
