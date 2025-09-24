open! Core
open Dream_html
open HTML

let split_stat_headers ~(sport_type : Models.Strava_models.sportType)
    (split : Models.Splits.Split.t) =
  let stats = split.stats in

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
    stats.elev_gain >>| fun _ ->
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

let split_stat_values ~(sport_type : Models.Strava_models.sportType)
    (split : Models.Splits.Split.t) =
  let stats = split.stats in

  let split_idx = Some (sprintf "%d" (split.split_index + 1)) in

  let duration = Some (Helpers.duration_stat_value stats.moving_time) in

  let open Option.Monad_infix in
  let distance = stats.distance >>| Helpers.distance_stat_value in

  let speed_pace =
    stats.average_speed >>| fun avg_val ->
    let max_val = Option.value_exn stats.max_speed in
    let to_string =
      match Helpers.velocity_type_of_activity_type sport_type with
      | Helpers.PaceVelocity -> Helpers.pace_stat_value
      | Helpers.SpeedVelocity -> Helpers.speed_stat_value
    in
    sprintf "%s (%s)" (to_string avg_val) (to_string max_val)
  in

  let heartrate =
    stats.average_heartrate >>| fun avg_val ->
    let max_val = Option.value_exn stats.max_heartrate in
    let to_string = Helpers.hr_stat_value in
    sprintf "%s (%s)" (to_string avg_val) (to_string max_val)
  in

  let power =
    stats.average_power >>| fun avg_val ->
    let max_val = Option.value_exn stats.max_power in
    let to_string = Helpers.power_stat_value in
    sprintf "%s (%s)" (to_string avg_val) (to_string max_val)
  in

  let gain_loss =
    stats.elev_gain >>| fun elev_gain ->
    let elev_loss = Option.value_exn stats.elev_loss in
    Helpers.elev_gain_loss_stat_value elev_gain elev_loss
  in

  let cadence =
    stats.average_cadence >>| fun avg_val ->
    let max_val = Option.value_exn stats.max_cadence in
    let to_string = Helpers.cadence_stat_value in
    sprintf "%s (%s)" (to_string avg_val) (to_string max_val)
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
    |> List.map ~f:(fun value -> td [] [ txt "%s" value ])
  in
  tr [] values

let activity_splits_table (activity : Models.Activity.t) =
  match List.length activity.splits with
  | 0 -> txt "No splits present"
  | _ ->
      let nodes =
        List.map
          ~f:(split_stat_values ~sport_type:activity.sport_type)
          activity.splits
      in
      div
        [ class_ "splitsTable" ]
        [
          table []
            [
              thead []
                [
                  tr []
                    (split_stat_headers ~sport_type:activity.sport_type
                       (List.hd_exn activity.splits));
                ];
              tbody [] nodes;
            ];
        ]
