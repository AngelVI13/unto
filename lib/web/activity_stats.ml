open Core
open Dream_html
open HTML

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
