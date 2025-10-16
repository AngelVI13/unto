open Core
open Dream_html
open HTML

type summaryType = Week | Month [@@deriving show { with_path = false }]

(* TODO: update all classnames from week -> summary *)
let summary_stat ~stat_label ~stat_value =
  div
    [ class_ "weekTotalStat" ]
    [
      span [ class_ "weekStatName" ] [ txt "%s" stat_label ];
      span [] [ txt "%s" stat_value ];
    ]

module Totals = struct
  type t = {
    duration : int;
    calories : int;
    distance : float option;
    elev_gain : int option;
    elev_loss : int option;
  }

  let empty =
    {
      duration = 0;
      calories = 0;
      distance = None;
      elev_gain = None;
      elev_loss = None;
    }

  let add_elev_data (elev_gain : int option) (elev_loss : int option) (t : t) =
    {
      t with
      elev_gain =
        (match t.elev_gain with
        | None -> elev_gain
        | Some gain -> (
            match elev_gain with
            | None -> Some gain
            | Some act_gain -> Some (gain + act_gain)));
      elev_loss =
        (match t.elev_loss with
        | None -> elev_loss
        | Some loss -> (
            match elev_loss with
            | None -> Some loss
            | Some act_loss -> Some (loss + act_loss)));
    }

  let add_distance (distance : float option) (t : t) =
    {
      t with
      distance =
        (match t.distance with
        | None -> distance
        | Some dist -> (
            match distance with
            | None -> Some dist
            | Some act_dist -> Some (dist +. act_dist)));
    }

  let add_activity (athlete : Models.Strava_models.StravaAthlete.t option)
      (acc : t) (activity : Models.Activity.t) =
    let duration = activity.stats.moving_time in

    let calories =
      match activity.stats.average_heartrate with
      | None -> 0
      | Some avg_hr -> (
          match athlete with
          | None -> 0
          | Some athlete ->
              Helpers.calculate_calories ~athlete ~avg_hr ~duration ())
    in
    {
      acc with
      duration = acc.duration + duration;
      calories = acc.calories + calories;
    }
    |> add_elev_data activity.stats.elev_gain activity.stats.elev_loss
    |> add_distance activity.stats.distance

  let of_activities athlete activities =
    List.fold ~init:empty ~f:(add_activity athlete) activities
end

let summary_activity_stat
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list) =
  let hd = List.hd_exn activities in
  let totals = Totals.of_activities athlete activities in

  let duration = Some (Helpers.duration_stat_node totals.duration) in
  let calories = Some (Helpers.calories_stat_from_total_node totals.calories) in
  let distance, elevation =
    match totals.distance with
    | None -> (None, None)
    | Some distance ->
        let distance = Some (Helpers.distance_stat_node distance) in
        let elevation =
          match (totals.elev_gain, totals.elev_loss) with
          | None, None -> None
          | Some gain, Some loss ->
              Some (Helpers.elev_gain_loss_stat_node gain loss)
          | _, _ -> assert false
        in
        (distance, elevation)
  in
  let stats = [ duration; distance; elevation; calories ] in
  let stats = List.filter_opt stats in
  div
    [ class_ "summaryContainer" ]
    [ Helpers.activity_header hd; div [] [ div [ class_ "weekTotals" ] stats ] ]

let total_summary ~(type_ : summaryType)
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list) =
  (* TODO: training load is just calories/10 -> its not but calories are part of it *)
  let totals = Totals.of_activities athlete activities in
  let duration = Helpers.duration_stat_value totals.duration in
  div
    [ class_ "totalsSummaryContainer" ]
    [
      h2 [] [ txt "%s Stats" (show_summaryType type_) ];
      div []
        [
          div
            [ class_ "weekTotals" ]
            [
              (* TODO: add averages somehow ? *)
              summary_stat ~stat_label:"Duration: " ~stat_value:duration;
              summary_stat ~stat_label:"Calories: "
                ~stat_value:(Int.to_string totals.calories);
            ];
        ];
    ]

let component ~(type_ : summaryType)
    (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list list) =
  let activities = List.concat activities in
  let activities_grouped =
    activities
    (* group by sport type *)
    |> List.sort_and_group ~compare:(fun act1 act2 ->
           Models.Strava_models.compare_sportType act1.sport_type
             act2.sport_type)
    (* sort by duration of group *)
    |> List.sort ~compare:(fun act_group1 act_group2 ->
           let totals1 = Totals.of_activities athlete act_group1 in
           let totals2 = Totals.of_activities athlete act_group2 in
           Int.compare totals1.duration totals2.duration)
    (* longest duration group first *)
    |> List.rev
  in
  let all_sumarries =
    [
      total_summary ~type_ athlete activities;
      div [ class_ "verticalLine" ] [];
      div [ class_ "horizontalLine" ] [];
    ]
    @ List.map ~f:(summary_activity_stat athlete) activities_grouped
  in
  div [ class_ "stats card statsGrid calendarSummary" ] all_sumarries
