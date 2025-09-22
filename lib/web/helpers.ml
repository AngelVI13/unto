open Core
open Dream_html
open HTML

let activity_icon_and_color (activity : Models.Activity.t) =
  let icon_color, img_src =
    match activity.sport_type with
    | Run -> ("gold", "/static/assets/running.png")
    | Ride -> ("coral", "/static/assets/cycling.png")
    | Crossfit -> ("greenyellow", "/static/assets/crosstrain.png")
    | _ -> ("gray", "/static/assets/unknown.png")
  in
  let icon_background = sprintf "background: %s;" icon_color in
  (icon_background, img_src)

module Stat = struct
  type t = {
    name : string;
    description : string;
    unit : string;
    icon_path : string;
  }

  let make ~name ~description ~unit ~icon_path =
    { name; description; unit; icon_path }

  let activity_stat (t : t) (value : string) =
    div
      [ class_ "activityCardStat" ]
      [
        (* NOTE: currently the text is not used - maybe remove? *)
        span
          [ class_ "activityCardStatName statNameText" ]
          [ txt "%s: " t.name ];
        span
          [ class_ "activityCardStatName statNameIcon" ]
          [
            img
              [
                class_ "stat-icon-img";
                title_ "%s (%s)" t.description t.unit;
                src "%s" t.icon_path;
              ];
          ];
        span [ class_ "activityCardStatValue" ] [ txt "%s" value ];
      ]

  let value_row (t : t) ~(value : string) =
    tr []
      [
        td []
          [
            span
              [ class_ "activityCardStatName statNameIcon" ]
              [
                img
                  [
                    class_ "stat-icon-img";
                    title_ "%s" t.description;
                    src "%s" t.icon_path;
                  ];
              ];
          ];
        td [] [ txt "%s" value ];
      ]

  let min_max_avg_row (t : t) ~(min_value : string) ~(max_value : string)
      ~(avg_value : string) =
    tr []
      [
        td []
          [
            span
              [ class_ "activityCardStatName statNameIcon" ]
              [
                img
                  [
                    class_ "stat-icon-img";
                    title_ "%s" t.description;
                    src "%s" t.icon_path;
                  ];
              ];
          ];
        td [] [ txt "%s" avg_value ];
        td [] [ txt "%s" max_value ];
        td [] [ txt "%s" min_value ];
      ]
end

let format_activity_duration duration_secs =
  let secs = duration_secs mod 60 in
  let mins = duration_secs / 60 in
  let mins_left = mins mod 60 in
  let hours = mins / 60 in
  sprintf "%d:%02d:%02d" hours mins_left secs

let duration_stat_value moving_time = format_activity_duration moving_time

let duration_stat =
  Stat.make ~name:"Duration" ~description:"Duration" ~unit:"hh:mm:ss"
    ~icon_path:"/static/assets/duration.png"

let hr_stat =
  Stat.make ~name:"Heartrate" ~description:"Heartrate" ~unit:"bpm"
    ~icon_path:"/static/assets/heartrate.png"

let distance_stat =
  Stat.make ~name:"Distance" ~description:"Distance" ~unit:"km"
    ~icon_path:"/static/assets/distance.png"

let pace_stat =
  Stat.make ~name:"Pace" ~description:"Pace" ~unit:"min/km"
    ~icon_path:"/static/assets/pace.png"

let speed_stat =
  Stat.make ~name:"Speed" ~description:"Speed" ~unit:"kph"
    ~icon_path:"/static/assets/speed.png"

let calories_stat =
  Stat.make ~name:"Calories" ~description:"Calories" ~unit:"kcal"
    ~icon_path:"/static/assets/calories.png"

let elev_gain_loss_stat =
  Stat.make ~name:"Elevation" ~description:"Elevation gain & loss" ~unit:"m"
    ~icon_path:"/static/assets/elevation.png"

let power_stat =
  Stat.make ~name:"Power" ~description:"Power" ~unit:"Watt"
    ~icon_path:"/static/assets/power.png"

let cadence_stat =
  Stat.make ~name:"Cadence" ~description:"Cadence" ~unit:"steps/min or rev/min"
    ~icon_path:"/static/assets/cadence.png"

let elevation_stat =
  Stat.make ~name:"Elevation" ~description:"Elevation" ~unit:"m"
    ~icon_path:"/static/assets/elevation.png"

let temperature_stat =
  Stat.make ~name:"Temperature" ~description:"Temperature" ~unit:"C"
    ~icon_path:"/static/assets/temperature.png"

let elevation_stat_value elev = Int.to_string elev

let elevation_stat_node elev =
  Stat.activity_stat elevation_stat (elevation_stat_value elev)

let cadence_stat_value cad = Int.to_string cad

let cadence_stat_node cad =
  Stat.activity_stat cadence_stat (cadence_stat_value cad)

let power_stat_value pwr = Int.to_string pwr
let power_stat_node pwr = Stat.activity_stat power_stat (power_stat_value pwr)

let duration_stat_node duration =
  Stat.activity_stat duration_stat (duration_stat_value duration)

let hr_stat_value hr = Int.to_string hr
let hr_stat_node hr = Stat.activity_stat hr_stat (hr_stat_value hr)

let distance_stat_value distance =
  let distance =
    Float.round_significant ~significant_digits:3 (distance /. 1000.0)
  in
  sprintf "%.2f" distance

let distance_stat_node distance =
  Stat.activity_stat distance_stat (distance_stat_value distance)

let pace_stat_value avg =
  let secs_per_km = Int.of_float (Float.round_down (1000.0 /. avg)) in
  let secs = secs_per_km mod 60 in
  let mins = secs_per_km / 60 in
  sprintf "%02d:%02d" mins secs

let speed_stat_value avg =
  sprintf "%.1f" Float.(round_significant ~significant_digits:3 (avg * 3.6))

type velocityType = PaceVelocity | SpeedVelocity

let velocity_type_of_activity_type
    (activity_type : Models.Strava_models.sportType) =
  match activity_type with
  | Models.Strava_models.Run | Models.Strava_models.TrailRun
  | Models.Strava_models.VirtualRun ->
      PaceVelocity
  | _ -> SpeedVelocity

let speed_pace_stat_node activity_type value =
  match velocity_type_of_activity_type activity_type with
  | PaceVelocity -> Stat.activity_stat pace_stat (pace_stat_value value)
  | SpeedVelocity -> Stat.activity_stat speed_stat (speed_stat_value value)

let speed_pace_stat_value activity_type value =
  match activity_type with
  | Models.Strava_models.Run | Models.Strava_models.TrailRun
  | Models.Strava_models.VirtualRun ->
      pace_stat_value value
  | _ -> speed_stat_value value

let calculate_calories ~(athlete : Models.Strava_models.StravaAthlete.t)
    ?(multiplier = 0.80) ~(avg_hr : int) ~(duration : int) () =
  let mins = Float.of_int duration /. 60.0 in

  (* TODO: get these 2 values from user. Multiplier is used to scale down
     calories so they better match with suunto numbers. You should start with
     multiplier 1 and then take the suunto calories and divide them by the
     calories from Unto and then this is your multiplier. For example 320
     (suunto) / 400 (unto) -> 0.80 multiplier *)
  (* TODO: those values should get stored for the activity ? Or better, we
     should store the user's birthyear so we can calculate his age for each
     activity so then we don't have to store the calories data to stats *)
  let age = Float.of_int 31 in

  (* NOTE: this formula was taken from here:
    https://www.omnicalculator.com/sports/calories-burned-by-heart-rate *)
  let calories =
    match athlete.sex with
    | "M" ->
        mins
        *. ((0.6309 *. Float.of_int avg_hr)
           +. (0.1988 *. athlete.weight) +. (0.2017 *. age) -. 55.0969)
        /. 4.184
    | "F" ->
        mins
        *. ((0.4472 *. Float.of_int avg_hr)
           +. (0.1263 *. athlete.weight) +. (0.0074 *. age) -. 20.4022)
        /. 4.184
    | _ -> assert false
  in
  Float.to_int (Float.round_nearest (calories *. multiplier))

let calories_stat_value ~(athlete : Models.Strava_models.StravaAthlete.t)
    ?(multiplier = 0.80) ~(avg_hr : int) ~(duration : int) () =
  calculate_calories ~athlete ~multiplier ~avg_hr ~duration ()

let calories_stat_from_total_node calories =
  Stat.activity_stat calories_stat (Int.to_string calories)

let calories_stat_node ~(athlete : Models.Strava_models.StravaAthlete.t)
    ~(avg_hr : int) ~(duration : int) () =
  calories_stat_from_total_node
    (calories_stat_value ~athlete ~avg_hr ~duration ())

let elev_gain_loss_stat_value elev_gain elev_loss =
  sprintf "+%d/-%d" elev_gain elev_loss

let elev_gain_loss_stat_node elev_gain elev_loss =
  Stat.activity_stat elev_gain_loss_stat
    (elev_gain_loss_stat_value elev_gain elev_loss)
