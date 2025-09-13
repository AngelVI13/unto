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

let activity_stat ~stat_name ~stat_description ~stat_icon_path ~stat_value =
  div
    [ class_ "activityCardStat" ]
    [
      (* NOTE: currently the text is not used - maybe remove? *)
      span
        [ class_ "activityCardStatName statNameText" ]
        [ txt "%s: " stat_name ];
      span
        [ class_ "activityCardStatName statNameIcon" ]
        [
          img
            [
              class_ "stat-icon-img";
              title_ stat_description;
              src stat_icon_path;
            ];
        ];
      span [ class_ "activityCardStatValue" ] [ txt "%s" stat_value ];
    ]

let format_activity_duration duration_secs =
  let secs = duration_secs mod 60 in
  let mins = duration_secs / 60 in
  let mins_left = mins mod 60 in
  let hours = mins / 60 in
  sprintf "%d:%02d:%02d" hours mins_left secs

let duration_stat_value moving_time = format_activity_duration moving_time

let duration_stat duration =
  activity_stat ~stat_name:"Duration" ~stat_description:"Duration (hh:mm:ss)"
    ~stat_icon_path:"/static/assets/duration.png"
    ~stat_value:(duration_stat_value duration)

let avg_hr_stat_value avg = Int.to_string avg

let avg_hr_stat avg =
  activity_stat ~stat_name:"Heartrate" ~stat_description:"Avg Heartrate (bpm)"
    ~stat_icon_path:"/static/assets/heartrate.png"
    ~stat_value:(avg_hr_stat_value avg)

let distance_stat_value distance =
  let distance =
    Float.round_significant ~significant_digits:3 (distance /. 1000.0)
  in
  sprintf "%.2f" distance

let distance_stat distance =
  activity_stat ~stat_name:"Distance" ~stat_description:"Distance (km)"
    ~stat_icon_path:"/static/assets/distance.png"
    ~stat_value:(distance_stat_value distance)

let pace_stat_value avg =
  let secs_per_km = Int.of_float (Float.round_down (1000.0 /. avg)) in
  let secs = secs_per_km mod 60 in
  let mins = secs_per_km / 60 in
  sprintf "%02d:%02d" mins secs

let speed_stat_value avg =
  sprintf "%.1f" Float.(round_significant ~significant_digits:3 (avg * 3.6))

let speed_pace_stat activity_type avg =
  match activity_type with
  | Models.Strava_models.Run | Models.Strava_models.TrailRun
  | Models.Strava_models.VirtualRun ->
      activity_stat ~stat_name:"Pace" ~stat_description:"Pace (min/km)"
        ~stat_icon_path:"/static/assets/pace.png"
        ~stat_value:(pace_stat_value avg)
  | _ ->
      activity_stat ~stat_name:"Speed" ~stat_description:"Speed (kph)"
        ~stat_icon_path:"/static/assets/speed.png"
        ~stat_value:(speed_stat_value avg)

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

let calories_stat_from_total calories =
  activity_stat ~stat_name:"Calories" ~stat_description:"Calories"
    ~stat_icon_path:"/static/assets/calories.png"
    ~stat_value:(Int.to_string calories)

let calories_stat ~(athlete : Models.Strava_models.StravaAthlete.t)
    ~(avg_hr : int) ~(duration : int) () =
  calories_stat_from_total (calories_stat_value ~athlete ~avg_hr ~duration ())

let elevation_stat elev_gain elev_loss =
  activity_stat ~stat_name:"Elevation"
    ~stat_description:"Elevation gain & loss (m)"
    ~stat_icon_path:"/static/assets/elevation.png"
    ~stat_value:(sprintf "+%d/-%d" elev_gain elev_loss)
