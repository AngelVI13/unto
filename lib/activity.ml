open Strava_models

type t = {
  id : int;
  athlete_id : int;
  name : string;
  sport_type : sportType;
  start_date : string;
  timezone : string;
  map_id : string;
  map_summary_polyline : string;
  (* NOTE: these are calculated from the data streams of the
       activity and not taken from strava directly *)
  stats : Stats.t;
  laps : Laps.t;
}
[@@deriving show { with_path = false }]

let t_of_StravaActivity (activity : StravaActivity.t) : t =
  {
    id = activity.id;
    athlete_id = activity.athlete.id;
    name = activity.name;
    sport_type = sportType_of_string activity.sport_type;
    start_date = activity.start_date_local;
    timezone = activity.timezone;
    map_id = activity.map.id;
    map_summary_polyline = activity.map.summary_polyline;
    stats = Stats.empty ();
    laps = Laps.empty ();
  }
