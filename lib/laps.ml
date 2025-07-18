open Core
open Strava_models
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Lap = struct
  type t = {
    moving_time : int;
    start : int;
    len : int;
    lap_index : int;
    stats : Stats.t;
  }
  [@@deriving show { with_path = false }, yojson, fields]

  let t_of_StravaLap (lap : StravaLap.t) start_index =
    {
      moving_time = lap.moving_time;
      start = start_index;
      len = lap.moving_time;
      lap_index = lap.lap_index;
      stats = Stats.empty ();
    }

  let set_stats (t : t) (stats : Stats.t) : t = { t with stats }
end

module Laps = struct
  type t = Lap.t list [@@deriving show { with_path = false }, yojson]

  let t_of_StravaLaps (strava_laps : StravaLaps.t) =
    let _, laps =
      List.fold ~init:(0, [])
        ~f:(fun (start, laps) strava_lap ->
          let lap = Lap.t_of_StravaLap strava_lap start in
          (start + strava_lap.moving_time, laps @ [ lap ]))
        strava_laps
    in
    laps
end
