open Strava_models
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  start_index : int;
  end_index : int;
  lap_index : int;
  stats : Stats.t;
}
[@@deriving show { with_path = false }, yojson, fields]

let t_of_StravaLap (lap : StravaLap.t) =
  {
    start_index = lap.start_index;
    end_index = lap.end_index;
    lap_index = lap.lap_index;
    stats = Stats.empty ();
  }

let set_stats (t : t) (stats : Stats.t) : t = { t with stats }
