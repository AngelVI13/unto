open Core
open Laps
open Splits
open Streams
open Strava_models
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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
  splits : Splits.t;
  streams : Streams.t; [@opaque]
}
[@@deriving show { with_path = false }, fields, yojson_of]

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
    laps = [];
    splits = [];
    streams = [];
  }

let calculate_stats (t : t) (streams : Streams.t) (laps : Laps.t) : t =
  let laps =
    List.map
      ~f:(fun l ->
        let lap_stats = Streams.lap_stats streams l in
        Lap.set_stats l lap_stats)
      laps
  in
  let stats = Streams.activity_stats streams in
  let splits = Streams.splits streams in
  let splits =
    match splits with
    | None -> []
    | Some splits ->
        List.map
          ~f:(fun split ->
            let split_stats = Streams.split_stats streams split in
            Split.set_stats split split_stats)
          splits
  in
  { t with streams; stats; laps; splits }
