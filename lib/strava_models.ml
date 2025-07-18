open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type sportType =
  | AlpineSki
  | BackcountrySki
  | Badminton
  | Canoeing
  | Crossfit
  | EBikeRide
  | Elliptical
  | EMountainBikeRide
  | Golf
  | GravelRide
  | Handcycle
  | HighIntensityIntervalTraining
  | Hike
  | IceSkate
  | InlineSkate
  | Kayaking
  | Kitesurf
  | MountainBikeRide
  | NordicSki
  | Pickleball
  | Pilates
  | Racquetball
  | Ride
  | RockClimbing
  | RollerSki
  | Rowing
  | Run
  | Sail
  | Skateboard
  | Snowboard
  | Snowshoe
  | Soccer
  | Squash
  | StairStepper
  | StandUpPaddling
  | Surfing
  | Swim
  | TableTennis
  | Tennis
  | TrailRun
  | Velomobile
  | VirtualRide
  | VirtualRow
  | VirtualRun
  | Walk
  | WeightTraining
  | Wheelchair
  | Windsurf
  | Workout
  | Yoga
[@@deriving yojson, show { with_path = false }, sexp]

let sportType_of_string s = sportType_of_sexp (Sexp.of_string s)

(* NOTE: here is a m/s to min/km converter:  *)
(* https://www.unitjuggler.com/convert-speed-from-ms-to-minkm.html?val=2.257 *)

module StravaLap = struct
  type t = { start_index : int; end_index : int; lap_index : int }
  [@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

  let empty () = { start_index = 0; end_index = 10; lap_index = 0 }
end

(* TODO: should i calculate splits here, i.e. for every 1km? because laps can differ from splits? *)

module StravaLaps = struct
  type t = StravaLap.t list [@@deriving show { with_path = false }, yojson]

  let empty () : t = [ StravaLap.empty () ]
end

module StravaPolylineMap = struct
  type t = { id : string; summary_polyline : string }
  [@@deriving yojson, show { with_path = false }] [@@yojson.allow_extra_fields]
end

module StravaAthlete = struct
  type t = { id : int }
  [@@deriving yojson, show { with_path = false }] [@@yojson.allow_extra_fields]
end

module StravaActivity = struct
  type t = {
    athlete : StravaAthlete.t;
    name : string;
    sport_type : string;
    id : int;
    start_date_local : string;
    timezone : string;
    map : StravaPolylineMap.t;
    gear_id : string option;
  }
  [@@deriving yojson, show { with_path = false }] [@@yojson.allow_extra_fields]
end

module StravaActivities = struct
  type t = StravaActivity.t list [@@deriving yojson, show { with_path = false }]
end
