open Core
open Import
open Elevation
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Or_error.Let_syntax

(* TODO: use https://github.com/mmottl/sqlite3-ocaml for sqlite storage *)
(* some examples can be found here: https://github.com/patoline/patoline/blob/75fd8c928efc68d0aaa400d3a699a0e668c26c5f/permap/permap.ml#L43 *)

(* TODO: 1. define the strava API I'm using (with only the needed fields) *)
(* TODO: 2. Insert data to databases on every request if the data is not already there *)
(* TODO: 3. Visualize the data in a web ui *)
(* TODO: 4. Analyze the data *)

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

module Stats = struct
  type t = {
    moving_time : int;
    elapsed_time : int;
    distance : float option;
    elev_gain : int option;
    elev_loss : int option;
    elev_high : int option;
    elev_low : int option;
    start_latlng : (float * float) option;
    end_latlng : (float * float) option;
    average_speed : float option;
    max_speed : float option;
    average_cadence : int option;
    max_cadence : int option;
    average_temp : int option;
    average_heartrate : int option;
    max_heartrate : int option;
    average_power : int option;
    max_power : int option;
  }
  [@@deriving show { with_path = false }, yojson]

  let empty () =
    {
      moving_time = 0;
      elapsed_time = 0;
      distance = None;
      elev_gain = None;
      elev_loss = None;
      elev_high = None;
      elev_low = None;
      start_latlng = None;
      end_latlng = None;
      average_speed = None;
      max_speed = None;
      average_cadence = None;
      max_cadence = None;
      average_temp = None;
      average_heartrate = None;
      max_heartrate = None;
      average_power = None;
      max_power = None;
    }
end

module Stream = struct
  type 'a t = {
    type_ : string; [@key "type"]
    data : 'a list;
    series_type : string;
    original_size : int;
    resolution : string;
  }
  [@@deriving show { with_path = false }, yojson]
end

module StreamType = struct
  type t =
    | TimeStream of int Stream.t
    | DistanceStream of float Stream.t
    | LatLngStream of float list Stream.t
    | AltitudeStream of float Stream.t
    | VelocityStream of float Stream.t
    | HeartRateStream of int Stream.t
    | CadenceStream of int Stream.t
    | WattsStream of int option Stream.t
    | TempStream of int Stream.t
    | GradeStream of float Stream.t
  [@@deriving show { with_path = false }, yojson_of]

  let t_of_yojson (json : Yojson.Safe.t) : t =
    let open Yojson.Safe.Util in
    let stream_type = Yojson.Safe.Util.member "type" json |> to_string in
    match stream_type with
    | "time" -> TimeStream (Stream.t_of_yojson [%of_yojson: int] json)
    | "distance" -> DistanceStream (Stream.t_of_yojson [%of_yojson: float] json)
    | "latlng" ->
        LatLngStream (Stream.t_of_yojson [%of_yojson: float list] json)
    | "altitude" -> AltitudeStream (Stream.t_of_yojson [%of_yojson: float] json)
    | "velocity_smooth" ->
        VelocityStream (Stream.t_of_yojson [%of_yojson: float] json)
    | "heartrate" -> HeartRateStream (Stream.t_of_yojson [%of_yojson: int] json)
    | "cadence" -> CadenceStream (Stream.t_of_yojson [%of_yojson: int] json)
    | "watts" -> WattsStream (Stream.t_of_yojson [%of_yojson: int option] json)
    | "temp" -> TempStream (Stream.t_of_yojson [%of_yojson: int] json)
    | "grade_smooth" ->
        GradeStream (Stream.t_of_yojson [%of_yojson: float] json)
    | _ -> failwith "unsupported"

  let stats_of_t (stats : Stats.t) (stream : t) : Stats.t =
    match stream with
    | TimeStream s ->
        let elapsed_time = List.last_exn s.data in
        let moving_time = List.length s.data - 1 in
        { stats with elapsed_time; moving_time }
    | DistanceStream s ->
        let distance = List.last_exn s.data in
        let average_speed = distance /. Float.of_int (List.length s.data - 2) in
        let average_speed =
          Float.round_decimal ~decimal_digits:3 average_speed
        in
        let distance, average_speed = (Some distance, Some average_speed) in
        { stats with distance; average_speed }
    | LatLngStream s ->
        let start = List.hd_exn s.data in
        let start_latlng = Some (List.nth_exn start 0, List.nth_exn start 1) in
        let end_ = List.last_exn s.data in
        let end_latlng = Some (List.nth_exn end_ 0, List.nth_exn end_ 1) in
        { stats with start_latlng; end_latlng }
    | AltitudeStream s ->
        let smoothing_window = 5 in
        let results = ElevResult.compute smoothing_window s.data in

        {
          stats with
          elev_high = results.elev_high;
          elev_low = results.elev_low;
          elev_gain = results.elev_gain;
          elev_loss = results.elev_loss;
        }
    | VelocityStream s ->
        let max_speed = List.max_elt ~compare:Float.compare s.data in
        { stats with max_speed }
    | HeartRateStream s ->
        let data = s.data in
        let average_heartrate = Utils.average data in
        let max_heartrate = List.max_elt ~compare:Int.compare data in
        { stats with average_heartrate; max_heartrate }
    | CadenceStream s ->
        let data = s.data in
        let data = List.filter data ~f:(fun cad -> cad > 0) in
        let average_cadence = Utils.average data in

        let max_cadence = List.max_elt ~compare:Int.compare data in
        { stats with average_cadence; max_cadence }
    | TempStream s ->
        let data = s.data in
        let average_temp = Utils.average data in
        { stats with average_temp }
    | WattsStream s ->
        (* NOTE: the the example activity im using doesn't have power data -> test this code *)
        let data = s.data in
        let data = List.filter ~f:Option.is_some data in
        let data = List.map ~f:(fun power -> Option.value_exn power) data in
        let average_power = Utils.average data in
        let max_power = List.max_elt ~compare:Int.compare data in
        { stats with average_power; max_power }
    | GradeStream _ -> stats
end

(* NOTE: here is a m/s to min/km converter:  *)
(* https://www.unitjuggler.com/convert-speed-from-ms-to-minkm.html?val=2.257 *)

module Streams = struct
  type t = StreamType.t list [@@deriving show { with_path = false }, yojson]

  let stats (streams : t) : Stats.t =
    List.fold ~init:(Stats.empty ()) ~f:StreamType.stats_of_t streams
end

module Lap = struct
  type t = { start_index : int; end_index : int; lap_index : int }
  [@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

  let empty () = { start_index = 0; end_index = 10; lap_index = 0 }
end

(* TODO: should i calculate splits here, i.e. for every 1km? because laps can differ from splits? *)

module Laps = struct
  type t = Lap.t list [@@deriving show { with_path = false }, yojson]

  let empty () : t = [ Lap.empty () ]
end

let _athlete_info token =
  let url = "https://www.strava.com/api/v3/athlete" in
  let headers = [ ("Authorization", sprintf "Bearer %s" token) ] in
  let res = Ezcurl.get ~headers ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> s in
  out

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

module Activity = struct
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
end

(* TODO: Create a custom activity class that has all the properties calculated from the streams *)

let list_activities token n =
  let url = Uri.of_string "https://www.strava.com/api/v3/athlete/activities" in
  let url = Uri.add_query_param url ("page", [ "1" ]) in
  let url = Uri.add_query_param url ("per_page", [ Int.to_string n ]) in
  let url = Uri.to_string url in

  let headers = [ ("Authorization", sprintf "Bearer %s" token) ] in
  let res = Ezcurl.get ~headers ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> s in
  out

(* NOTE: this returns 280KB of data for a single activity (because we're *)
(* downloading all of the streams) *)
let get_streams token activity_id =
  let url =
    Uri.of_string
    @@ sprintf "https://www.strava.com/api/v3/activities/%d/streams" activity_id
  in
  let url =
    Uri.add_query_param url
      ( "keys",
        [
          "time";
          "distance";
          "latlng";
          "altitude";
          "velocity_smooth";
          "heartrate";
          "cadence";
          "watts";
          "temp";
          "grade_smooth";
        ] )
  in
  let url = Uri.to_string url in

  let headers = [ ("Authorization", sprintf "Bearer %s" token) ] in
  let res = Ezcurl.get ~headers ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> s in
  out

let get_laps token activity_id =
  let url =
    sprintf "https://www.strava.com/api/v3/activities/%d/laps" activity_id
  in

  let headers = [ ("Authorization", sprintf "Bearer %s" token) ] in
  let res = Ezcurl.get ~headers ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> s in
  out

let pull_activities token num_activities =
  let activities = list_activities token num_activities in
  print_endline activities

(* TODO: also pull_lap_times for the activities *)

let pull_streams_aux token activity_id =
  let resp = get_streams token activity_id in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  Or_error.try_with (fun () -> Streams.t_of_yojson json)

(* TODO: try with should raise a specific error otherwise i can't figure out what went wrong *)
let pull_streams token activity_id =
  let%bind streams = pull_streams_aux token activity_id in
  let filename = sprintf "streams_%d.json" activity_id in
  Yojson.Safe.to_file filename (Streams.yojson_of_t streams);
  printf "Saved streams to file %s\n" filename;
  Ok ()

let pull_laps token activity_id =
  let resp = get_laps token activity_id in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let%bind laps = Or_error.try_with (fun () -> Laps.t_of_yojson json) in
  let filename = sprintf "laps_%d.json" activity_id in
  Yojson.Safe.to_file filename (Laps.yojson_of_t laps);
  printf "Saved laps to file %s\n" filename;
  Ok ()

let process_activities token num_activities =
  let resp = list_activities token num_activities in
  let timestamp = Time_ns.now () |> Time_ns.to_string in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let filename = sprintf "%s_activities.json" timestamp in
  Yojson.Safe.to_file filename json;
  printf "Saved activities to file %s\n" filename;
  let%bind strava_activities =
    Or_error.try_with (fun () -> StravaActivities.t_of_yojson json)
  in
  let activities = List.map ~f:Activity.t_of_StravaActivity strava_activities in
  let streams =
    List.map
      ~f:(fun activity ->
        printf "downloading streams for activity=%d\n" activity.id;
        match pull_streams_aux token activity.id with
        | Ok s ->
            printf "successfully downloaded streams for activity=%d\n"
              activity.id;
            s
        | Error e -> Error.raise e)
      activities
  in
  (* let%bind streams = Or_error.combine_errors streams in *)
  let stats = List.map ~f:Streams.stats streams in
  let out = [%yojson_of: Stats.t list] stats in
  let filename = sprintf "%s_stats.json" timestamp in
  Yojson.Safe.to_file filename out;
  printf "Saved stats to file %s\n" filename;
  Ok ()

let%expect_test "deserialize get_stream.json" =
  let json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/get_streams_test.json"
  in
  let streams = Streams.t_of_yojson json in
  printf "%s" (Streams.show streams);
  [%expect
    {|
    [(TimeStream
        { type_ = "time";
          data =
          [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19;
            20];
          series_type = "distance"; original_size = 21; resolution = "high" });
      (DistanceStream
         { type_ = "distance";
           data =
           [0.; 2.8; 5.5; 8.3; 11.; 13.8; 16.5; 19.3; 22.; 27.; 32.; 36.; 39.;
             42.; 45.; 48.; 51.; 54.; 57.; 60.; 64.];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (LatLngStream
         { type_ = "latlng";
           data =
           [[54.70304; 25.317412]; [54.703055; 25.317374];
             [54.703069; 25.317336]; [54.703084; 25.317297];
             [54.703099; 25.317259]; [54.703113; 25.317221];
             [54.703128; 25.317183]; [54.703143; 25.317144];
             [54.703158; 25.317106]; [54.703175; 25.31704];
             [54.70319; 25.316983]; [54.703203; 25.316937]; [54.70321; 25.31689];
             [54.703218; 25.316848]; [54.703228; 25.3168];
             [54.703233; 25.316753]; [54.70324; 25.31671];
             [54.703245; 25.316665]; [54.703255; 25.31662];
             [54.703265; 25.316572]; [54.703273; 25.316523]];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (AltitudeStream
         { type_ = "altitude";
           data =
           [115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 114.6;
             114.2; 114.; 113.8; 113.8; 113.4; 113.4; 113.2; 113.2; 112.8; 112.8;
             112.6];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (VelocityStream
         { type_ = "velocity_smooth";
           data =
           [0.; 0.; 2.75; 2.75; 2.75; 2.75; 2.75; 2.75; 2.75; 3.2; 3.65; 3.9;
             3.95; 4.; 3.6; 3.2; 3.; 3.; 3.; 3.; 3.2];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (HeartRateStream
         { type_ = "heartrate";
           data =
           [80; 79; 78; 77; 77; 75; 75; 75; 76; 78; 80; 80; 82; 84; 87; 89; 91;
             95; 97; 99; 100];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (CadenceStream
         { type_ = "cadence";
           data =
           [84; 84; 84; 84; 84; 84; 84; 84; 84; 85; 86; 86; 86; 85; 84; 83; 82;
             82; 82; 82; 82];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (WattsStream
         { type_ = "watts";
           data =
           [None; None; None; None; None; None; None; None; (Some 283);
             (Some 243); (Some 279); (Some 273); (Some 263); (Some 261);
             (Some 219); (Some 220); (Some 201); (Some 209); (Some 193);
             (Some 200); (Some 198)];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (TempStream
         { type_ = "temp";
           data =
           [29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29;
             29; 29; 29; 29];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (GradeStream
         { type_ = "grade_smooth";
           data =
           [0.; 0.; 0.; 0.; 0.; 0.; 0.; -4.5; -6.5; -7.2; -8.2; -5.3; -6.2; -5.;
             -5.; -5.; -5.; -5.; -4.6; -4.6; -3.1];
           series_type = "distance"; original_size = 21; resolution = "high" })
      ]
    |}]

let%expect_test "deserialize running activity" =
  let json =
    Yojson.Safe.from_file "/home/angel/Documents/ocaml/unto/one_activity.json"
  in
  let activities = StravaActivities.t_of_yojson json in
  printf "%s" (StravaActivities.show activities);
  [%expect
    {|
    [{ athlete = { id = 3504239 }; name = "Afternoon Run"; sport_type = "Run";
       id = 14995177737; start_date_local = "2025-07-03T16:29:12Z";
       timezone = "(GMT+02:00) Europe/Vilnius";
       map =
       { id = "a14995177737";
         summary_polyline =
         "wmemIkqzyCiAtDIv@k@bC}@jC_AxFWt@Pm@lA}GpBiGlAuEHq@Tk@t@eHD}@]oHQaA_@}@WwBOkBGIMw@aBlBu@^cAbAg@|@_AhAo@dBu@bAe@hAg@t@oAhAqDXc@a@g@]q@QCNJJXEnAjADv@f@~@FfDpBbHv@|Af@`Db@VHz@Nd@ZVKgAb@}GCaDL}@D{BRkAR[\\}APER_@b@}AVcBlAmCb@o@ZQhAJGO_@KS_@WmAsAmEo@oAGk@Wu@o@Gw@RKZBl@QcAWC]k@kBuBo@oBEcAJu@GMe@[YJoAcAe@_ACOFo@XuAAo@FoAHC?SLm@Ay@I_@a@u@_@KKLz@jAH^KrAGbB`@mBDo@Me@?]ZcBj@aAPGDPRJFIq@~Ag@zA[jB[r@Kl@c@Da@d@JPFAI@Rh@?RMu@@QKUd@OV{@Bo@GYIE\\Sh@sB\\Ed@k@Jq@R_@t@D^Vz@i@nA@T_ATUl@qAHgAf@OFR~@\\FF@ZRc@fAy@P?TRbAELr@VGNZ^JeAz@g@MWv@TQrAPNW|@uCp@wCPK`B`BvAbBt@f@Zk@`AqD\\aCpBkGBwAJSVGf@TbAo@n@aCj@n@|@d@l@TZCj@^nBWjAuALw@NBPa@XIH_@\\a@VWZOTy@VoAXyBt@aCh@aAJc@T_Ch@yCZo@Nu@Ng@VE|@zAUiCGU]WmA\\eCEQJe@fABc@E{@_AgDs@e@QmAEaAw@kCEo@y@m@c@q@gBIo@g@o@Ob@}A|@k@Za@l@G~@o@dCCDd@MfBBrAE|@m@lAu@bAc@lCBdBGp@@~@b@rAl@`@i@Sk@_AmARw@a@eAmAaAUWg@?Z[hAAn@Hf@Mb@?XVr@n@p@n@pAIt@Bb@GpB[t@SjAMPAj@S~@EzAIFAh@DrA_@|@Mx@cAfAI\\WRMVO~Ak@nBS`@BdAEVU\\Op@cA^Ya@]^E[o@FELFRVPVv@h@ZNf@YnBqAnEUh@K~@dBzC\\z@PwACYQACMLQ@_@a@RQj@UF_A_BKAKVeA|F_@pAYVm@LG\\]n@Qr@JhAEf@jBnBXl@@pEDJ|@j@LZJfAuAHw@bAoBz@M?[c@YMeCZTlCXpBs@{E}@eMk@}C^rBx@xGX~E"
         };
       gear_id = (Some "g272465") }
      ] |}]

let%expect_test "deserialize crossfit activity" =
  let json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/crossfit_activity.json"
  in
  let activities = StravaActivities.t_of_yojson json in
  printf "%s" (StravaActivities.show activities);
  [%expect
    {|
    [{ athlete = { id = 3504239 }; name = "Lunch Crossfit";
       sport_type = "Crossfit"; id = 15086886566;
       start_date_local = "2025-07-12T11:49:53Z";
       timezone = "(GMT+03:00) Africa/Addis_Ababa";
       map = { id = "a15086886566"; summary_polyline = "" }; gear_id = None }
      ] |}]

let%expect_test "process_streams" =
  let json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/streams_14995177737.json"
  in
  let streams = Streams.t_of_yojson json in
  let stats = Streams.stats streams in
  printf "%s" (Stats.show stats);
  [%expect
    {|
    Smoothing equilibrium reached at depth=8
    { moving_time = 4888; elapsed_time = 5562; distance = (Some 11033.);
      elev_gain = (Some 108); elev_loss = (Some 103); elev_high = (Some 140);
      elev_low = (Some 110); start_latlng = (Some (54.755563, 25.37736));
      end_latlng = (Some (54.755553, 25.377283)); average_speed = (Some 2.258);
      max_speed = (Some 5.); average_cadence = (Some 75);
      max_cadence = (Some 99); average_temp = (Some 32);
      average_heartrate = (Some 169); max_heartrate = (Some 186);
      average_power = None; max_power = None } |}]

let%expect_test "altitude smoothing (exponential moving average)" =
  let data =
    [
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      114.6;
      114.2;
      114.0;
      113.8;
      113.8;
      113.4;
      113.4;
      113.2;
      113.2;
      112.8;
      112.8;
      112.6;
    ]
  in
  let ema = Utils.exponential_moving_average 0.2 data in
  List.iter ~f:(printf "%.2f ") ema;
  [%expect
    {| 115.20 115.20 115.20 115.20 115.20 115.20 115.20 115.20 115.20 115.08 114.90 114.72 114.54 114.39 114.19 114.03 113.87 113.73 113.55 113.40 113.24 |}]

let%expect_test "altitude smoothing (moving average)" =
  let data =
    [
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      115.2;
      114.6;
      114.2;
      114.0;
      113.8;
      113.8;
      113.4;
      113.4;
      113.2;
      113.2;
      112.8;
      112.8;
      112.6;
    ]
  in
  let ma = Utils.moving_average (module Utils.FloatOps) 5 data in
  List.iter ~f:(printf "%.2f ") ma;
  [%expect
    {| 115.20 115.20 115.20 115.20 115.14 115.05 114.95 114.82 114.69 114.53 114.36 114.18 114.00 113.78 113.56 113.38 113.30 113.22 113.15 113.06 113.00 |}]

let%expect_test "sportType of string" =
  let sport_str = "Run" in
  let sport = sportType_of_string sport_str in
  let success = match sport with Run -> true | _ -> false in
  printf "%b" success;
  [%expect {| true |}]

(* let%expect_test "write raw altitude csv" = *)
(*   let json = *)
(*     Yojson.Safe.from_file *)
(*       "/home/angel/Documents/ocaml/unto/streams_14995177737.json" *)
(*   in *)
(*   let streams = Streams.t_of_yojson json in *)
(*   List.iter streams ~f:(fun stream -> *)
(*       match stream with *)
(*       | AltitudeStream s -> *)
(*           let raw_data = s.data in *)
(*           let smoothed_ma = Utils.repeat_smoothing 8 5 s.data in *)
(*           let smoothed_int = List.map ~f:Int.of_float smoothed_ma in *)
(*           let out_raw = *)
(*             List.foldi ~init:"time,raw,8x_ma_5,8x_ma_5_int\n" *)
(*               ~f:(fun i acc alt -> *)
(*                 sprintf "%s%d,%f,%f,%d\n" acc i alt *)
(*                   (List.nth_exn smoothed_ma i) *)
(*                   (List.nth_exn smoothed_int i)) *)
(*               raw_data *)
(*           in *)
(*           Out_channel.write_all "/home/angel/Documents/ocaml/unto/alt5.csv" *)
(*             ~data:out_raw *)
(*       | _ -> ()); *)
(*   [%expect {| a |}] *)
