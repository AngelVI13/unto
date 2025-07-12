open Core
open Elevation
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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
[@@deriving yojson, show { with_path = false }]

module ActivityStats = struct
  type t = {
    moving_time : int;
    elapsed_time : int;
    distance : float option;
    elev_gain : float option;
    elev_loss : float option;
    elev_high : float option;
    elev_low : float option;
    start_latlng : (float * float) option;
    end_latlng : (float * float) option;
    average_speed : float option;
    max_speed : float option;
    average_cadence : float option;
    average_temp : int option;
    average_heartrate : float option;
    max_heartrate : float option;
  }
  [@@deriving show { with_path = false }]

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
      average_temp = None;
      average_heartrate = None;
      max_heartrate = None;
    }
end

module Activity = struct
  type t = {
    id : string;
    athlete_id : string;
    name : string;
    sport_type : sportType;
    start_date : string;
    timezone : string;
    map_id : string;
    map_summary_polyline : string;
    (* NOTE: these are calculated from the data streams of the
       activity and not taken from strava directly *)
    stats : ActivityStats.t;
  }
  [@@deriving show { with_path = false }]
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

  let stats_of_t (stats : ActivityStats.t) (stream : t) : ActivityStats.t =
    match stream with
    | TimeStream s ->
        let elapsed_time = List.last_exn s.data - 1 in
        let moving_time = List.length s.data - 2 in
        { stats with elapsed_time; moving_time }
    | DistanceStream s ->
        let distance = Some (List.last_exn s.data) in
        { stats with distance }
    | LatLngStream s ->
        let start = List.hd_exn s.data in
        let start_latlng = Some (List.nth_exn start 0, List.nth_exn start 1) in
        let end_ = List.last_exn s.data in
        let end_latlng = Some (List.nth_exn end_ 0, List.nth_exn end_ 1) in
        { stats with start_latlng; end_latlng }
    | AltitudeStream s ->
        (* let smoothed = Utils.exponential_moving_average 0.20 s.data in *)
        let smoothing_window = 5 in
        let smoothed = Utils.moving_average smoothing_window s.data in
        let compute_fn = ElevResult.compute smoothed in
        let results =
          List.foldi ~init:(ElevResult.empty ()) ~f:compute_fn smoothed
        in
        {
          stats with
          elev_high = results.elev_high;
          elev_low = results.elev_low;
          elev_gain = results.elev_gain;
          elev_loss = results.elev_loss;
        }
    | _ -> stats
  (* total_elevation_gain = None; *)
  (* average_speed = None; *)
  (* max_speed = None; *)
  (* average_cadence = None; *)
  (* average_temp = None; *)
  (* average_heartrate = None; *)
  (* max_heartrate = None; *)
end

(* NOTE: this was translated from this:
  https://github.com/gpxstudio/gpx.studio/blob/a9ea0e223d925f964c274deb4d558d88f1246f3c/gpx/src/gpx.ts#L1901
  *)
let distance (coord1 : float list) (coord2 : float list) =
  let open Float in
  let earth_radius = 6371008.8 in
  let rad = pi / 180.0 in
  let lat1 = List.nth_exn coord1 0 in
  let lat1 = lat1 * rad in
  let lat2 = List.nth_exn coord2 0 in
  let lat2 = lat2 * rad in
  let lon1 = List.nth_exn coord1 1 in
  let lon2 = List.nth_exn coord2 1 in
  let a =
    (sin lat1 * sin lat2) + (cos lat1 * cos lat2 * cos ((lon2 - lon1) * rad))
  in
  let max_meters = earth_radius * acos (min a 1.0) in
  max_meters

module Streams = struct
  type t = StreamType.t list [@@deriving show { with_path = false }, yojson]

  let stats (streams : t) : ActivityStats.t =
    List.fold ~init:(ActivityStats.empty ()) ~f:StreamType.stats_of_t streams
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
    (* TODO: convert to correct sport type and not just string *)
    (* sport_type : sportType; *)
    sport_type : string;
    id : int;
    start_date_local : string;
    timezone : string;
    map : StravaPolylineMap.t;
    gear_id : string;
  }
  [@@deriving yojson, show { with_path = false }] [@@yojson.allow_extra_fields]
end

module StravaActivities = struct
  type t = StravaActivity.t list [@@deriving yojson, show { with_path = false }]
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

let pull_activities token num_activities =
  let activities = list_activities token num_activities in
  print_endline activities

(* TODO: also pull_lap_times for the activities *)

(* TODO: try with should raise a specific error otherwise i can't figure out what went wrong *)
let pull_streams token activity_id =
  let open Or_error.Let_syntax in
  let resp = get_streams token activity_id in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let%bind _ = Or_error.try_with (fun () -> Streams.t_of_yojson json) in
  let filename = sprintf "streams_%d.json" activity_id in
  Yojson.Safe.to_file filename json;
  printf "Saved streams to file %s\n" filename;
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

let%expect_test "deserialize activity" =
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
       gear_id = "g272465" }
      ] |}]

let%expect_test "process_streams" =
  let json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/streams_14995177737.json"
  in
  let streams = Streams.t_of_yojson json in
  let stats = Streams.stats streams in
  printf "%s" (ActivityStats.show stats);
  [%expect
    {|
    { moving_time = 4887; elapsed_time = 5561; distance = (Some 11033.);
      elev_gain = (Some 102.6); elev_loss = (Some 96.0363636364);
      elev_high = (Some 140.927272727); elev_low = (Some 110.054545455);
      start_latlng = (Some (54.755563, 25.37736));
      end_latlng = (Some (54.755553, 25.377283)); average_speed = None;
      max_speed = None; average_cadence = None; average_temp = None;
      average_heartrate = None; max_heartrate = None } |}]

let%expect_test "distance by coords" =
  let coord1 = [ 54.755563; 25.37736 ] in
  let coord2 = [ 54.755523; 25.377214 ] in
  let distance = distance coord1 coord2 in
  printf "%f" distance;
  [%expect {|
    10.370148 |}]

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
  let ma = Utils.moving_average 5 data in
  List.iter ~f:(printf "%.2f ") ma;
  [%expect
    {| 115.20 115.20 115.20 115.20 115.14 115.05 114.95 114.82 114.69 114.53 114.36 114.18 114.00 113.78 113.56 113.38 113.30 113.22 113.15 113.06 113.00 |}]

(* TODO: implement the gpx studio algorithm and compare with these simple approaches *)
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
(*           let smoothed_ma = Utils.moving_average 5 s.data in *)
(*           (* NOTE: The exponential moving average seems to be closest to suunto but strava graph is way too flat. 0.15 looks good, try 0.10 or 0.7 or 0.5 *) *)
(*           (* NOTE: The moving average also ok for values of 20-ish, maybe try 15 *) *)
(*           let smoothed_ema = Utils.exponential_moving_average 0.20 s.data in *)
(*           let out_raw = *)
(*             List.foldi ~init:"time,raw,ma_30,ema_015\n" *)
(*               ~f:(fun i acc alt -> *)
(*                 sprintf "%s\n%d,%f,%f,%f" acc i alt *)
(*                   (List.nth_exn smoothed_ma i) *)
(*                   (List.nth_exn smoothed_ema i)) *)
(*               raw_data *)
(*           in *)
(*           Out_channel.write_all "/home/angel/Documents/ocaml/unto/alt4.csv" *)
(*             ~data:out_raw *)
(*       | _ -> ()); *)
(*   [%expect {| a |}] *)
