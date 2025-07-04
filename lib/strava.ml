open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* TODO: use https://github.com/mmottl/sqlite3-ocaml for sqlite storage *)
(* some examples can be found here: https://github.com/patoline/patoline/blob/75fd8c928efc68d0aaa400d3a699a0e668c26c5f/permap/permap.ml#L43 *)

(* TODO: 1. define the strava API I'm using (with only the needed fields) *)
(* TODO: 2. Insert data to databases on every request if the data is not already there *)
(* TODO: 3. Visualize the data in a web ui *)
(* TODO: 4. Analyze the data *)

module Stream = struct
  type 'a t = {
    type_ : string; [@key "type"]
    data : 'a list;
    series_type : string;
    original_size : int;
    resolution : string;
  }
  [@@deriving show { with_path = false }, yojson]

  let int_example () =
    {
      type_ = "int_example";
      data = [ 1; 2; 3; 4; 5 ];
      series_type = "distance";
      original_size = 5;
      resolution = "high";
    }

  let float_example () =
    {
      type_ = "float_example";
      data = [ 1.0; 2.1; 3.2; 4.3; 5.4 ];
      series_type = "distance";
      original_size = 5;
      resolution = "high";
    }
end

type streamType =
  | IntStream of int Stream.t
  | IntOptStream of int option Stream.t
  | FloatStream of float Stream.t
  | TupleStream of float list Stream.t
[@@deriving show { with_path = false }, yojson_of]

let streamType_of_yojson (json : Yojson.Safe.t) : streamType =
  match Yojson.Safe.Util.member "data" json with
  (* this matches lists that have null and int into an IntOptStream *)
  | `List l
    when List.exists ~f:(Poly.( = ) `Null) l
         && List.exists l ~f:(function `Int _ -> true | _ -> false) ->
      IntOptStream (Stream.t_of_yojson [%of_yojson: int option] json)
  | `List (`Int _ :: _) -> IntStream (Stream.t_of_yojson [%of_yojson: int] json)
  | `List (`Float _ :: _) ->
      FloatStream (Stream.t_of_yojson [%of_yojson: float] json)
  | `List (`List (`Float _ :: [ `Float _ ]) :: _) ->
      TupleStream (Stream.t_of_yojson [%of_yojson: float list] json)
  | _ -> failwith "unsupported"

module Streams = struct
  type t = streamType list [@@deriving show { with_path = false }, yojson]

  let example () =
    [ IntStream (Stream.int_example ()); FloatStream (Stream.float_example ()) ]
end

let _athlete_info token =
  let url = "https://www.strava.com/api/v3/athlete" in
  let headers = [ ("Authorization", sprintf "Bearer %s" token) ] in
  let res = Ezcurl.get ~headers ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> s in
  out

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

let%expect_test "serialize example streams object" =
  let streams = Streams.example () in
  let json_steams = Streams.yojson_of_t streams in
  let json_obj = Yojson.Safe.to_basic json_steams in
  printf "%s" (Yojson.Safe.to_string json_steams);
  [%expect
    {| [["IntStream",{"type":"int_example","data":[1,2,3,4,5],"series_type":"distance","original_size":5,"resolution":"high"}],["FloatStream",{"type":"float_example","data":[1.0,2.1,3.2,4.3,5.4],"series_type":"distance","original_size":5,"resolution":"high"}]] |}];
  printf "%s" (Yojson.Basic.to_string json_obj);
  [%expect
    {| [["IntStream",{"type":"int_example","data":[1,2,3,4,5],"series_type":"distance","original_size":5,"resolution":"high"}],["FloatStream",{"type":"float_example","data":[1.0,2.1,3.2,4.3,5.4],"series_type":"distance","original_size":5,"resolution":"high"}]] |}]

let%expect_test "deserialize get_stream.json" =
  let json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/get_streams_test.json"
  in
  let streams = Streams.t_of_yojson json in
  printf "%s" (Streams.show streams);
  [%expect
    {|
    [(IntStream
        { type_ = "time";
          data =
          [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19;
            20];
          series_type = "distance"; original_size = 21; resolution = "high" });
      (FloatStream
         { type_ = "distance";
           data =
           [0.; 2.8; 5.5; 8.3; 11.; 13.8; 16.5; 19.3; 22.; 27.; 32.; 36.; 39.;
             42.; 45.; 48.; 51.; 54.; 57.; 60.; 64.];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (TupleStream
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
      (FloatStream
         { type_ = "altitude";
           data =
           [115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 114.6;
             114.2; 114.; 113.8; 113.8; 113.4; 113.4; 113.2; 113.2; 112.8; 112.8;
             112.6];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (FloatStream
         { type_ = "velocity_smooth";
           data =
           [0.; 0.; 2.75; 2.75; 2.75; 2.75; 2.75; 2.75; 2.75; 3.2; 3.65; 3.9;
             3.95; 4.; 3.6; 3.2; 3.; 3.; 3.; 3.; 3.2];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (IntStream
         { type_ = "heartrate";
           data =
           [80; 79; 78; 77; 77; 75; 75; 75; 76; 78; 80; 80; 82; 84; 87; 89; 91;
             95; 97; 99; 100];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (IntStream
         { type_ = "cadence";
           data =
           [84; 84; 84; 84; 84; 84; 84; 84; 84; 85; 86; 86; 86; 85; 84; 83; 82;
             82; 82; 82; 82];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (IntOptStream
         { type_ = "watts";
           data =
           [None; None; None; None; None; None; None; None; (Some 283);
             (Some 243); (Some 279); (Some 273); (Some 263); (Some 261);
             (Some 219); (Some 220); (Some 201); (Some 209); (Some 193);
             (Some 200); (Some 198)];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (IntStream
         { type_ = "temp";
           data =
           [29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29;
             29; 29; 29; 29];
           series_type = "distance"; original_size = 21; resolution = "high" });
      (FloatStream
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
