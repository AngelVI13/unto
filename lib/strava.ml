open Core
open Import
open Streams
open Strava_api
open Strava_models
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Or_error.Let_syntax

(* TODO: use https://github.com/mmottl/sqlite3-ocaml for sqlite storage *)
(* some examples can be found here: https://github.com/patoline/patoline/blob/75fd8c928efc68d0aaa400d3a699a0e668c26c5f/permap/permap.ml#L43 *)

(* TODO: 1. define the strava API I'm using (with only the needed fields) *)
(* TODO: 2. Insert data to databases on every request if the data is not already there *)
(* TODO: 3. Visualize the data in a web ui *)
(* TODO: 4. Analyze the data *)

let pull_activities token num_activities =
  let activities = list_activities token num_activities in
  print_endline activities

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

let pull_laps_aux token activity_id =
  let resp = get_laps token activity_id in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  Or_error.try_with (fun () -> StravaLaps.t_of_yojson json)

let pull_laps token activity_id =
  let%bind laps = pull_laps_aux token activity_id in
  let filename = sprintf "laps_%d.json" activity_id in
  Yojson.Safe.to_file filename (StravaLaps.yojson_of_t laps);
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
  let activities =
    List.map
      ~f:(fun activity ->
        printf "processing activity=%d\n" activity.id;
        printf "downloading streams\n";
        let streams = pull_streams_aux token activity.id in
        printf "downloading laps\n";
        let laps = pull_laps_aux token activity.id in
        match (streams, laps) with
        | Ok streams, Ok laps ->
            printf "successfully downloaded laps & streams\n";
            let laps = List.map ~f:Lap.t_of_StravaLap laps in
            printf "calculating stats\n";
            Activity.calculate_stats activity streams laps
        | Error e, Ok _ -> Error.raise e
        | Ok _, Error e -> Error.raise e
        | Error e1, Error e2 ->
            failwith
              (sprintf "Errors:\n%s\n%s\n" (Error.to_string_hum e1)
                 (Error.to_string_hum e2)))
      activities
  in
  (* let%bind streams = Or_error.combine_errors streams in *)
  (* let stats = *)
  (*   List.map ~f:(fun a -> Field.get Activity.Fields.stats a) activities *)
  (* in *)
  let out = [%yojson_of: Activity.t list] activities in
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
  let stats = Streams.activity_stats streams in
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
