open Core
open Laps
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

(* NOTE: *)
(* Expected tables: *)
(*   1. activity table  *)
(*   2. activity stats table (with activity id as primary key) *)
(*   3. lap table (with activity id as primary key) *)
(*   4. lap stats table (with lap id as primary key) *)
(*   5. splits table (with activity id as primary key) *)
(*   6. splits stats table (with splits id as primary key) *)
(*   7. streams table (with activity id as primary key) - this table only  *)
(*     stores filenames where the streams data is actually stored *)
(* Splits and laps can be found by activity ID an lap/split index, i don't
   think i need to create a separate lap/split id for each one. *)

(* TODO: 3. Visualize the data in a web ui *)
(* TODO: 4. Analyze the data *)

let pull_activities token num_activities =
  let activities = list_activities ~token ~page:1 ~per_page:num_activities () in
  print_endline activities

let pull_streams_aux token activity_id =
  let resp = get_streams token activity_id in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let filename = sprintf "raw_streams_%d.json" activity_id in
  Yojson.Safe.to_file filename json;
  Or_error.try_with (fun () -> Streams.t_of_yojson_smoothed json)

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
  Yojson.Safe.to_file (sprintf "raw_laps_%d.json" activity_id) json;
  Or_error.try_with (fun () -> StravaLaps.t_of_yojson json)

let pull_laps token activity_id =
  let%bind laps = pull_laps_aux token activity_id in
  let filename = sprintf "laps_%d.json" activity_id in
  Yojson.Safe.to_file filename (StravaLaps.yojson_of_t laps);
  printf "Saved laps to file %s\n" filename;
  Ok ()

let process_user token =
  let resp = athlete_info token in
  let timestamp = Time_ns.now () |> Time_ns.to_string in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let filename = sprintf "%s_raw_user.json" timestamp in
  Yojson.Safe.to_file filename json;
  printf "Saved raw user info to file %s\n" filename;
  let%bind strava_athlete =
    Or_error.try_with (fun () -> StravaAthlete.t_of_yojson json)
  in
  let out = [%yojson_of: StravaAthlete.t] strava_athlete in
  let filename = sprintf "%s_user.json" timestamp in
  Yojson.Safe.to_file filename out;
  printf "Saved user info to file %s\n" filename;
  Ok ()

(* NOTE: I don't know where strava gets these zones., they are not the same
   as my suunto and not the same as what i had entered manually before. For
   now we ignore this data and maybe compute it ourselves *)
let process_zones token =
  let resp = zones token in
  let timestamp = Time_ns.now () |> Time_ns.to_string in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let filename = sprintf "%s_raw_zones.json" timestamp in
  Yojson.Safe.to_file filename json;
  printf "Saved raw zones info to file %s\n" filename;
  (* let%bind zones = Or_error.try_with (fun () -> StravaZones.t_of_yojson json) in *)
  (* let out = [%yojson_of: StravaZones.t] zones in *)
  (* let filename = sprintf "%s_zones.json" timestamp in *)
  (* Yojson.Safe.to_file filename out; *)
  (* printf "Saved zones info to file %s\n" filename; *)
  Ok ()

let process_activities token num_activities =
  let resp = list_activities ~token ~page:1 ~per_page:num_activities () in
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
            let laps = Laps.t_of_StravaLaps laps in
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
  let out = [%yojson_of: Activity.t list] activities in
  let filename = sprintf "%s_stats.json" timestamp in
  Yojson.Safe.to_file filename out;
  printf "Saved stats to file %s\n" filename;
  Ok ()

let fetch_one_page ~token ~page ~per_page ~exclude =
  printf "downloading activity page %d (per_page=%d)\n" page per_page;
  let resp = list_activities ~token ~page ~per_page () in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let%bind strava_activities =
    Or_error.try_with (fun () -> StravaActivities.t_of_yojson json)
  in
  let activities = List.map ~f:Activity.t_of_StravaActivity strava_activities in
  let activities =
    List.filter
      ~f:(fun activity ->
        match List.exists ~f:(Int.equal activity.id) exclude with
        | true ->
            printf "...skipping activity (%d) - already exists\n" activity.id;
            false
        | false -> true)
      activities
  in
  let activities =
    List.map
      ~f:(fun activity ->
        Time_ns.pause (Time_ns.Span.create ~ms:100 ());
        printf "\tprocessing activity=%d\n" activity.id;
        printf "\t\tdownloading streams\n";
        let streams = pull_streams_aux token activity.id in
        printf "\t\tdownloading laps\n";
        let laps = pull_laps_aux token activity.id in
        match (streams, laps) with
        | Ok streams, Ok laps ->
            printf "\t\tsuccessfully downloaded laps & streams\n";
            let laps = Laps.t_of_StravaLaps laps in
            printf "\t\tcalculating stats\n";
            Activity.calculate_stats activity streams laps
        | Error e, Ok _ -> Error.raise e
        | Ok _, Error e -> Error.raise e
        | Error e1, Error e2 ->
            failwith
              (sprintf "Errors:\n%s\n%s\n" (Error.to_string_hum e1)
                 (Error.to_string_hum e2)))
      activities
  in
  Ok activities

let fetch_activities ~token ~num_activities ~exclude =
  let per_page = min num_activities 100 in
  let pages = Float.(round_up (of_int num_activities /. of_int per_page)) in
  let pages = Int.of_float pages in
  let activities =
    List.init pages ~f:(fun i ->
        fetch_one_page ~token ~page:(i + 1) ~per_page ~exclude)
  in
  let%bind activities = Or_error.all activities in
  let activities = List.join activities in
  Ok activities

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
          smoothed = <opaque>; series_type = "distance"; original_size = 21;
          resolution = "high" });
      (DistanceStream
         { type_ = "distance";
           data =
           [0.; 2.8; 5.5; 8.3; 11.; 13.8; 16.5; 19.3; 22.; 27.; 32.; 36.; 39.;
             42.; 45.; 48.; 51.; 54.; 57.; 60.; 64.];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
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
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (AltitudeStream
         { type_ = "altitude";
           data =
           [115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 114.6;
             114.2; 114.; 113.8; 113.8; 113.4; 113.4; 113.2; 113.2; 112.8; 112.8;
             112.6];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (VelocityStream
         { type_ = "velocity_smooth";
           data =
           [0.; 0.; 2.75; 2.75; 2.75; 2.75; 2.75; 2.75; 2.75; 3.2; 3.65; 3.9;
             3.95; 4.; 3.6; 3.2; 3.; 3.; 3.; 3.; 3.2];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (HeartRateStream
         { type_ = "heartrate";
           data =
           [80; 79; 78; 77; 77; 75; 75; 75; 76; 78; 80; 80; 82; 84; 87; 89; 91;
             95; 97; 99; 100];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (CadenceStream
         { type_ = "cadence";
           data =
           [84; 84; 84; 84; 84; 84; 84; 84; 84; 85; 86; 86; 86; 85; 84; 83; 82;
             82; 82; 82; 82];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (WattsStream
         { type_ = "watts";
           data =
           [None; None; None; None; None; None; None; None; (Some 283);
             (Some 243); (Some 279); (Some 273); (Some 263); (Some 261);
             (Some 219); (Some 220); (Some 201); (Some 209); (Some 193);
             (Some 200); (Some 198)];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (TempStream
         { type_ = "temp";
           data =
           [29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29;
             29; 29; 29; 29];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (GradeStream
         { type_ = "grade_smooth";
           data =
           [0.; 0.; 0.; 0.; 0.; 0.; 0.; -4.5; -6.5; -7.2; -8.2; -5.3; -6.2; -5.;
             -5.; -5.; -5.; -5.; -4.6; -4.6; -3.1];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" })
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
  let streams = Streams.t_of_yojson_smoothed json in
  let stats = Streams.activity_stats streams in
  printf "%s" (Stats.show stats);
  [%expect
    {|
    Smoothing equilibrium reached at depth=8
    { data_points = 4889; moving_time = 4888; elapsed_time = 5562;
      distance = (Some 11033.); elev_gain = (Some 108); elev_loss = (Some 103);
      elev_high = (Some 140); elev_low = (Some 110);
      start_latlng = (Some (54.755563, 25.37736));
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

let%expect_test "all stats" =
  let streams_json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/raw_streams_15145174551.json"
  in
  let streams = Streams.t_of_yojson_smoothed streams_json in

  let laps_json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/raw_laps_15145174551.json"
  in
  let laps = StravaLaps.t_of_yojson laps_json in
  let laps = Laps.t_of_StravaLaps laps in

  let json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/raw_activity_15145174551.json"
  in
  let activity = List.nth_exn (StravaActivities.t_of_yojson json) 0 in
  let activity = Activity.t_of_StravaActivity activity in
  let activity = Activity.calculate_stats activity streams laps in
  (* let activity_json = Activity.yojson_of_t activity in *)
  (* Yojson.Safe.to_file *)
  (*   "/home/angel/Documents/ocaml/unto/processed_15145174551.json" activity_json; *)
  printf "%s" (Activity.show activity);
  [%expect
    {|
    Smoothing equilibrium reached at depth=7
    { id = 15145174551; athlete_id = 3504239; name = "Afternoon Run";
      sport_type = Run; start_date = "2025-07-17T16:41:32Z";
      timezone = "(GMT+02:00) Europe/Vilnius"; map_id = "a15145174551";
      map_summary_polyline =
      "m{gmIqhqyClBAnADj@LnAO\\QTa@`As@y@r@OX_@RiATsCIaBA{@f@i@fAeAG_@cAq@qA_A{Bc@yA^UZe@P[Pw@RqCTo@Ce@RcCx@}@t@_Bt@k@dAOd@Xt@EhA\\RGPQTs@HsA@MF@VVFf@N^LN\\FXd@n@J`@]n@PfC|BnA\\DbBKb@?rAi@jAKj@Hc@d@gA?wAJy@MgCLm@Ei@JmACqANcAWk@IiAJ`@?TIv@Al@KNIdA{@jA]Ds@b@uAo@c@Xk@AcAN}AO[VCd@{@~@Kf@Bt@L^hBv@l@IbAv@j@FTNlAbCj@hBzB|Fx@bB\\fAhAlCV`@\\tAnDtHf@bBG|AHlAx@vBi@j@cAlCc@f@c@vB?jAKtAMb@?b@i@fAi@t@KZYGaA|@IQOcBToAFu@PKFWh@UFMPFPU@[f@`@@n@Tb@Mj@Cb@\\dAL`CNJ`@MXRl@DLP\\HL^D|@~@C~@\\l@p@xDeCz@[d@g@|AeCNc@D[G]Yi@ZAb@^L?pBcD\\yA^_E?a@[mC_@gBCe@TaDXq@\\YJARf@REP\\`Al@rBUPFLdCLj@@xATTCl@Lh@GNQNc@KGRYTi@N]VUKUBG~@D~AHTGTY]Wg@YeAa@_AEm@U]S}Ag@wBAqATiBSc@Cu@i@yBa@i@m@[k@gAsAo@OBy@mALv@Mh@D\\a@f@Fl@AtBMZC|@[xAOlDUz@EjBc@xAIx@eArAo@|AAPS\\y@T]Sg@VgAi@a@C]WMm@wAwD_AgBc@wASOcAdBYjAMTg@E{@d@o@EeAaD_@s@y@aAOBb@}A`@mBG{EhAgBlCiF`DsE|@kBFq@AeBDa@MKKu@UK\\EZNJVJ`BXfArAZZXTCRLLXX_@HBBTWNMj@Hz@Kf@Ez@l@lC@XZb@t@XzB?V\\BZKFSh@KB[x@u@VMXu@RiAqAiBeF}AuCe@m@}AyCiA{@{Bi@cB\\aAh@cA~@q@PEXL|CJX^`@]`AUNCRW`@OBK]?wAy@}Ag@[[m@s@e@s@mA]Ua@w@OGOe@w@Y]m@sAmAMs@_@O{@f@D@Ct@u@n@A\\Pl@RVVnBj@pBPdAz@vBf@hBjArCkA_CeBwF";
      stats =
      { data_points = 4615; moving_time = 4614; elapsed_time = 4719;
        distance = (Some 10604.); elev_gain = (Some 146); elev_loss = (Some 145);
        elev_high = (Some 150); elev_low = (Some 112);
        start_latlng = (Some (54.769788, 25.326307));
        end_latlng = (Some (54.77022, 25.326757)); average_speed = (Some 2.299);
        max_speed = (Some 6.); average_cadence = (Some 75);
        max_cadence = (Some 98); average_temp = (Some 26);
        average_heartrate = (Some 166); max_heartrate = (Some 180);
        average_power = None; max_power = None };
      laps =
      [{ moving_time = 505; start = 0; len = 505; lap_index = 1;
         stats =
         { data_points = 505; moving_time = 504; elapsed_time = 504;
           distance = (Some 1027.); elev_gain = (Some 10); elev_loss = (Some 10);
           elev_high = (Some 149); elev_low = (Some 142);
           start_latlng = (Some (54.769788, 25.326307));
           end_latlng = (Some (54.769812, 25.32632));
           average_speed = (Some 2.042); max_speed = (Some 3.8);
           average_cadence = (Some 80); max_cadence = (Some 81);
           average_temp = (Some 29); average_heartrate = (Some 142);
           max_heartrate = (Some 153); average_power = None; max_power = None }
         };
        { moving_time = 3753; start = 505; len = 3753; lap_index = 2;
          stats =
          { data_points = 3753; moving_time = 3752; elapsed_time = 3752;
            distance = (Some 8872.); elev_gain = (Some 127);
            elev_loss = (Some 127); elev_high = (Some 150);
            elev_low = (Some 112); start_latlng = (Some (54.769812, 25.32632));
            end_latlng = (Some (54.769815, 25.3267));
            average_speed = (Some 2.365); max_speed = (Some 6.);
            average_cadence = (Some 74); max_cadence = (Some 98);
            average_temp = (Some 26); average_heartrate = (Some 170);
            max_heartrate = (Some 180); average_power = None; max_power = None }
          };
        { moving_time = 355; start = 4258; len = 355; lap_index = 3;
          stats =
          { data_points = 357; moving_time = 356; elapsed_time = 461;
            distance = (Some 705.); elev_gain = (Some 8); elev_loss = (Some 8);
            elev_high = (Some 146); elev_low = (Some 143);
            start_latlng = (Some (54.769838, 25.326683));
            end_latlng = (Some (54.77022, 25.326757));
            average_speed = (Some 1.986); max_speed = (Some 4.);
            average_cadence = (Some 79); max_cadence = (Some 82);
            average_temp = (Some 27); average_heartrate = (Some 150);
            max_heartrate = (Some 178); average_power = None; max_power = None }
          }
        ];
      splits =
      (Some [{ split_index = 0; start = 0; len = 495;
               stats =
               { data_points = 495; moving_time = 494; elapsed_time = 494;
                 distance = (Some 1001.); elev_gain = (Some 10);
                 elev_loss = (Some 10); elev_high = (Some 149);
                 elev_low = (Some 142);
                 start_latlng = (Some (54.769788, 25.326307));
                 end_latlng = (Some (54.76971, 25.326038));
                 average_speed = (Some 2.03); max_speed = (Some 3.8);
                 average_cadence = (Some 80); max_cadence = (Some 81);
                 average_temp = (Some 29); average_heartrate = (Some 142);
                 max_heartrate = (Some 152); average_power = None;
                 max_power = None }
               };
              { split_index = 1; start = 495; len = 333;
                stats =
                { data_points = 333; moving_time = 332; elapsed_time = 332;
                  distance = (Some 997.); elev_gain = (Some 4);
                  elev_loss = (Some 16); elev_high = (Some 145);
                  elev_low = (Some 131);
                  start_latlng = (Some (54.769727, 25.326067));
                  end_latlng = (Some (54.764443, 25.327693));
                  average_speed = (Some 3.012); max_speed = (Some 5.8);
                  average_cadence = (Some 82); max_cadence = (Some 90);
                  average_temp = (Some 27); average_heartrate = (Some 163);
                  max_heartrate = (Some 171); average_power = None;
                  max_power = None }
                };
              { split_index = 2; start = 828; len = 405;
                stats =
                { data_points = 405; moving_time = 404; elapsed_time = 404;
                  distance = (Some 998.); elev_gain = (Some 8);
                  elev_loss = (Some 7); elev_high = (Some 135);
                  elev_low = (Some 127);
                  start_latlng = (Some (54.764423, 25.327732));
                  end_latlng = (Some (54.765352, 25.326905));
                  average_speed = (Some 2.476); max_speed = (Some 4.8);
                  average_cadence = (Some 74); max_cadence = (Some 86);
                  average_temp = (Some 25); average_heartrate = (Some 170);
                  max_heartrate = (Some 175); average_power = None;
                  max_power = None }
                };
              { split_index = 3; start = 1233; len = 375;
                stats =
                { data_points = 375; moving_time = 374; elapsed_time = 374;
                  distance = (Some 997.); elev_gain = (Some 15);
                  elev_loss = (Some 2); elev_high = (Some 147);
                  elev_low = (Some 134);
                  start_latlng = (Some (54.765352, 25.326905));
                  end_latlng = (Some (54.764235, 25.316138));
                  average_speed = (Some 2.673); max_speed = (Some 4.4);
                  average_cadence = (Some 77); max_cadence = (Some 83);
                  average_temp = (Some 25); average_heartrate = (Some 171);
                  max_heartrate = (Some 175); average_power = None;
                  max_power = None }
                };
              { split_index = 4; start = 1608; len = 460;
                stats =
                { data_points = 460; moving_time = 459; elapsed_time = 459;
                  distance = (Some 998.); elev_gain = (Some 5);
                  elev_loss = (Some 18); elev_high = (Some 149);
                  elev_low = (Some 133);
                  start_latlng = (Some (54.764235, 25.316138));
                  end_latlng = (Some (54.75836, 25.31797));
                  average_speed = (Some 2.179); max_speed = (Some 5.6);
                  average_cadence = (Some 69); max_cadence = (Some 98);
                  average_temp = (Some 26); average_heartrate = (Some 163);
                  max_heartrate = (Some 173); average_power = None;
                  max_power = None }
                };
              { split_index = 5; start = 2068; len = 449;
                stats =
                { data_points = 449; moving_time = 448; elapsed_time = 448;
                  distance = (Some 997.7); elev_gain = (Some 13);
                  elev_loss = (Some 19); elev_high = (Some 132);
                  elev_low = (Some 118);
                  start_latlng = (Some (54.758343, 25.318051));
                  end_latlng = (Some (54.75852, 25.320668));
                  average_speed = (Some 2.232); max_speed = (Some 4.6);
                  average_cadence = (Some 71); max_cadence = (Some 82);
                  average_temp = (Some 26); average_heartrate = (Some 170);
                  max_heartrate = (Some 174); average_power = None;
                  max_power = None }
                };
              { split_index = 6; start = 2517; len = 556;
                stats =
                { data_points = 556; moving_time = 555; elapsed_time = 555;
                  distance = (Some 999.); elev_gain = (Some 35);
                  elev_loss = (Some 14); elev_high = (Some 147);
                  elev_low = (Some 112);
                  start_latlng = (Some (54.75852, 25.320668));
                  end_latlng = (Some (54.763295, 25.318497));
                  average_speed = (Some 1.803); max_speed = (Some 4.);
                  average_cadence = (Some 70); max_cadence = (Some 81);
                  average_temp = (Some 26); average_heartrate = (Some 172);
                  max_heartrate = (Some 177); average_power = None;
                  max_power = None }
                };
              { split_index = 7; start = 3073; len = 351;
                stats =
                { data_points = 351; moving_time = 350; elapsed_time = 350;
                  distance = (Some 1000.); elev_gain = (Some 4);
                  elev_loss = (Some 18); elev_high = (Some 149);
                  elev_low = (Some 133);
                  start_latlng = (Some (54.763295, 25.318497));
                  end_latlng = (Some (54.763765, 25.326707));
                  average_speed = (Some 2.865); max_speed = (Some 6.);
                  average_cadence = (Some 79); max_cadence = (Some 82);
                  average_temp = (Some 25); average_heartrate = (Some 174);
                  max_heartrate = (Some 177); average_power = None;
                  max_power = None }
                };
              { split_index = 8; start = 3424; len = 455;
                stats =
                { data_points = 455; moving_time = 454; elapsed_time = 454;
                  distance = (Some 1000.); elev_gain = (Some 27);
                  elev_loss = (Some 25); elev_high = (Some 136);
                  elev_low = (Some 113);
                  start_latlng = (Some (54.763765, 25.326707));
                  end_latlng = (Some (54.764205, 25.325385));
                  average_speed = (Some 2.208); max_speed = (Some 4.);
                  average_cadence = (Some 73); max_cadence = (Some 89);
                  average_temp = (Some 26); average_heartrate = (Some 175);
                  max_heartrate = (Some 180); average_power = None;
                  max_power = None }
                };
              { split_index = 9; start = 3879; len = 435;
                stats =
                { data_points = 435; moving_time = 434; elapsed_time = 539;
                  distance = (Some 999.); elev_gain = (Some 17);
                  elev_loss = (Some 7); elev_high = (Some 150);
                  elev_low = (Some 135);
                  start_latlng = (Some (54.76423, 25.32541));
                  end_latlng = (Some (54.769912, 25.325525));
                  average_speed = (Some 2.307); max_speed = (Some 4.6);
                  average_cadence = (Some 75); max_cadence = (Some 83);
                  average_temp = (Some 26); average_heartrate = (Some 170);
                  max_heartrate = (Some 178); average_power = None;
                  max_power = None }
                };
              { split_index = 10; start = 4314; len = 301;
                stats =
                { data_points = 301; moving_time = 300; elapsed_time = 300;
                  distance = (Some 602.); elev_gain = (Some 8);
                  elev_loss = (Some 7); elev_high = (Some 146);
                  elev_low = (Some 143);
                  start_latlng = (Some (54.769912, 25.325525));
                  end_latlng = (Some (54.77022, 25.326757));
                  average_speed = (Some 2.013); max_speed = (Some 3.2);
                  average_cadence = (Some 79); max_cadence = (Some 80);
                  average_temp = (Some 27); average_heartrate = (Some 151);
                  max_heartrate = (Some 155); average_power = None;
                  max_power = None }
                }
              ]);
      streams = <opaque> } |}]

let parse_json_from_bytes_lexbuf (b : bytes) : Yojson.Safe.t =
  let s = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b in
  let lexbuf = Lexing.from_string s in
  let lexer_state = Yojson.init_lexer () in
  Yojson.Safe.from_lexbuf lexer_state lexbuf

let parse_json_from_bytes (b : bytes) : Yojson.Safe.t =
  let s = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b in
  Yojson.Safe.from_string s

let%expect_test "compress text" =
  let data =
    In_channel.read_all "/home/angel/Documents/ocaml/unto/get_streams_test.json"
  in
  let data_b = Bytes.of_string data in
  let compressed = LZ4.Bytes.compress data_b in
  printf "%d -> %d\n" (Bytes.length data_b) (Bytes.length compressed);
  let decompressed =
    LZ4.Bytes.decompress ~length:(String.length data) compressed
  in
  let json = parse_json_from_bytes decompressed in
  let streams = Streams.t_of_yojson json in
  printf "%s\n" (Streams.show streams);
  [%expect
    {|
    2186 -> 879
    [(TimeStream
        { type_ = "time";
          data =
          [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19;
            20];
          smoothed = <opaque>; series_type = "distance"; original_size = 21;
          resolution = "high" });
      (DistanceStream
         { type_ = "distance";
           data =
           [0.; 2.8; 5.5; 8.3; 11.; 13.8; 16.5; 19.3; 22.; 27.; 32.; 36.; 39.;
             42.; 45.; 48.; 51.; 54.; 57.; 60.; 64.];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
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
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (AltitudeStream
         { type_ = "altitude";
           data =
           [115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 115.2; 114.6;
             114.2; 114.; 113.8; 113.8; 113.4; 113.4; 113.2; 113.2; 112.8; 112.8;
             112.6];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (VelocityStream
         { type_ = "velocity_smooth";
           data =
           [0.; 0.; 2.75; 2.75; 2.75; 2.75; 2.75; 2.75; 2.75; 3.2; 3.65; 3.9;
             3.95; 4.; 3.6; 3.2; 3.; 3.; 3.; 3.; 3.2];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (HeartRateStream
         { type_ = "heartrate";
           data =
           [80; 79; 78; 77; 77; 75; 75; 75; 76; 78; 80; 80; 82; 84; 87; 89; 91;
             95; 97; 99; 100];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (CadenceStream
         { type_ = "cadence";
           data =
           [84; 84; 84; 84; 84; 84; 84; 84; 84; 85; 86; 86; 86; 85; 84; 83; 82;
             82; 82; 82; 82];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (WattsStream
         { type_ = "watts";
           data =
           [None; None; None; None; None; None; None; None; (Some 283);
             (Some 243); (Some 279); (Some 273); (Some 263); (Some 261);
             (Some 219); (Some 220); (Some 201); (Some 209); (Some 193);
             (Some 200); (Some 198)];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (TempStream
         { type_ = "temp";
           data =
           [29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29; 29;
             29; 29; 29; 29];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" });
      (GradeStream
         { type_ = "grade_smooth";
           data =
           [0.; 0.; 0.; 0.; 0.; 0.; 0.; -4.5; -6.5; -7.2; -8.2; -5.3; -6.2; -5.;
             -5.; -5.; -5.; -5.; -4.6; -4.6; -3.1];
           smoothed = <opaque>; series_type = "distance"; original_size = 21;
           resolution = "high" })
      ] |}]
