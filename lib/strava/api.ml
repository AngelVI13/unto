open Core
open Models.Laps
open Utils
open Models.Streams
open Strava_api
open Models.Strava_models
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Or_error.Let_syntax

(* alias to Time_ns *)
module Time_ns = Time_ns_unix

(* TODO: 3. Visualize the data in a web ui *)
(* TODO: 4. Analyze the data *)
(* TODO: what is table index ? do i need to create one ? *)
(* TODO: activity 1419970951 is marked as Threadmill run but strava only shows
   it as RUN is the API giving correct type *)
(* TODO: there is a gap in activities from 2016-09-19T18:22:17Z =>
   2016-11-23T14:40:56Z for some reason *)

let pull_activities token num_activities =
  let activities = list_activities ~token ~page:1 ~per_page:num_activities () in
  print_endline activities

let pull_streams_aux token activity_id =
  let resp = get_streams token activity_id in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  (* let filename = sprintf "raw_streams_%d.json" activity_id in *)
  (* Yojson.Safe.to_file filename json; *)
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
  (* let filename = sprintf "raw_laps_%d.json" activity_id in *)
  (* Yojson.Safe.to_file filename json; *)
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
  let activities =
    List.map ~f:Models.Activity.t_of_StravaActivity strava_activities
  in
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
            Models.Activity.calculate_stats activity streams laps
        | Error e, Ok _ -> Error.raise e
        | Ok _, Error e -> Error.raise e
        | Error e1, Error e2 ->
            failwith
              (sprintf "Errors:\n%s\n%s\n" (Error.to_string_hum e1)
                 (Error.to_string_hum e2)))
      activities
  in
  let out = [%yojson_of: Models.Activity.t list] activities in
  let filename = sprintf "%s_stats.json" timestamp in
  Yojson.Safe.to_file filename out;
  printf "Saved stats to file %s\n" filename;
  Ok ()

let process_activity ~token (activity : Models.Activity.t) =
  printf "\tprocessing activity=%d\n" activity.id;

  Time_ns.pause (Time_ns.Span.create ~ms:500 ());
  printf "\t\tdownloading streams\n";
  let streams = pull_streams_aux token activity.id in

  Time_ns.pause (Time_ns.Span.create ~ms:500 ());
  printf "\t\tdownloading laps\n";
  let laps = pull_laps_aux token activity.id in

  let err_prefix = "error while downloading/parsing" in
  match (streams, laps) with
  | Ok streams, Ok laps ->
      printf "\t\tsuccessfully downloaded laps & streams\n";
      let laps = Laps.t_of_StravaLaps laps in
      printf "\t\tcalculating stats\n";
      let activity = Models.Activity.calculate_stats activity streams laps in
      Some activity
  | Error e, Ok _ ->
      printf "\t\t%s streams: %s\n" err_prefix (Error.to_string_hum e);
      None
  | Ok _, Error e ->
      printf "\t\t%s laps: %s\n" err_prefix (Error.to_string_hum e);
      None
  | Error e1, Error e2 ->
      printf "\t\t%s laps & streams:\n%s\n%s\n" err_prefix
        (Error.to_string_hum e1) (Error.to_string_hum e2);
      None

let fetch_athlete ~token =
  let resp = athlete_info token in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  let%bind strava_athlete =
    Or_error.try_with (fun () -> StravaAthlete.t_of_yojson json)
  in
  Ok strava_athlete

let fetch_one_page ~token ~page ~per_page =
  printf "downloading activity page %d (per_page=%d)\n" page per_page;
  let resp = list_activities ~token ~page ~per_page () in
  let%bind json = Or_error.try_with (fun () -> Yojson.Safe.from_string resp) in
  (* Yojson.Safe.to_file *)
  (*   (sprintf "activities_page_%d_per_%d.json" page per_page) *)
  (*   json; *)
  let%bind strava_activities =
    Or_error.try_with (fun () -> StravaActivities.t_of_yojson json)
  in
  let activities =
    List.map ~f:Models.Activity.t_of_StravaActivity strava_activities
  in
  Ok activities

let filter_activities (activities : Models.Activity.t list) exclude =
  List.filter
    ~f:(fun activity ->
      match List.exists ~f:(Int.equal activity.id) exclude with
      | true ->
          printf "...skipping activity (%d) - already exists\n" activity.id;
          false
      | false -> true)
    activities

let fetch_activities ~token ~num_activities ~start_page ~exclude =
  let per_page = 100 in
  let max_pages = 25 in
  let page_list = List.init max_pages ~f:(Int.( + ) 1) in
  let activities =
    List.fold ~init:[]
      ~f:(fun acc page_num ->
        if List.length acc >= num_activities then (
          printf "reached num_activities=%d, skipping page=%d\n" num_activities
            page_num;
          acc)
        else if page_num < start_page then (
          printf "skipping page=%d (start_page=%d)\n" page_num start_page;
          acc)
        else
          let activities = fetch_one_page ~token ~page:page_num ~per_page in
          match activities with
          | Error e ->
              printf "failed to fetch activities on page=%d per_page=%d:\n%s\n"
                page_num per_page (Error.to_string_hum e);
              acc
          | Ok activities ->
              printf "found %d activities on page=%d\n" (List.length activities)
                page_num;
              let activities =
                List.filter
                  ~f:(fun activity ->
                    match List.exists ~f:(Int.equal activity.id) exclude with
                    | true ->
                        printf "...skipping activity (%d) - already exists\n"
                          activity.id;
                        false
                    | false -> true)
                  activities
              in
              printf "%d activities after filtering\n" (List.length activities);
              let remaining = num_activities - List.length acc in
              printf "take %d activities\n" remaining;
              acc @ List.take activities remaining)
      page_list
  in
  let activities =
    activities |> List.map ~f:(process_activity ~token) |> List.filter_opt
  in
  Ok activities

let%expect_test "deserialize get_stream.json" =
  let json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/get_streams_test.json"
  in
  let streams = Streams.t_of_yojson_strava json in
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
  printf "%s" (Models.Stats.show stats);
  [%expect
    {|
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
  let streams = Streams.t_of_yojson_strava json in
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
