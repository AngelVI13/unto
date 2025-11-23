open Core
open Db_ops
open Models.Strava_models
open Models.Stats
module DB = DbOps (Turso)

type t = Turso.conn [@@deriving show { with_path = false }]

let make ~hostname ~token : Turso.conn =
  (* TODO: check if db exists and if not, create it *)
  { hostname; token; log_name = "db_logs.txt" }

let create_tables handle =
  let _ = DB.create_athletes handle in
  let _ = DB.create_activities handle in
  let _ = DB.create_stats handle in
  let _ = DB.create_laps handle in
  let _ = DB.create_splits handle in
  let _ = DB.create_streams handle in
  ()

let add_test_split handle =
  let _ =
    DB.add_split handle ~id:None ~activity_id:12345L ~start:0L ~len:15L
      ~split_index:7L ~stats_id:13L
  in
  ()

let add_test_activity handle id =
  let _ =
    DB.add_activity handle ~id ~athlete_id:1L
      ~name:(sprintf "run %d" (Int64.to_int_exn id))
      ~sport_type:"Run" ~start_date:"date" ~timezone:"EEST" ~map_id:"asasdsad"
      ~map_summary_polyline:"asda1221"
  in
  ()

let to_int64_option (i : int option) =
  match i with None -> None | Some value -> Some (Int64.of_int value)

let to_int_option (i : Int64.t option) =
  match i with None -> None | Some value -> Some (Int64.to_int_exn value)

let to_loc_option (a : float option) (b : float option) =
  match (a, b) with
  | None, None -> None
  | Some a, Some b -> Some (a, b)
  | _ -> assert false

let get_num_athletes handle = DB.num_athletes handle |> Int64.to_int_exn

let add_athlete_if_not_exist handle (athlete : StravaAthlete.t) =
  let athlete_ids = ref [] in
  DB.list_athlete_ids handle (fun ~id -> athlete_ids := id :: !athlete_ids);

  if List.exists ~f:(Int64.equal (Int64.of_int athlete.id)) !athlete_ids then ()
  else
    ignore
      (DB.add_athlete handle ~id:(Int64.of_int athlete.id)
         ~firstname:athlete.firstname ~lastname:athlete.lastname
         ~city:athlete.city ~state:athlete.state ~country:athlete.country
         ~sex:athlete.sex ~created_at:athlete.created_at ~weight:athlete.weight)

let get_athlete handle : StravaAthlete.t option =
  let athletes = ref [] in
  DB.list_athletes handle
    (fun
      ~id ~firstname ~lastname ~city ~state ~country ~sex ~created_at ~weight ->
      let athlete =
        StravaAthlete.Fields.create ~id:(Int64.to_int_exn id) ~firstname
          ~lastname ~city ~state ~country ~sex ~created_at ~weight
      in
      athletes := athlete :: !athletes);
  List.hd !athletes

let get_activities_between handle ~(start_date : string) ~(end_date : string) :
    Models.Activity.t list =
  let activities = ref [] in
  DB.activities_between handle ~start_date ~end_date
    (fun
      ~id
      ~athlete_id
      ~name
      ~sport_type
      ~start_date
      ~timezone
      ~map_id
      ~map_summary_polyline
      ~moving_time
      ~elapsed_time
      ~distance
      ~elev_gain
      ~elev_loss
      ~elev_high
      ~elev_low
      ~start_lat
      ~start_lng
      ~end_lat
      ~end_lng
      ~average_speed
      ~max_speed
      ~average_cadence
      ~max_cadence
      ~average_temp
      ~average_heartrate
      ~max_heartrate
      ~average_power
      ~max_power
    ->
      let stats =
        Models.Stats.Fields.create ~data_points:(-1)
          ~moving_time:(Int64.to_int_exn moving_time)
          ~elapsed_time:(Int64.to_int_exn elapsed_time)
          ~distance ~elev_gain:(to_int_option elev_gain)
          ~elev_loss:(to_int_option elev_loss)
          ~elev_high:(to_int_option elev_high)
          ~elev_low:(to_int_option elev_low)
          ~start_latlng:(to_loc_option start_lat start_lng)
          ~end_latlng:(to_loc_option end_lat end_lng)
          ~average_speed ~max_speed
          ~average_cadence:(to_int_option average_cadence)
          ~max_cadence:(to_int_option max_cadence)
          ~average_temp:(to_int_option average_temp)
          ~average_heartrate:(to_int_option average_heartrate)
          ~max_heartrate:(to_int_option max_heartrate)
          ~average_power:(to_int_option average_power)
          ~max_power:(to_int_option max_power)
      in
      let activity =
        Models.Activity.Fields.create ~id:(Int64.to_int_exn id)
          ~athlete_id:(Int64.to_int_exn athlete_id)
          ~name
          ~sport_type:(Models.Strava_models.sportType_of_string sport_type)
          ~start_date ~timezone ~map_id ~map_summary_polyline ~stats
          ~laps:(Models.Laps.Laps.empty ())
          ~splits:(Models.Splits.Splits.empty ())
          ~streams:(Models.Streams.Streams.empty ())
      in
      activities := activity :: !activities);
  !activities

let get_weeks_activities handle ~(start_date : Date.t) : Models.Activity.t list
    =
  let end_date = Date.add_days start_date 7 in
  let start_date = Utils.iso8601_of_date start_date in
  let end_date = Utils.iso8601_of_date end_date in
  get_activities_between ~start_date ~end_date handle

let get_months_activities handle ~(start_date : Date.t) ~(end_date : Date.t) :
    Models.Activity.t list =
  let start_date = Utils.iso8601_of_date start_date in
  let end_date = Utils.iso8601_of_date ~end_of_day:true end_date in
  get_activities_between ~start_date ~end_date handle

let get_laps_by_activity_id handle ~(activity_id : int) : Models.Laps.Laps.t =
  let laps = ref [] in
  DB.laps_by_activity_id handle ~activity_id:(Int64.of_int activity_id)
    (fun
      ~lap_index
      ~start
      ~len
      ~moving_time
      ~elapsed_time
      ~distance
      ~elev_gain
      ~elev_loss
      ~elev_high
      ~elev_low
      ~start_lat
      ~start_lng
      ~end_lat
      ~end_lng
      ~average_speed
      ~max_speed
      ~average_cadence
      ~max_cadence
      ~average_temp
      ~average_heartrate
      ~max_heartrate
      ~average_power
      ~max_power
    ->
      let lap =
        Models.Laps.Lap.make ~start:(Int64.to_int_exn start)
          ~len:(Int64.to_int_exn len)
          ~index:(Int64.to_int_exn lap_index)
      in
      let stats =
        Models.Stats.Fields.create ~data_points:(-1)
          ~moving_time:(Int64.to_int_exn moving_time)
          ~elapsed_time:(Int64.to_int_exn elapsed_time)
          ~distance ~elev_gain:(to_int_option elev_gain)
          ~elev_loss:(to_int_option elev_loss)
          ~elev_high:(to_int_option elev_high)
          ~elev_low:(to_int_option elev_low)
          ~start_latlng:(to_loc_option start_lat start_lng)
          ~end_latlng:(to_loc_option end_lat end_lng)
          ~average_speed ~max_speed
          ~average_cadence:(to_int_option average_cadence)
          ~max_cadence:(to_int_option max_cadence)
          ~average_temp:(to_int_option average_temp)
          ~average_heartrate:(to_int_option average_heartrate)
          ~max_heartrate:(to_int_option max_heartrate)
          ~average_power:(to_int_option average_power)
          ~max_power:(to_int_option max_power)
      in
      let lap = Models.Laps.Lap.set_stats lap stats in
      laps := lap :: !laps);
  List.rev !laps

let get_splits_by_activity_id handle ~(activity_id : int) :
    Models.Splits.Splits.t =
  let splits = ref [] in
  DB.splits_by_activity_id handle ~activity_id:(Int64.of_int activity_id)
    (fun
      ~split_index
      ~start
      ~len
      ~moving_time
      ~elapsed_time
      ~distance
      ~elev_gain
      ~elev_loss
      ~elev_high
      ~elev_low
      ~start_lat
      ~start_lng
      ~end_lat
      ~end_lng
      ~average_speed
      ~max_speed
      ~average_cadence
      ~max_cadence
      ~average_temp
      ~average_heartrate
      ~max_heartrate
      ~average_power
      ~max_power
    ->
      let split =
        Models.Splits.Split.make ~start:(Int64.to_int_exn start)
          ~len:(Int64.to_int_exn len)
          ~index:(Int64.to_int_exn split_index)
      in
      let stats =
        Models.Stats.Fields.create ~data_points:(-1)
          ~moving_time:(Int64.to_int_exn moving_time)
          ~elapsed_time:(Int64.to_int_exn elapsed_time)
          ~distance ~elev_gain:(to_int_option elev_gain)
          ~elev_loss:(to_int_option elev_loss)
          ~elev_high:(to_int_option elev_high)
          ~elev_low:(to_int_option elev_low)
          ~start_latlng:(to_loc_option start_lat start_lng)
          ~end_latlng:(to_loc_option end_lat end_lng)
          ~average_speed ~max_speed
          ~average_cadence:(to_int_option average_cadence)
          ~max_cadence:(to_int_option max_cadence)
          ~average_temp:(to_int_option average_temp)
          ~average_heartrate:(to_int_option average_heartrate)
          ~max_heartrate:(to_int_option max_heartrate)
          ~average_power:(to_int_option average_power)
          ~max_power:(to_int_option max_power)
      in
      let split = Models.Splits.Split.set_stats split stats in
      splits := split :: !splits);
  List.rev !splits

(* NOTE: currently this is the same as other activity methods but it will change with addition of laps and splits *)
let get_activity handle ~(activity_id : int) : Models.Activity.t option =
  let activities = ref [] in
  DB.activity_by_id handle ~activity_id:(Int64.of_int activity_id)
    (fun
      ~id
      ~athlete_id
      ~name
      ~sport_type
      ~start_date
      ~timezone
      ~map_id
      ~map_summary_polyline
      ~moving_time
      ~elapsed_time
      ~distance
      ~elev_gain
      ~elev_loss
      ~elev_high
      ~elev_low
      ~start_lat
      ~start_lng
      ~end_lat
      ~end_lng
      ~average_speed
      ~max_speed
      ~average_cadence
      ~max_cadence
      ~average_temp
      ~average_heartrate
      ~max_heartrate
      ~average_power
      ~max_power
      ~data
      ~data_len
    ->
      let stats =
        Models.Stats.Fields.create ~data_points:(-1)
          ~moving_time:(Int64.to_int_exn moving_time)
          ~elapsed_time:(Int64.to_int_exn elapsed_time)
          ~distance ~elev_gain:(to_int_option elev_gain)
          ~elev_loss:(to_int_option elev_loss)
          ~elev_high:(to_int_option elev_high)
          ~elev_low:(to_int_option elev_low)
          ~start_latlng:(to_loc_option start_lat start_lng)
          ~end_latlng:(to_loc_option end_lat end_lng)
          ~average_speed ~max_speed
          ~average_cadence:(to_int_option average_cadence)
          ~max_cadence:(to_int_option max_cadence)
          ~average_temp:(to_int_option average_temp)
          ~average_heartrate:(to_int_option average_heartrate)
          ~max_heartrate:(to_int_option max_heartrate)
          ~average_power:(to_int_option average_power)
          ~max_power:(to_int_option max_power)
      in
      (* decode streams blob into type *)
      let data = Base64.decode_exn ~pad:false data in
      let data_bin = Bytes.of_string data in
      let data =
        LZ4.Bytes.decompress ~length:(Int64.to_int_exn data_len) data_bin
      in
      let json = Utils.parse_json_from_bytes data in
      let streams = Models.Streams.Streams.t_of_yojson json in

      let splits = get_splits_by_activity_id ~activity_id handle in
      let laps = get_laps_by_activity_id ~activity_id handle in

      let activity =
        Models.Activity.Fields.create ~id:(Int64.to_int_exn id)
          ~athlete_id:(Int64.to_int_exn athlete_id)
          ~name
          ~sport_type:(Models.Strava_models.sportType_of_string sport_type)
          ~start_date ~timezone ~map_id ~map_summary_polyline ~stats ~laps
          ~splits ~streams
      in
      activities := activity :: !activities);
  List.hd !activities

let add_stats handle (stats : Models.Stats.t) (activity_id : int) =
  let _ =
    DB.add_stats handle ~id:None ~activity_id:(Int64.of_int activity_id)
      ~data_points:(Int64.of_int stats.data_points)
      ~moving_time:(Int64.of_int stats.moving_time)
      ~elapsed_time:(Int64.of_int stats.elapsed_time)
      ~distance:stats.distance
      ~elev_gain:(to_int64_option stats.elev_gain)
      ~elev_loss:(to_int64_option stats.elev_loss)
      ~elev_high:(to_int64_option stats.elev_high)
      ~elev_low:(to_int64_option stats.elev_low)
      ~start_lat:(Models.Stats.start_lat stats)
      ~start_lng:(Models.Stats.start_lng stats)
      ~end_lat:(Models.Stats.end_lat stats)
      ~end_lng:(Models.Stats.end_lng stats)
      ~average_speed:stats.average_speed ~max_speed:stats.max_speed
      ~average_cadence:(to_int64_option stats.average_cadence)
      ~max_cadence:(to_int64_option stats.max_cadence)
      ~average_temp:(to_int64_option stats.average_temp)
      ~average_heartrate:(to_int64_option stats.average_heartrate)
      ~max_heartrate:(to_int64_option stats.max_heartrate)
      ~average_power:(to_int64_option stats.average_power)
      ~max_power:(to_int64_option stats.max_power)
  in
  let stats_id = ref (Int64.of_int (-1)) in
  DB.stats_id_for_activity handle ~activity_id:(Int64.of_int activity_id)
    (fun ~id -> stats_id := id);
  !stats_id

let add_activity_aux handle (activity : Models.Activity.t) (athlete_id : int) =
  let _ =
    DB.add_activity handle ~id:(Int64.of_int activity.id)
      ~athlete_id:(Int64.of_int athlete_id) ~name:activity.name
      ~sport_type:(Models.Strava_models.show_sportType activity.sport_type)
      ~start_date:activity.start_date ~timezone:activity.timezone
      ~map_id:activity.map_id
      ~map_summary_polyline:activity.map_summary_polyline
  in
  ()

let add_lap handle (lap : Models.Laps.Lap.t) (activity_id : int) =
  let stats_id = add_stats handle lap.stats activity_id in
  ignore
    (DB.add_lap handle ~id:None ~activity_id:(Int64.of_int activity_id)
       ~lap_index:(Int64.of_int lap.lap_index)
       ~moving_time:(Int64.of_int lap.moving_time)
       ~start:(Int64.of_int lap.start) ~len:(Int64.of_int lap.len) ~stats_id)

let add_split handle (split : Models.Splits.Split.t) (activity_id : int) =
  let stats_id = add_stats handle split.stats activity_id in
  ignore
    (DB.add_split handle ~id:None ~activity_id:(Int64.of_int activity_id)
       ~split_index:(Int64.of_int split.split_index)
       ~start:(Int64.of_int split.start) ~len:(Int64.of_int split.len) ~stats_id)

let add_streams handle (streams : Models.Streams.Streams.t) (activity_id : int)
    =
  let streams = Models.Streams.Streams.yojson_of_t streams in
  let streams = Yojson.Safe.to_string streams in
  let streams_bin = Bytes.of_string streams in
  let compressed = LZ4.Bytes.compress streams_bin in
  let data =
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:compressed
  in
  let data = Base64.encode_exn ~pad:false data in
  ignore
    (DB.add_streams handle ~id:None ~activity_id:(Int64.of_int activity_id)
       ~data
       ~data_len:(Int64.of_int @@ String.length streams))

let add_activity handle (activity : Models.Activity.t) (athlete_id : int) =
  add_activity_aux handle activity athlete_id;
  ignore (add_stats handle activity.stats activity.id);
  ignore (List.map ~f:(fun lap -> add_lap handle lap activity.id) activity.laps);
  ignore
    (List.map
       ~f:(fun split -> add_split handle split activity.id)
       activity.splits);
  add_streams handle activity.streams activity.id

let all_activities handle =
  let activities = ref [] in
  let _ =
    DB.list_activities handle (fun ~id ->
        (* printf "Found activity %d in db\n" (Int64.to_int_exn id); *)
        activities := Int64.to_int_exn id :: !activities)
  in
  !activities

let stream_for_activity handle (activity_id : int) =
  DB.streams_for_activity handle ~activity_id:(Int64.of_int activity_id)
    (fun ~id ~activity_id ~data ~data_len ->
      let _ = (id, activity_id) in
      let data = Base64.decode_exn ~pad:false data in
      let data_bin = Bytes.of_string data in
      let data =
        LZ4.Bytes.decompress ~length:(Int64.to_int_exn data_len) data_bin
      in
      let json = Utils.parse_json_from_bytes data in
      Yojson.Safe.to_file
        (sprintf "decompressed_streams_%d.json" (Int64.to_int_exn activity_id))
        json;
      let streams = Models.Streams.Streams.t_of_yojson json in
      printf "%s\n" (Models.Streams.Streams.show streams);
      ())

(* NOTE: this is not needed for turso connection *)
let close _ = Ok ()

let test2 () =
  let token = Sys.getenv_exn "TURSO_DB_TOKEN" in
  let hostname = Sys.getenv_exn "TURSO_DB_HOSTNAME" in
  let module DB = DbOps (Turso) in
  DB.list_athletes (make ~hostname ~token)
    (fun
      ~id ~firstname ~lastname ~city ~state ~country ~sex ~created_at ~weight ->
      let _ =
        (id, firstname, lastname, city, state, country, sex, created_at, weight)
      in
      ())

let test () =
  let module DB = DbOps (Turso) in
  let today = Time_ns.now () |> Time_ns.to_date ~zone:Timezone.utc in
  let start = Date.add_days today (-80) in
  let end_day = Date.add_days today (-70) in
  let _ = (start, end_day) in
  let activities = ref [] in
  (* DB.create_activities { url = ""; token = "" } *)
  let token = Sys.getenv_exn "TURSO_DB_TOKEN" in
  let hostname = Sys.getenv_exn "TURSO_DB_HOSTNAME" in
  DB.activities_between
    (make ~hostname ~token)
    (* ~start_date:(Utils.iso8601_of_date start) *)
    (* ~end_date:(Utils.iso8601_of_date end_day) *)
    ~start_date:"2025-10-01T00:00:00Z" ~end_date:"2025-10-04T00:00:00Z"
    (fun
      ~id
      ~athlete_id
      ~name
      ~sport_type
      ~start_date
      ~timezone
      ~map_id
      ~map_summary_polyline
      ~moving_time
      ~elapsed_time
      ~distance
      ~elev_gain
      ~elev_loss
      ~elev_high
      ~elev_low
      ~start_lat
      ~start_lng
      ~end_lat
      ~end_lng
      ~average_speed
      ~max_speed
      ~average_cadence
      ~max_cadence
      ~average_temp
      ~average_heartrate
      ~max_heartrate
      ~average_power
      ~max_power
    ->
      let stats =
        Models.Stats.Fields.create ~data_points:(-1)
          ~moving_time:(Int64.to_int_exn moving_time)
          ~elapsed_time:(Int64.to_int_exn elapsed_time)
          ~distance ~elev_gain:(to_int_option elev_gain)
          ~elev_loss:(to_int_option elev_loss)
          ~elev_high:(to_int_option elev_high)
          ~elev_low:(to_int_option elev_low)
          ~start_latlng:(to_loc_option start_lat start_lng)
          ~end_latlng:(to_loc_option end_lat end_lng)
          ~average_speed ~max_speed
          ~average_cadence:(to_int_option average_cadence)
          ~max_cadence:(to_int_option max_cadence)
          ~average_temp:(to_int_option average_temp)
          ~average_heartrate:(to_int_option average_heartrate)
          ~max_heartrate:(to_int_option max_heartrate)
          ~average_power:(to_int_option average_power)
          ~max_power:(to_int_option max_power)
      in
      let activity =
        Models.Activity.Fields.create ~id:(Int64.to_int_exn id)
          ~athlete_id:(Int64.to_int_exn athlete_id)
          ~name
          ~sport_type:(Models.Strava_models.sportType_of_string sport_type)
          ~start_date ~timezone ~map_id ~map_summary_polyline ~stats
          ~laps:(Models.Laps.Laps.empty ())
          ~splits:(Models.Splits.Splits.empty ())
          ~streams:(Models.Streams.Streams.empty ())
      in
      printf "%s\n" (Models.Activity.show activity);
      activities := activity :: !activities);
  !activities

let test4 () =
  let token = Sys.getenv_exn "TURSO_DB_TOKEN" in
  let hostname = Sys.getenv_exn "TURSO_DB_HOSTNAME" in
  printf "%s\n" token;
  let module DB = DbOps (Turso) in
  (* DB.create_activities { url = ""; token = "" } *)
  DB.add_athlete (make ~hostname ~token) ~id:(Int64.of_int 2) ~firstname:"John"
    ~lastname:"Smith" ~city:"Ruse" ~state:"Ruse" ~country:"Bulgaria" ~sex:"M"
    ~created_at:"1994-03-13" ~weight:68.5

let test5 () =
  let token = Sys.getenv_exn "TURSO_DB_TOKEN" in
  let hostname = Sys.getenv_exn "TURSO_DB_HOSTNAME" in
  let module DB = DbOps (Turso) in
  (* DB.create_activities { url = ""; token = "" } *)
  let num = DB.num_athletes (make ~hostname ~token) in
  printf "RESULT: %s\n" (Int64.to_string_hum num)

let test6 () = Turso_api.create "new-test-db"
let test7 () = Turso_api.delete "new-test-db"
let test8 () = Ok ()

let test9 () =
  let open Or_error.Let_syntax in
  let%bind db_info = Turso_api.create "new-tst" in
  printf "CREATE_INFO: %s\n" (Turso_api.DbResponse.show db_info);
  let%bind tokens = Turso_api.create_tokens db_info.name in
  printf "TOKENS: %s\n" tokens;
  let%bind delete_info = Turso_api.delete db_info.name in
  printf "DELETE_INFO: %s\n" (Turso_api.DeleteDbResponse.show delete_info);
  Ok ()

let test10 () =
  let open Or_error.Let_syntax in
  let%bind dbs = Turso_api.list_dbs () in
  List.iter
    ~f:(fun db -> printf "DB: %s\n" (Turso_api.DbEntryResponse.show db))
    dbs;
  Ok ()

let test11 app_name =
  let open Or_error.Let_syntax in
  let%bind db_info = Turso_api.create (sprintf "%s-users" app_name) in
  printf "CREATE_INFO: %s\n" (Turso_api.DbResponse.show db_info);
  let%bind tokens = Turso_api.create_tokens db_info.name in
  printf "TOKENS: %s\n" tokens;
  let%bind dbs = Turso_api.list_dbs () in
  List.iter
    ~f:(fun db -> printf "DB: %s\n" (Turso_api.DbEntryResponse.show db))
    dbs;
  Ok ()

let test12 () =
  let token = Sys.getenv_exn "TURSO_USERS_TOKEN" in
  let hostname = Sys.getenv_exn "TURSO_USERS_HOSTNAME" in
  let open Users_ops in
  let module DB = UsersOps (Turso) in
  let _ = DB.create_users (make ~hostname ~token) in
  let _ =
    DB.add_user (make ~hostname ~token) ~id:None ~user:"Angel" ~db_name:"app"
      ~hostname:"app-angelvi13.aws-eu-west-1.turso.io" ~token:""
  in
  let _ =
    DB.add_user (make ~hostname ~token) ~id:None ~user:"Test" ~db_name:"testapp"
      ~hostname:"testapp-angelvi13.aws-eu-west-1.turso.io" ~token:""
  in
  ()

let test13 () =
  let hostname = Sys.getenv_exn "TURSO_TEST_DB_HOSTNAME" in
  let token = Sys.getenv_exn "TURSO_TEST_DB_TOKEN" in
  let db = make ~hostname ~token in
  (* "sql": "SELECT a.id, st.data, st.data_len FROM activities a JOIN streams st ON a.id = st.activity_id WHERE a.id == @activity_id;", *)
  let req =
    {|
{
  "requests": [
    {
      "type": "execute",
      "stmt": {
        "sql": "SELECT a.id, st.data_len FROM activities a JOIN streams st ON a.id = st.activity_id WHERE a.id == @activity_id;",
        "named_args": [
          {
            "name": "activity_id",
            "value": {
              "type": "integer",
              "value": "16167913862"
            }
          }
        ]
      }
    }
  ]
}
  |}
  in
  let resp = Turso.make_turso_request db (`String req) in

  printf "%s" resp;
  Ok ()

let test14 () =
  (* let module DB = DbOps (Sqlgg_sqlite3) in *)
  (* let handle = Sqlite3.db_open "testapp.db" in *)
  (* ignore (DB.create_new_streams handle); *)
  (* let stream_rows = ref [] in *)
  (* DB.list_streams handle (fun ~id ~activity_id ~data ~data_len -> *)
  (*     let _ = (id, activity_id, data, data_len) in *)
  (*     let data = Base64.encode_exn ~pad:false data in *)
  (*     stream_rows := (id, activity_id, data, data_len) :: !stream_rows; *)
  (*     ()); *)
  (* List.iter !stream_rows ~f:(fun (id, activity_id, data, data_len) -> *)
  (*     ignore *)
  (*       (DB.add_new_streams handle ~id:(Some id) ~activity_id ~data ~data_len)); *)
  (* ignore (Sqlite3.db_close handle); *)
  Ok ()
