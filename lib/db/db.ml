open Core
open Db_ops
open Models.Strava_models
open Models.Stats
module DB = DbOps (Sqlgg_sqlite3)

type t = { filename : string; handle : Sqlite3.db }

let explain msg db = printf "%s : %s\n" msg (Sqlite3.errmsg db)

let create filename =
  let handle = Sqlite3.db_open filename in
  let _ = DB.create_athletes handle in
  let _ = DB.create_activities handle in
  let _ = DB.create_stats handle in
  let _ = DB.create_laps handle in
  let _ = DB.create_splits handle in
  let _ = DB.create_streams handle in
  { filename; handle }

let add_test_split { handle; _ } =
  let _ =
    DB.add_split handle ~id:None ~activity_id:12345L ~start:0L ~len:15L
      ~split_index:7L ~stats_id:13L
  in
  ()

let add_test_activity { handle; _ } id =
  let _ =
    DB.add_activity handle ~id ~athlete_id:1L
      ~name:(sprintf "run %d" (Int64.to_int_exn id))
      ~sport_type:"Run" ~start_date:"date" ~timezone:"EEST" ~map_id:"asasdsad"
      ~map_summary_polyline:"asda1221" ~stats_id:14L
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

let add_athlete_if_not_exist { handle; _ } (athlete : StravaAthlete.t) =
  let athlete_ids = ref [] in
  DB.list_athlete_ids handle (fun ~id -> athlete_ids := id :: !athlete_ids);

  if List.exists ~f:(Int64.equal (Int64.of_int athlete.id)) !athlete_ids then ()
  else
    ignore
      (DB.add_athlete handle ~id:(Int64.of_int athlete.id)
         ~firstname:athlete.firstname ~lastname:athlete.lastname
         ~city:athlete.city ~state:athlete.state ~country:athlete.country
         ~sex:athlete.sex ~created_at:athlete.created_at ~weight:athlete.weight)

let get_athlete { handle; _ } : StravaAthlete.t option =
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

let get_weeks_activities { handle; _ } ~(start_date : Date.t) :
    Models.Activity.t list =
  let end_date = Date.add_days start_date 7 in
  let end_date = Utils.iso8601_of_date end_date in
  let start_date = Utils.iso8601_of_date start_date in
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
      ~stats_id
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
      let _ = stats_id in
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
let get_activity { handle; _ } ~(activity_id : int) : Models.Activity.t option =
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
      ~stats_id
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
      let _ = stats_id in
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

let add_stats (t : t) (stats : Models.Stats.t) (activity_id : int) =
  let _ =
    DB.add_stats t.handle ~id:None ~activity_id:(Int64.of_int activity_id)
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
  (* explain (sprintf "add_stats %d" activity_id) t.handle; *)
  let stats_id = ref (Int64.of_int (-1)) in
  DB.stats_id_for_activity t.handle ~activity_id:(Int64.of_int activity_id)
    (fun ~id -> stats_id := id);
  (* explain *)
  (*   (sprintf "get_stats_id %d -> %d" activity_id (Int64.to_int_exn !stats_id)) *)
  (*   t.handle; *)
  !stats_id

let add_activity_aux (t : t) (activity : Models.Activity.t) (athlete_id : int)
    (stats_id : Int64.t) =
  let _ =
    DB.add_activity t.handle ~id:(Int64.of_int activity.id)
      ~athlete_id:(Int64.of_int athlete_id) ~name:activity.name
      ~sport_type:(Models.Strava_models.show_sportType activity.sport_type)
      ~start_date:activity.start_date ~timezone:activity.timezone
      ~map_id:activity.map_id
      ~map_summary_polyline:activity.map_summary_polyline ~stats_id
  in
  (* explain (sprintf "add_activity %d" activity.id) t.handle; *)
  ()

let add_lap (t : t) (lap : Models.Laps.Lap.t) (activity_id : int) =
  let stats_id = add_stats t lap.stats activity_id in
  ignore
    (DB.add_lap t.handle ~id:None ~activity_id:(Int64.of_int activity_id)
       ~lap_index:(Int64.of_int lap.lap_index)
       ~moving_time:(Int64.of_int lap.moving_time)
       ~start:(Int64.of_int lap.start) ~len:(Int64.of_int lap.len) ~stats_id)

let add_split (t : t) (split : Models.Splits.Split.t) (activity_id : int) =
  let stats_id = add_stats t split.stats activity_id in
  ignore
    (DB.add_split t.handle ~id:None ~activity_id:(Int64.of_int activity_id)
       ~split_index:(Int64.of_int split.split_index)
       ~start:(Int64.of_int split.start) ~len:(Int64.of_int split.len) ~stats_id)

let add_streams (t : t) (streams : Models.Streams.Streams.t) (activity_id : int)
    =
  let streams = Models.Streams.Streams.yojson_of_t streams in
  let streams = Yojson.Safe.to_string streams in
  let streams_bin = Bytes.of_string streams in
  let compressed = LZ4.Bytes.compress streams_bin in
  ignore
    (DB.add_streams t.handle ~id:None ~activity_id:(Int64.of_int activity_id)
       ~data:
         (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:compressed)
       ~data_len:(Int64.of_int @@ String.length streams))

let add_activity (t : t) (activity : Models.Activity.t) (athlete_id : int) =
  let stats_id = add_stats t activity.stats activity.id in
  add_activity_aux t activity athlete_id stats_id;
  ignore (List.map ~f:(fun lap -> add_lap t lap activity.id) activity.laps);
  ignore
    (List.map ~f:(fun split -> add_split t split activity.id) activity.splits);
  add_streams t activity.streams activity.id

let all_activities { handle; _ } =
  let activities = ref [] in
  let _ =
    DB.list_activities handle (fun ~id ->
        (* printf "Found activity %d in db\n" (Int64.to_int_exn id); *)
        activities := Int64.to_int_exn id :: !activities)
  in
  !activities

let stream_for_activity { handle; _ } (activity_id : int) =
  DB.streams_for_activity handle ~activity_id:(Int64.of_int activity_id)
    (fun ~id ~activity_id ~data ~data_len ->
      let _ = (id, activity_id) in
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

let load filename =
  match Sys_unix.file_exists filename with
  | `Yes ->
      printf "Opening existing db %s\n" filename;
      { handle = Sqlite3.db_open filename; filename }
  | _ ->
      printf "Creating new db file %s\n" filename;
      create filename

let close db = Or_error.try_with (fun () -> Sqlite3.db_close db.handle)
