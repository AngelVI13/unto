open Core
open Stats
open Elevation
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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

module Streams = struct
  type t = StreamType.t list [@@deriving show { with_path = false }, yojson]

  let stats (streams : t) : Stats.t =
    List.fold ~init:(Stats.empty ()) ~f:StreamType.stats_of_t streams
end
