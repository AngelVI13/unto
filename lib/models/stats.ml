open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  data_points : int;
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
    data_points = 0;
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

let start_lat (t : t) =
  match t.start_latlng with
  | None -> None
  | Some latlng ->
      let lat, _ = latlng in
      Some lat

let start_lng (t : t) =
  match t.start_latlng with
  | None -> None
  | Some latlng ->
      let _, lng = latlng in
      Some lng

let end_lat (t : t) =
  match t.end_latlng with
  | None -> None
  | Some latlng ->
      let lat, _ = latlng in
      Some lat

let end_lng (t : t) =
  match t.end_latlng with
  | None -> None
  | Some latlng ->
      let _, lng = latlng in
      Some lng
