open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* TODO: To get an auth_code, paste this URL to your browser, authorize the app
   and copy the auth_code from the redirected url *)
(* https://www.strava.com/oauth/authorize?client_id=21710&response_type=code&redirect_uri=http://localhost/exchange_token&approval_prompt=force&scope=read_all *)

(* TODO: json serialize the data *)
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
  | TupleStream of (float * float) Stream.t
[@@deriving show, yojson_of]

let streamType_of_yojson (json : Yojson.Safe.t) : streamType =
  (* TODO: support the other types as well *)
  match
    (Yojson.Safe.Util.member "data" json, Yojson.Safe.Util.member "type" json)
  with
  | `List (`Int _ :: _), `String _ ->
      IntStream (Stream.t_of_yojson [%of_yojson: int] json)
  | `List (`Float _ :: _), `String _ ->
      FloatStream (Stream.t_of_yojson [%of_yojson: float] json)
  | _ -> failwith "unsupported"

module Streams = struct
  type t = streamType list [@@deriving show, yojson]

  let example () =
    [ IntStream (Stream.int_example ()); FloatStream (Stream.float_example ()) ]
end

let _athlete_info token =
  let url = "https://www.strava.com/api/v3/athlete" in
  let headers = [ ("Authorization", sprintf "Bearer %s" token) ] in
  let res = Ezcurl.get ~headers ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> s in
  out

let _list_activities token =
  let url = "https://www.strava.com/api/v3/athlete/activities" in
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

(* TODO: load these from .env file *)
let _obtain_access_token auth_code =
  let url = "https://www.strava.com/oauth/token" in
  let params =
    [
      Curl.CURLFORM_CONTENT ("client_id", "21710", Curl.DEFAULT);
      Curl.CURLFORM_CONTENT
        ( "client_secret",
          "dec7bc0ead05b83433646ff57298da78272ddaa7",
          Curl.DEFAULT );
      Curl.CURLFORM_CONTENT ("code", auth_code, Curl.DEFAULT);
      Curl.CURLFORM_CONTENT ("grant_type", "authorization_code", Curl.DEFAULT);
    ]
  in
  let res = Ezcurl.post ~params ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> s in
  out

(* TODO: make a AHK script that does the 2FA *)
(* let auth_code = "a298cebcec3a1bc04c9719ba18c9c00e6166c2ad" in *)
(* let out = obtain_access_token auth_code in *)
(* let token = "5873bbbc433d6d8a886e96c69d3629358ff3afbf" in *)
(* let out = athlete_info token in *)
(* print_endline out; *)
(* let out = list_activities token in *)
(* print_endline out *)
(* let activity_id = 14883225124 in *)
(* let out = get_streams token activity_id in *)
(* print_endline out *)

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

let%expect_test "deserialize example.json" =
  let json =
    Yojson.Safe.from_file "/home/angel/Documents/ocaml/unto/example.json"
  in
  let streams = Streams.t_of_yojson json in
  printf "%s" (Streams.show streams);
  [%expect
    {|
    [(Strava.IntStream
        { type_ = "time"; data = [0; 1; 2; 3; 4]; series_type = "distance";
          original_size = 5; resolution = "high" });
      (Strava.FloatStream
         { type_ = "distance"; data = [0.; 2.8; 5.5; 8.3; 11.];
           series_type = "distance"; original_size = 5; resolution = "high" })
      ]
    |}]
