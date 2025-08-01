open Core
open Core_bench

let () =
  let open Unto in
  let data =
    In_channel.read_all "/home/angel/Documents/ocaml/unto/get_streams_test.json"
  in
  let data_b = Bytes.of_string data in

  let tests =
    [
      Bench.Test.create ~name:"lexbuf" (fun () ->
          ignore (Strava.parse_json_from_bytes_lexbuf data_b));
      Bench.Test.create ~name:"from_string" (fun () ->
          ignore (Strava.parse_json_from_bytes data_b));
    ]
  in

  Command_unix.run (Bench.make_command tests)
