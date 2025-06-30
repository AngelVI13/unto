open Core

let () =
  let token = "5873bbbc433d6d8a886e96c69d3629358ff3afbf" in

  let activity_id = 14883225124 in
  let out = Unto.Strava.get_streams token activity_id in
  print_endline out
