open Core
open Laps
open Splits
open Streams
open Route
open Strava_models
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = {
  id : int;
  athlete_id : int;
  name : string;
  sport_type : sportType;
  start_date : string;
  timezone : string;
  map_id : string;
  (* NOTE: Polyline values are string encodings of the latitude and longitude points
     using the Google encoded polyline algorithm format. More info can be found
     here:
       https://developers.google.com/maps/documentation/utilities/polylinealgorithm
       *)
  map_summary_polyline : string;
  (* NOTE: these are calculated from the data streams of the
       activity and not taken from strava directly *)
  stats : Stats.t;
  laps : Laps.t;
  splits : Splits.t;
  streams : Streams.t; [@opaque]
  route : Route.t option; [@opaque]
}
[@@deriving show { with_path = false }, fields, yojson_of]

let t_of_StravaActivity (activity : StravaActivity.t) : t =
  {
    id = activity.id;
    athlete_id = activity.athlete.id;
    name = activity.name;
    sport_type = sportType_of_string activity.sport_type;
    start_date = activity.start_date_local;
    timezone = activity.timezone;
    map_id = activity.map.id;
    map_summary_polyline = activity.map.summary_polyline;
    stats = Stats.empty ();
    laps = [];
    splits = [];
    streams = [];
    route = None;
  }

let calculate_stats (t : t) (streams : Streams.t) (laps : Laps.t) : t =
  let laps =
    try
      List.map
        ~f:(fun l ->
          let lap_stats = Streams.lap_stats streams l in
          Lap.set_stats l lap_stats)
        laps
    with _ -> []
  in
  let stats = Streams.activity_stats streams in
  let splits = Streams.splits streams in
  let splits =
    match splits with
    | None -> []
    | Some splits -> (
        try
          List.map
            ~f:(fun split ->
              let split_stats = Streams.split_stats streams split in
              Split.set_stats split split_stats)
            splits
        with _ -> [])
  in
  let route = Route.of_streams streams in
  { t with streams; stats; laps; splits; route }

let empty () =
  {
    id = 0;
    athlete_id = 0;
    name = "";
    sport_type = Run;
    start_date = "";
    timezone = "";
    map_id = "";
    map_summary_polyline = "";
    stats = Stats.empty ();
    laps = [];
    splits = [];
    streams = [];
    route = None;
  }

let simple_serialize (t : t) : Yojson.Safe.t =
  let route_json =
    match t.route with None -> `Null | Some r -> Route.yojson_of_t r
  in
  `Assoc
    [
      ("id", `Int t.id);
      ("start_date", `String t.start_date);
      ("sport_type", `String (show_sportType t.sport_type));
      ("route", route_json);
    ]

let simple_deserialize (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let id = json |> member "id" |> to_int in
  let start_date = json |> member "start_date" |> to_string in
  let sport_type = json |> member "sport_type" |> to_string in
  let route_json = json |> member "route" in
  let route =
    match route_json with
    | `Null -> None
    | _ -> Some (Route.t_of_yojson route_json)
  in
  let activity = empty () in
  {
    activity with
    id;
    start_date;
    sport_type = sportType_of_string sport_type;
    route;
  }

let simple_show (t : t) : string =
  let route_str =
    match t.route with None -> "None" | Some r -> Route.show r
  in
  sprintf "{\n\tid=%d;\n\tsport_type=%s;\n\tstart_date=%s;\n\troute=%s;\n}\n"
    t.id
    (show_sportType t.sport_type)
    t.start_date route_str

let%expect_test "simple_serialize" =
  let hash_json =
    Yojson.Safe.from_file
      "/home/angel/Documents/ocaml/unto/5kloop_streams_16575000264.json"
  in
  let streams = Streams.t_of_yojson_smoothed hash_json in
  let route = Option.value_exn (Route.of_streams streams) in

  let activity = empty () in
  let activity =
    {
      activity with
      id = 1;
      sport_type = RockClimbing;
      start_date = "Start date";
      route = Some route;
    }
  in

  let json = simple_serialize activity in

  printf "Activity before: %s" (simple_show activity);
  [%expect
    {|
      Activity before: {
      	id=1;
      	sport_type=RockClimbing;
      	start_date=Start date;
      	route={ hash =
        [["u9dp0n7s"; "u9dp0n7t"; "u9dp0n7e"; "u9dp0n7u"; "u9dp0n7k"; "u9dp0n7v";
           "u9dp0n7m"; "u9dp0n7g"; "u9dp0n77"];
          ["u9dp0n6q"; "u9dp0n6r"; "u9dp0n6m"; "u9dp0n6w"; "u9dp0n6n"; "u9dp0n6x";
            "u9dp0n6p"; "u9dp0n6t"; "u9dp0n6j"];
          ["u9dp0j2r"; "u9dp0j82"; "u9dp0j2q"; "u9dp0j2x"; "u9dp0j2p"; "u9dp0j88";
            "u9dp0j80"; "u9dp0j2w"; "u9dp0j2n"];
          ["u99zpvnw"; "u99zpvnx"; "u99zpvnt"; "u99zpvny"; "u99zpvnq"; "u99zpvnz";
            "u99zpvnr"; "u99zpvnv"; "u99zpvnm"];
          ["u99zpvh2"; "u99zpvh3"; "u99zpuur"; "u99zpvh8"; "u99zpvh0"; "u99zpvh9";
            "u99zpvh1"; "u99zpuux"; "u99zpuup"];
          ["u99zpudq"; "u99zpudr"; "u99zpudm"; "u99zpudw"; "u99zpudn"; "u99zpudx";
            "u99zpudp"; "u99zpudt"; "u99zpudj"];
          ["u99zpu9x"; "u99zpuc8"; "u99zpu9w"; "u99zpu9z"; "u99zpu9r"; "u99zpucb";
            "u99zpuc2"; "u99zpu9y"; "u99zpu9q"];
          ["u99zptnb"; "u99zptnc"; "u99zpsyz"; "u99zptp0"; "u99zptn8"; "u99zptp1";
            "u99zptn9"; "u99zpszp"; "u99zpsyx"];
          ["u99zptnw"; "u99zptnx"; "u99zptnt"; "u99zptny"; "u99zptnq"; "u99zptnz";
            "u99zptnr"; "u99zptnv"; "u99zptnm"];
          ["u99zptm1"; "u99zptm4"; "u99zptm0"; "u99zptm3"; "u99zptkc"; "u99zptm6";
            "u99zptkf"; "u99zptm2"; "u99zptkb"];
          ["u99zptep"; "u99zptg0"; "u99zpten"; "u99zpter"; "u99zptdz"; "u99zptg2";
            "u99zptfb"; "u99zpteq"; "u99zptdy"];
          ["u99zpqqw"; "u99zpqqx"; "u99zpqqt"; "u99zpqqy"; "u99zpqqq"; "u99zpqqz";
            "u99zpqqr"; "u99zpqqv"; "u99zpqqm"];
          ["u99zpqwc"; "u99zpqwf"; "u99zpqwb"; "u99zpqx1"; "u99zpqw9"; "u99zpqx4";
            "u99zpqwd"; "u99zpqx0"; "u99zpqw8"];
          ["u99zpw82"; "u99zpw83"; "u99zpw2r"; "u99zpw88"; "u99zpw80"; "u99zpw89";
            "u99zpw81"; "u99zpw2x"; "u99zpw2p"];
          ["u99zpybd"; "u99zpybe"; "u99zpyb9"; "u99zpybf"; "u99zpyb6"; "u99zpybg";
            "u99zpyb7"; "u99zpybc"; "u99zpyb3"];
          ["u99zpyfe"; "u99zpyfs"; "u99zpyfd"; "u99zpyfg"; "u99zpyf7"; "u99zpyfu";
            "u99zpyfk"; "u99zpyff"; "u99zpyf6"];
          ["u99zpyxv"; "u99zpyxy"; "u99zpyxu"; "u9dp0n8j"; "u99zpyxt"; "u9dp0n8n";
            "u99zpyxw"; "u9dp0n8h"; "u99zpyxs"];
          ["u9dp0n8y"; "u9dp0n8z"; "u9dp0n8v"; "u9dp0n9n"; "u9dp0n8w"; "u9dp0n9p";
            "u9dp0n8x"; "u9dp0n9j"; "u9dp0n8t"];
          ["u9dp0ndd"; "u9dp0nde"; "u9dp0nd9"; "u9dp0ndf"; "u9dp0nd6"; "u9dp0ndg";
            "u9dp0nd7"; "u9dp0ndc"; "u9dp0nd3"];
          ["u9dp0n6q"; "u9dp0n6r"; "u9dp0n6m"; "u9dp0n6w"; "u9dp0n6n"; "u9dp0n6x";
            "u9dp0n6p"; "u9dp0n6t"; "u9dp0n6j"];
          ["u9dp0n7e"; "u9dp0n7s"; "u9dp0n7d"; "u9dp0n7g"; "u9dp0n77"; "u9dp0n7u";
            "u9dp0n7k"; "u9dp0n7f"; "u9dp0n76"]
          ];
        id = None; start_hash = "u9dp0n7";
        coarse_hash =
        ["u9dp0n7"; "u9dp0n6"; "u9dp0j2"; "u99zpud"; "u99zptn"; "u99zpqw";
          "u99zpyf"; "u9dp0n8"; "u9dp0n7"]
        };
      } |}];
  let deserialized = simple_deserialize json in
  printf "Activity after: %s" (simple_show deserialized);
  [%expect
    {|
      Activity after: {
      	id=1;
      	sport_type=RockClimbing;
      	start_date=Start date;
      	route={ hash =
        [["u9dp0n7s"; "u9dp0n7t"; "u9dp0n7e"; "u9dp0n7u"; "u9dp0n7k"; "u9dp0n7v";
           "u9dp0n7m"; "u9dp0n7g"; "u9dp0n77"];
          ["u9dp0n6q"; "u9dp0n6r"; "u9dp0n6m"; "u9dp0n6w"; "u9dp0n6n"; "u9dp0n6x";
            "u9dp0n6p"; "u9dp0n6t"; "u9dp0n6j"];
          ["u9dp0j2r"; "u9dp0j82"; "u9dp0j2q"; "u9dp0j2x"; "u9dp0j2p"; "u9dp0j88";
            "u9dp0j80"; "u9dp0j2w"; "u9dp0j2n"];
          ["u99zpvnw"; "u99zpvnx"; "u99zpvnt"; "u99zpvny"; "u99zpvnq"; "u99zpvnz";
            "u99zpvnr"; "u99zpvnv"; "u99zpvnm"];
          ["u99zpvh2"; "u99zpvh3"; "u99zpuur"; "u99zpvh8"; "u99zpvh0"; "u99zpvh9";
            "u99zpvh1"; "u99zpuux"; "u99zpuup"];
          ["u99zpudq"; "u99zpudr"; "u99zpudm"; "u99zpudw"; "u99zpudn"; "u99zpudx";
            "u99zpudp"; "u99zpudt"; "u99zpudj"];
          ["u99zpu9x"; "u99zpuc8"; "u99zpu9w"; "u99zpu9z"; "u99zpu9r"; "u99zpucb";
            "u99zpuc2"; "u99zpu9y"; "u99zpu9q"];
          ["u99zptnb"; "u99zptnc"; "u99zpsyz"; "u99zptp0"; "u99zptn8"; "u99zptp1";
            "u99zptn9"; "u99zpszp"; "u99zpsyx"];
          ["u99zptnw"; "u99zptnx"; "u99zptnt"; "u99zptny"; "u99zptnq"; "u99zptnz";
            "u99zptnr"; "u99zptnv"; "u99zptnm"];
          ["u99zptm1"; "u99zptm4"; "u99zptm0"; "u99zptm3"; "u99zptkc"; "u99zptm6";
            "u99zptkf"; "u99zptm2"; "u99zptkb"];
          ["u99zptep"; "u99zptg0"; "u99zpten"; "u99zpter"; "u99zptdz"; "u99zptg2";
            "u99zptfb"; "u99zpteq"; "u99zptdy"];
          ["u99zpqqw"; "u99zpqqx"; "u99zpqqt"; "u99zpqqy"; "u99zpqqq"; "u99zpqqz";
            "u99zpqqr"; "u99zpqqv"; "u99zpqqm"];
          ["u99zpqwc"; "u99zpqwf"; "u99zpqwb"; "u99zpqx1"; "u99zpqw9"; "u99zpqx4";
            "u99zpqwd"; "u99zpqx0"; "u99zpqw8"];
          ["u99zpw82"; "u99zpw83"; "u99zpw2r"; "u99zpw88"; "u99zpw80"; "u99zpw89";
            "u99zpw81"; "u99zpw2x"; "u99zpw2p"];
          ["u99zpybd"; "u99zpybe"; "u99zpyb9"; "u99zpybf"; "u99zpyb6"; "u99zpybg";
            "u99zpyb7"; "u99zpybc"; "u99zpyb3"];
          ["u99zpyfe"; "u99zpyfs"; "u99zpyfd"; "u99zpyfg"; "u99zpyf7"; "u99zpyfu";
            "u99zpyfk"; "u99zpyff"; "u99zpyf6"];
          ["u99zpyxv"; "u99zpyxy"; "u99zpyxu"; "u9dp0n8j"; "u99zpyxt"; "u9dp0n8n";
            "u99zpyxw"; "u9dp0n8h"; "u99zpyxs"];
          ["u9dp0n8y"; "u9dp0n8z"; "u9dp0n8v"; "u9dp0n9n"; "u9dp0n8w"; "u9dp0n9p";
            "u9dp0n8x"; "u9dp0n9j"; "u9dp0n8t"];
          ["u9dp0ndd"; "u9dp0nde"; "u9dp0nd9"; "u9dp0ndf"; "u9dp0nd6"; "u9dp0ndg";
            "u9dp0nd7"; "u9dp0ndc"; "u9dp0nd3"];
          ["u9dp0n6q"; "u9dp0n6r"; "u9dp0n6m"; "u9dp0n6w"; "u9dp0n6n"; "u9dp0n6x";
            "u9dp0n6p"; "u9dp0n6t"; "u9dp0n6j"];
          ["u9dp0n7e"; "u9dp0n7s"; "u9dp0n7d"; "u9dp0n7g"; "u9dp0n77"; "u9dp0n7u";
            "u9dp0n7k"; "u9dp0n7f"; "u9dp0n76"]
          ];
        id = None; start_hash = "u9dp0n7";
        coarse_hash =
        ["u9dp0n7"; "u9dp0n6"; "u9dp0j2"; "u99zpud"; "u99zptn"; "u99zpqw";
          "u99zpyf"; "u9dp0n8"; "u9dp0n7"]
        };
      } |}]
