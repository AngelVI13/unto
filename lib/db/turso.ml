open Core

let test_response1 =
  {|
{
  "baton": null,
  "base_url": null,
  "results": [
    {
      "type": "ok",
      "response": {
        "type": "execute",
        "result": {
          "cols": [
            {
              "name": "COUNT(*)",
              "decltype": null
            }
          ],
          "rows": [
            [
              {
                "type": "integer",
                "value": "1"
              }
            ]
          ],
          "affected_row_count": 0,
          "last_insert_rowid": null,
          "replication_index": null,
          "rows_read": 1,
          "rows_written": 0,
          "query_duration_ms": 147.602
        }
      }
    }
  ]
}
  |}

let test_response =
  {|
{
  "baton": null,
  "base_url": null,
  "results": [
    {
      "type": "ok",
      "response": {
        "type": "execute",
        "result": {
          "cols": [
            {
              "name": "id",
              "decltype": "INTEGER"
            },
            {
              "name": "athlete_id",
              "decltype": "INTEGER"
            },
            {
              "name": "name",
              "decltype": "TEXT"
            },
            {
              "name": "sport_type",
              "decltype": "TEXT"
            },
            {
              "name": "start_date",
              "decltype": "TEXT"
            },
            {
              "name": "timezone",
              "decltype": "TEXT"
            },
            {
              "name": "map_id",
              "decltype": "TEXT"
            },
            {
              "name": "map_summary_polyline",
              "decltype": "TEXT"
            },
            {
              "name": "stats_id",
              "decltype": "INTEGER"
            },
            {
              "name": "moving_time",
              "decltype": "INTEGER"
            },
            {
              "name": "elapsed_time",
              "decltype": "INTEGER"
            },
            {
              "name": "distance",
              "decltype": "REAL"
            },
            {
              "name": "elev_gain",
              "decltype": "INTEGER"
            },
            {
              "name": "elev_loss",
              "decltype": "INTEGER"
            },
            {
              "name": "elev_high",
              "decltype": "INTEGER"
            },
            {
              "name": "elev_low",
              "decltype": "INTEGER"
            },
            {
              "name": "start_lat",
              "decltype": "REAL"
            },
            {
              "name": "start_lng",
              "decltype": "REAL"
            },
            {
              "name": "end_lat",
              "decltype": "REAL"
            },
            {
              "name": "end_lng",
              "decltype": "REAL"
            },
            {
              "name": "average_speed",
              "decltype": "REAL"
            },
            {
              "name": "max_speed",
              "decltype": "REAL"
            },
            {
              "name": "average_cadence",
              "decltype": "INTEGER"
            },
            {
              "name": "max_cadence",
              "decltype": "INTEGER"
            },
            {
              "name": "average_temp",
              "decltype": "INTEGER"
            },
            {
              "name": "average_heartrate",
              "decltype": "INTEGER"
            },
            {
              "name": "max_heartrate",
              "decltype": "INTEGER"
            },
            {
              "name": "average_power",
              "decltype": "INTEGER"
            },
            {
              "name": "max_power",
              "decltype": "INTEGER"
            }
          ],
          "rows": [
            [
              {
                "type": "integer",
                "value": "15995758488"
              },
              {
                "type": "integer",
                "value": "3504239"
              },
              {
                "type": "text",
                "value": "Lunch Crossfit"
              },
              {
                "type": "text",
                "value": "Crossfit"
              },
              {
                "type": "text",
                "value": "2025-10-01T11:23:07Z"
              },
              {
                "type": "text",
                "value": "(GMT+03:00) Africa/Addis_Ababa"
              },
              {
                "type": "text",
                "value": "a15995758488"
              },
              {
                "type": "text",
                "value": ""
              },
              {
                "type": "integer",
                "value": "21623"
              },
              {
                "type": "integer",
                "value": "2280"
              },
              {
                "type": "integer",
                "value": "2280"
              },
              {
                "type": "null"
              },
              {
                "type": "integer",
                "value": "0"
              },
              {
                "type": "integer",
                "value": "0"
              },
              {
                "type": "integer",
                "value": "129"
              },
              {
                "type": "integer",
                "value": "129"
              },
              {
                "type": "null"
              },
              {
                "type": "null"
              },
              {
                "type": "null"
              },
              {
                "type": "null"
              },
              {
                "type": "null"
              },
              {
                "type": "null"
              },
              {
                "type": "null"
              },
              {
                "type": "null"
              },
              {
                "type": "integer",
                "value": "28"
              },
              {
                "type": "integer",
                "value": "103"
              },
              {
                "type": "integer",
                "value": "133"
              },
              {
                "type": "null"
              },
              {
                "type": "null"
              }
            ],
            [
              {
                "type": "integer",
                "value": "16020312195"
              },
              {
                "type": "integer",
                "value": "3504239"
              },
              {
                "type": "text",
                "value": "Afternoon Run"
              },
              {
                "type": "text",
                "value": "Run"
              },
              {
                "type": "text",
                "value": "2025-10-03T16:19:17Z"
              },
              {
                "type": "text",
                "value": "(GMT+02:00) Europe/Vilnius"
              },
              {
                "type": "text",
                "value": "a16020312195"
              },
              {
                "type": "text",
                "value": "{{zlIihoyCLHLPPNjBbAp@X^JhAf@H@JFNLd@NJHtBf@fA\\hAh@j@f@`AnA~AvCh@v@hAxB@@H?HFX~@VtA\\hCJ~@Ll@d@hAd@jBZxAdAfBb@x@`@l@NZbCpDVZb@RDJ?XQ`@?FBb@Aj@IdAm@tBGFMGe@pAYLE?IEe@m@[YMU[_@m@c@u@w@y@o@y@_AiBeBa@Y_@c@e@a@{@e@oAy@i@[iAe@kAm@y@[gB]o@EeBWk@AQBw@C_@@sBGoFLSEu@EkAMeAE_AOMEQWKa@k@{@K_@Gg@CgAAqGt@oFj@eGEiAGc@_@uACe@Hc@"
              },
              {
                "type": "integer",
                "value": "21690"
              },
              {
                "type": "integer",
                "value": "1382"
              },
              {
                "type": "integer",
                "value": "1478"
              },
              {
                "type": "float",
                "value": 3655.0
              },
              {
                "type": "integer",
                "value": "32"
              },
              {
                "type": "integer",
                "value": "29"
              },
              {
                "type": "integer",
                "value": "118"
              },
              {
                "type": "integer",
                "value": "90"
              },
              {
                "type": "float",
                "value": 54.702965
              },
              {
                "type": "float",
                "value": 25.317403
              },
              {
                "type": "float",
                "value": 54.702937
              },
              {
                "type": "float",
                "value": 25.317482
              },
              {
                "type": "float",
                "value": 2.647
              },
              {
                "type": "float",
                "value": 4.8
              },
              {
                "type": "integer",
                "value": "81"
              },
              {
                "type": "integer",
                "value": "121"
              },
              {
                "type": "integer",
                "value": "17"
              },
              {
                "type": "integer",
                "value": "143"
              },
              {
                "type": "integer",
                "value": "164"
              },
              {
                "type": "integer",
                "value": "193"
              },
              {
                "type": "integer",
                "value": "331"
              }
            ]
          ],
          "affected_row_count": 0,
          "last_insert_rowid": null,
          "replication_index": null,
          "rows_read": 2474,
          "rows_written": 0,
          "query_duration_ms": 485.327
        }
      }
    }
  ]
}
|}

(* TODO: this is taken from:
  https://github.com/ygrek/sqlgg/blob/master/impl/ocaml/sqlite3/sqlgg_sqlite3.ml
  . with the gpt examples from here:
    https://chatgpt.com/s/t_68f34cc041a4819187f3be324feb99d6 -> remove the
    sqlite stuff and replace with the turso stuff. *)

module M = struct
  module Types = struct
    module Bool = struct
      type t = bool

      let to_literal b = if b then "1" else "0"
      let bool_to_literal = to_literal
      let of_string s = if String.(s = "1") then true else false
    end

    module Int = struct
      include Int64

      let to_literal = to_string
      let int64_to_literal = to_literal
    end

    module UInt64 = struct
      type t = Unsigned.UInt64.t

      let to_literal _ = failwith "uint64 unsupported by sqlite"
      let uint64_to_literal = to_literal
    end

    module Text = struct
      type t = string

      (* cf. https://sqlite.org/lang_expr.html "Literal Values"
        "A string constant is formed by enclosing the string in single quotes
        ('). A single quote within the string can be encoded by putting two
        single quotes in a row - as in Pascal. C-style escapes using the
        backslash character are not supported because they are not standard
        SQL." *)
      let to_literal s =
        let b = Buffer.create (String.length s + (String.length s / 4)) in
        for i = 0 to String.length s - 1 do
          match String.unsafe_get s i with
          | '\'' -> Buffer.add_string b "''"
          | c -> Buffer.add_char b c
        done;
        Buffer.contents b

      let string_to_literal = to_literal
    end

    module Blob = struct
      (* "BLOB literals are string literals containing hexadecimal data and preceded
       by a single "x" or "X" character. Example: X'53514C697465'" *)
      type t = string

      let to_hex =
        [|
          '0';
          '1';
          '2';
          '3';
          '4';
          '5';
          '6';
          '7';
          '8';
          '9';
          'A';
          'B';
          'C';
          'D';
          'E';
          'F';
        |]

      let to_literal s =
        let b = Buffer.create (3 + (String.length s * 2)) in
        Buffer.add_string b "x'";
        for i = 0 to String.length s - 1 do
          let c = Char.to_int (String.unsafe_get s i) in
          Buffer.add_char b (Array.unsafe_get to_hex (c lsr 4));
          Buffer.add_char b (Array.unsafe_get to_hex (c land 0x0F))
        done;
        Buffer.add_string b "'";
        Buffer.contents b
    end

    module Float = struct
      type t = float

      let to_literal = string_of_float
      let float_to_literal = to_literal
      let of_string s = Float.of_string s
    end

    (* you probably want better type, e.g. (int*int) or Z.t *)
    module Decimal = Float
    module Datetime = Float (* ? *)
    module Any = Text
  end

  open Types

  type statement = string
  type 'a connection = { url : string; token : string }
  type params = Libsql.ArgValue.t list ref
  type row = Libsql.ResultColValue.t list
  type result = Libsql.ArgValue.t list
  type execute_response = { affected_rows : int64; insert_id : int64 option }
  type num = int64
  type text = string
  type any = string
  type datetime = float

  exception Oops of string

  let extract_field (row : Libsql.ResultColValue.t list) (idx : int) :
      string option =
    match List.nth row idx with
    | Some col -> col.value
    | None -> raise (Oops "Invalid row format")

  let get_column_Text row i =
    match extract_field row i with
    | Some s -> s
    | None -> raise (Oops "Expected text")

  let get_column_Text_nullable row i = extract_field row i

  let get_column_Int row i =
    match extract_field row i with
    | Some s -> Int64.of_string s
    | None -> raise (Oops "Expected int")

  let get_column_Int_nullable row i =
    match extract_field row i with
    | Some s -> Some (Int64.of_string s)
    | None -> None

  let get_column_Bool row i =
    match extract_field row i with
    | Some s -> Bool.of_string s
    | None -> raise (Oops "Expected bool")

  let get_column_Bool_nullable row i =
    match extract_field row i with
    | Some s -> Some (Bool.of_string s)
    | None -> None

  let get_column_Any, get_column_Any_nullable =
    (get_column_Text, get_column_Text_nullable)

  let get_column_Float row i =
    match extract_field row i with
    | Some s -> Float.of_string s
    | None -> raise (Oops "Expected float")

  let get_column_Float_nullable row i =
    match extract_field row i with
    | Some s -> Some (Float.of_string s)
    | None -> None

  let get_column_Decimal, get_column_Decimal_nullable =
    (get_column_Float, get_column_Float_nullable)

  let get_column_Datetime, get_column_Datetime_nullable =
    (get_column_Float, get_column_Float_nullable)

  (* set params *)
  let start_params _stmt _n = ref []
  let finish_params params = !params

  let set_param_Text params v =
    params :=
      Libsql.ArgValue.make ~type_:Libsql.Text ~value:(Text.to_literal v)
      :: !params

  let set_param_Int params v =
    params :=
      Libsql.ArgValue.make ~type_:Libsql.Integer ~value:(Int.to_literal v)
      :: !params

  let set_param_Bool params v =
    params :=
      Libsql.ArgValue.make ~type_:Libsql.Integer ~value:(Bool.to_literal v)
      :: !params

  let set_param_Float params v =
    params :=
      Libsql.ArgValue.make ~type_:Libsql.Float ~value:(Float.to_literal v)
      :: !params

  let set_param_null params =
    params := Libsql.ArgValue.make ~type_:Libsql.Null ~value:"" :: !params

  (* TODO: are these correct ? *)
  let set_param_Any = set_param_Text
  let set_param_Decimal = set_param_Float
  let set_param_Datetime = set_param_Float
  let no_params _ = []

  let make_turso_request token content =
    let url = "https://testapp-angelvi13.aws-eu-west-1.turso.io/v2/pipeline" in

    let headers =
      [
        ("Authorization", sprintf "Bearer %s" token);
        ("Content-Type", "application/json");
      ]
    in
    let res = Ezcurl.post ~headers ~url ~content ~params:[] () in
    let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
    out

  let turso_post db sql params =
    let token = Sys.getenv_exn "TURSO_DB_TOKEN" in
    printf "%s\n" token;
    (* placeholder: call Turso HTTP API here *)
    (* return JSON response as Yojson.Safe.t list *)
    (* Str. *)
    let regexp = Re.Perl.re {|\@(\w+)|} |> Re.compile in
    let matches =
      Re.all regexp sql
      |> List.map ~f:(fun group -> Re.Group.get group 1)
      |> List.rev
    in
    let found_question =
      String.find sql ~f:(fun char -> Char.(char = of_string "?"))
    in
    let _ =
      match found_question with
      | Some _ ->
          failwith "SQL contains ? -> not supported, use named arguments @ARG"
      | None -> ()
    in
    let named_args =
      List.fold2_exn ~init:[]
        ~f:(fun acc name value -> Libsql.NamedArg.make ~name ~value :: acc)
        matches params
    in
    let stmt = Libsql.Stmt.make ~sql ~named_args in
    let request = Libsql.Requests.make stmt in
    (* printf "%s\n" sql; *)
    printf "%s\n" (Libsql.Requests.show request);
    printf "%s\n" (Libsql.Requests.to_json_string request);
    printf "\n";
    let _ = db in
    (* let resp = *)
    (*   make_turso_request token *)
    (*     (`String (Libsql.Requests.to_json_string request)) *)
    (* in *)
    (* printf "%s" resp; *)
    (* let resp = Yojson.Safe.from_string resp |> Libsql.Response.t_of_yojson in *)
    let resp = Yojson.Safe.from_string test_response1 in
    printf "-------------\n";
    let resp = resp |> Libsql.Response.t_of_yojson in
    printf "+++++++++++++\n";
    printf "\n\n%s\n\n" (Libsql.Response.show resp);

    let rows = Libsql.Response.rows resp in
    rows

  let select db sql set_params callback =
    (* NOTE: if there is a `?` then we send unnamed arguments
       if there is a @name then we send named argument.
       Each param responds to the each @argument. If named arg appears 3 times
       then we get 3 parameters for it  *)
    let params = set_params sql in
    let rows = turso_post db sql params in
    (* TODO: for some reason the map_summary_polyline has extra escape backslashes -> does it need fixing ? *)
    List.iter ~f:callback rows

  (* TODO: TEST all of these *)
  let execute db sql set_params =
    let params = set_params sql in
    ignore (turso_post db sql params);
    1L

  let select_one_maybe db sql set_params convert =
    let params = set_params sql in
    match turso_post db sql params with
    | [] -> None
    | row :: _ -> Some (convert row)

  let select_one db sql set_params convert =
    let params = set_params sql in
    match turso_post db sql params with
    | row :: _ -> convert row
    | [] -> raise (Oops "Expected one row, got zero")
end

let () =
  (* checking signature match *)
  let module S : Sqlgg_traits.M = M in
  ignore (S.Oops "ok")

include M
