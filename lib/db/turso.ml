open Core

(* TODO: this is taken from:
  https://github.com/ygrek/sqlgg/blob/master/impl/ocaml/sqlite3/sqlgg_sqlite3.ml
  . with the gpt examples from here:
    https://chatgpt.com/s/t_68f34cc041a4819187f3be324feb99d6 -> remove the
    sqlite stuff and replace with the turso stuff. *)

type one_or_all = [ `One | `All ]

let rec convert_json = function
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as x -> x
  | `Assoc assoc_list ->
      let convert_pair (key, value) = (key, convert_json value) in
      `Assoc (List.map ~f:convert_pair assoc_list)
  | `List json_list | `Tuple json_list ->
      `List (List.map ~f:convert_json json_list)
  | `Variant _ -> failwith "Variant type is not supported"

module M = struct
  module Types = struct
    module Bool = struct
      type t = bool

      let to_literal = string_of_bool
      let bool_to_literal = to_literal
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
        Buffer.add_string b "'";
        for i = 0 to String.length s - 1 do
          match String.unsafe_get s i with
          | '\'' -> Buffer.add_string b "''"
          | c -> Buffer.add_char b c
        done;
        Buffer.add_string b "'";
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
    end

    (* you probably want better type, e.g. (int*int) or Z.t *)
    module Decimal = Float
    module Datetime = Float (* ? *)

    module One_or_all = struct
      type t = one_or_all

      let to_literal = function `One -> "one" | `All -> "all"
      let one_or_all_to_literal = to_literal
    end

    module Any = Text
  end

  open Types

  type statement = string
  type 'a connection = { url : string; token : string }
  type params = string list ref
  type row = Yojson.Safe.t
  type result = string list
  type execute_response = { affected_rows : int64; insert_id : int64 option }
  type num = int64
  type text = string
  type any = string
  type datetime = float

  exception Oops of string

  let extract_field row idx =
    match row with
    | `List lst when idx < List.length lst -> List.nth_exn lst idx
    | _ -> raise (Oops "Invalid row format")

  let get_column_Text row i =
    match extract_field row i with
    | `String s -> s
    | _ -> raise (Oops "Expected text")

  let get_column_Text_nullable row i =
    match extract_field row i with
    | `Null -> None
    | `String s -> Some s
    | _ -> raise (Oops "Expected text")

  let get_column_Int row i =
    match extract_field row i with
    | `Intlit s -> Int64.of_string s
    | `Int n -> Int64.of_int n
    | _ -> raise (Oops "Expected int")

  let get_column_Int_nullable row i =
    match extract_field row i with
    | `Null -> None
    | `Intlit s -> Some (Int64.of_string s)
    | `Int n -> Some (Int64.of_int n)
    | _ -> raise (Oops "Expected int")

  let get_column_Bool row i =
    match extract_field row i with
    | `Bool b -> b
    | _ -> raise (Oops "Expected bool")

  let get_column_Bool_nullable row i =
    match extract_field row i with
    | `Null -> None
    | `Bool b -> Some b
    | _ -> raise (Oops "Expected bool")

  let get_column_Any, get_column_Any_nullable =
    (get_column_Text, get_column_Text_nullable)

  (* get columns *)
  let get_column_Float = fun _ _ -> 0.0
  let get_column_Float_nullable = fun _ _ -> None
  let get_column_Decimal = fun _ _ -> 0.0
  let get_column_Decimal_nullable = fun _ _ -> None
  let get_column_Datetime = fun _ _ -> 0.0
  let get_column_Datetime_nullable = fun _ _ -> None

  (* set params *)
  let start_params _stmt _n = ref []
  let finish_params params = !params
  let set_param_Text params v = params := Text.to_literal v :: !params
  let set_param_Int params v = params := Int.to_literal v :: !params
  let set_param_Bool params v = params := Bool.to_literal v :: !params
  let set_param_Float params v = params := Float.to_literal v :: !params
  let set_param_null params = params := "NULL" :: !params
  let set_param_Any = set_param_Text
  let set_param_Decimal = set_param_Float
  let set_param_Datetime = set_param_Float
  let no_params _ = []

  let turso_post db sql params =
    (* placeholder: call Turso HTTP API here *)
    (* return JSON response as Yojson.Safe.t list *)
    (* Str. *)
    let regexp = Re.Perl.re {|\@(\w+)|} |> Re.compile in
    let matches =
      Re.all regexp sql |> List.map ~f:(fun group -> Re.Group.get group 1)
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
    let _ = db in
    printf "%s\n" sql;
    List.iter ~f:(fun x -> Printf.printf "%s," x) params;
    printf "\n";
    List.iter ~f:(fun x -> Printf.printf "%s|" x) matches;
    printf "\n";
    []

  let select db sql set_params callback =
    (* NOTE: if there is a `?` then we send unnamed arguments  *)
    (* if there is a @name then we send named argument.  *)
    (* Each param responds to the each @argument. If named arg appears 3 times
    then we get 3 parameters for it  *)
    let params = set_params sql in
    let rows = turso_post db sql params in
    List.iter ~f:callback rows

  let execute db sql set_params =
    let _ = (db, set_params) in
    let _ = turso_post db sql [] in
    1L

  let select_one_maybe db sql set_params convert =
    let _ = (db, set_params, convert) in
    match turso_post db sql [] with
    | [] -> None
    | row :: _ -> Some (convert row)

  let select_one db sql set_params convert =
    let _ = (db, set_params, convert) in
    match turso_post db sql [] with
    | row :: _ -> convert row
    | [] -> raise (Oops "Expected one row, got zero")
end

let () =
  (* checking signature match *)
  let module S : Sqlgg_traits.M = M in
  ignore (S.Oops "ok")

include M
