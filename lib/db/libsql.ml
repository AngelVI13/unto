open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type argType = Null | Integer | Float | Text | Blob
[@@deriving show { with_path = false }]

let yojson_of_argType t =
  let str = show_argType t |> String.lowercase in
  `String str

let argType_of_yojson (json : Yojson.Safe.t) : argType =
  match json with
  | `String s -> (
      match String.lowercase s with
      | "null" -> Null
      | "integer" -> Integer
      | "float" -> Float
      | "text" -> Text
      | "blob" -> Blob
      | _ -> failwith ("Unknown argType: " ^ s))
  | _ -> failwith "Expected a string for argType"

type requestType = Execute | Close [@@deriving show { with_path = false }]

let yojson_of_requestType t =
  let str = show_requestType t |> String.lowercase in
  `String str

let requestType_of_yojson (json : Yojson.Safe.t) : requestType =
  match json with
  | `String s -> (
      match String.lowercase s with
      | "execute" -> Execute
      | "close" -> Close
      | _ -> failwith ("Unknown requestType: " ^ s))
  | _ -> failwith "Expected a string for requestType"

module ArgValue = struct
  type t = { type_ : argType; [@key "type"] value : string }
  [@@deriving show { with_path = false }, yojson]

  let make ~type_ ~value = { type_; value }
end

module NamedArg = struct
  type t = { name : string; value : ArgValue.t }
  [@@deriving show { with_path = false }, yojson]

  let make ~name ~value = { name; value }
end

module Stmt = struct
  type t = { sql : string; named_args : NamedArg.t list }
  [@@deriving show { with_path = false }, yojson]

  let make ~sql ~named_args = { sql; named_args }
end

module Request = struct
  type t = { type_ : requestType; [@key "type"] stmt : Stmt.t }
  [@@deriving show { with_path = false }, yojson]

  let make ?(type_ = Execute) stmt = { type_; stmt }
end

module Requests = struct
  type t = { requests : Request.t list }
  [@@deriving show { with_path = false }, yojson]

  let make stmt = { requests = [ Request.make stmt ] }

  let to_json_string t =
    let json = yojson_of_t t in
    Yojson.Safe.to_string json
end

(* NOTE: Turso JSON Response Schema *)
module ResultCol = struct
  type t = { name : string; decltype : string }
  [@@deriving show { with_path = false }, yojson]
end

module ResultColValue = struct
  type t = {
    type_ : string; [@key "type"]
    value : string option; [@yojson.option]
  }
  [@@deriving show { with_path = false }, yojson_of]

  (* NOTE: Custom converter is needed because Turso returns everything as
     string except floats and nulls *)
  let t_of_yojson (json : Yojson.Safe.t) : t =
    let open Yojson.Safe.Util in
    let type_ = json |> member "type" |> to_string in
    let value =
      match json |> member "value" with
      | `String s -> Some s
      | `Float f -> Some (Float.to_string f)
      | `Null -> None
      | _ -> failwith "unexpected type of row.value"
    in
    { type_; value }
end

module ResultRow = struct
  type t = ResultColValue.t list [@@deriving show { with_path = false }, yojson]
end

module ResultRowsCols = struct
  type t = { cols : ResultCol.t list; rows : ResultRow.t list }
  [@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]
end

module ResultResp = struct
  type t = { type_ : string; [@key "type"] result : ResultRowsCols.t }
  [@@deriving show { with_path = false }, yojson]
end

module Result = struct
  type t = { type_ : string; [@key "type"] response : ResultResp.t }
  [@@deriving show { with_path = false }, yojson]
end

module Response = struct
  type t = { results : Result.t list }
  [@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

  let rows (t : t) =
    match List.nth t.results 0 with
    | None -> []
    | Some r -> r.response.result.rows
end
