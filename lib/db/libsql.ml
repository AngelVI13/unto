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
