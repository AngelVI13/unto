open! Core

type argType = Null | Integer | Float | Text | Blob [@@deriving show]
type requestType = Execute | Close [@@deriving show]

module ArgValue = struct
  type t = { type_ : argType; value : string } [@@deriving show]
end

module NamedArg = struct
  type t = { name : string; value : ArgValue.t } [@@deriving show]
end

module Stmt = struct
  type t = { sql : string; named_args : NamedArg.t list } [@@deriving show]
end

module Request = struct
  type t = { type_ : requestType; stmt : Stmt.t } [@@deriving show]
end

module Requests = struct
  type t = Request.t list [@@deriving show]
end
