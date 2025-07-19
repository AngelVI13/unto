open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Split = struct
  type t = { start : int; len : int; stats : Stats.t }
  [@@deriving show { with_path = false }, yojson]
end

module Splits = struct
  type t = Split.t list [@@deriving show { with_path = false }, yojson]
end
