open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Split = struct
  type t = { start : int; len : int; stats : Stats.t }
  [@@deriving show { with_path = false }, yojson]

  let empty () = { start = 0; len = 0; stats = Stats.empty () }
  let make ~start ~len = { start; len; stats = Stats.empty () }
  let set_stats (t : t) (stats : Stats.t) : t = { t with stats }
end

module Splits = struct
  type t = Split.t list [@@deriving show { with_path = false }, yojson]

  let empty () : t = []
end
