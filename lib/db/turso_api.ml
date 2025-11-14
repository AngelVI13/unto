open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module DbResponse = struct
  type t = {
    db_id : string; [@key "DbId"]
    hostname : string; [@key "Hostname"]
    name : string; [@key "Name"]
  }
  [@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]
end

module CreateDbResponse = struct
  type t = { database : DbResponse.t }
  [@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]
end

module DeleteDbResponse = struct
  type t = { database : string }
  [@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]
end

module CreateTokensResponse = struct
  type t = { jwt : string }
  [@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]
end

module ErrorResponse = struct
  type t = { error : string }
  [@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]
end

let delete (db_name : string) =
  let token = Sys.getenv_exn "TURSO_API_TOKEN" in
  let org_slug = Sys.getenv_exn "TURSO_ORG_SLUG" in
  let url =
    sprintf "https://api.turso.tech/v1/organizations/%s/databases/%s" org_slug
      db_name
  in

  let headers = [ ("Authorization", sprintf "Bearer %s" token) ] in
  let res = Ezcurl.http ~headers ~url ~meth:Ezcurl.DELETE () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  let out = Yojson.Safe.from_string out in
  match DeleteDbResponse.t_of_yojson out with
  | t -> Ok t
  | exception _ ->
      let err = ErrorResponse.t_of_yojson out in
      Or_error.error_string err.error

let create_tokens db_name =
  let token = Sys.getenv_exn "TURSO_API_TOKEN" in
  let org_slug = Sys.getenv_exn "TURSO_ORG_SLUG" in
  let url =
    sprintf
      "https://api.turso.tech/v1/organizations/%s/databases/%s/auth/tokens?expiration=2w&authorization=full-access"
      org_slug db_name
  in

  let headers = [ ("Authorization", sprintf "Bearer %s" token) ] in
  let res = Ezcurl.post ~headers ~url ~params:[] () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  let out = Yojson.Safe.from_string out in
  match CreateTokensResponse.t_of_yojson out with
  | t -> Ok t.jwt
  | exception _ ->
      let err = ErrorResponse.t_of_yojson out in
      Or_error.error_string err.error

let create (name : string) =
  let token = Sys.getenv_exn "TURSO_API_TOKEN" in
  let org_slug = Sys.getenv_exn "TURSO_ORG_SLUG" in
  let url =
    sprintf "https://api.turso.tech/v1/organizations/%s/databases" org_slug
  in

  let headers =
    [
      ("Authorization", sprintf "Bearer %s" token);
      ("Content-Type", "application/json");
    ]
  in
  let content =
    `Assoc [ ("name", `String name); ("group", `String "default") ]
    |> Yojson.Safe.to_string
  in
  let content = `String content in
  let res = Ezcurl.post ~headers ~url ~content ~params:[] () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  let out = Yojson.Safe.from_string out in

  match CreateDbResponse.t_of_yojson out with
  | t -> Ok t.database
  | exception _ ->
      let err = ErrorResponse.t_of_yojson out in
      Or_error.error_string err.error
