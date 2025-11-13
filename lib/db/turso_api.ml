open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module DbResponse = struct
  type t = {
    db_id : string; [@key "DbId"]
    hostname : string; [@key "Hostname"]
    name : string; [@key "Name"]
  }
  [@@deriving show, yojson] [@@yojson.allow_extra_fields]
end

module CreateDbResponse = struct
  type t = { database : DbResponse.t }
  [@@deriving show, yojson] [@@yojson.allow_extra_fields]
end

module ErrorResponse = struct
  type t = { error : string }
  [@@deriving show, yojson] [@@yojson.allow_extra_fields]
end

(* TODO: implement Create Token & Invalidate Tokens & Delete Turso API endpoints  *)
let create (name : string) =
  let token = Sys.getenv_exn "TURSO_API_TOKEN" in
  let org_slug = Sys.getenv_exn "TURSO_ORG_SLUG" in
  printf "%s\n" token;
  printf "%s\n" org_slug;
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
  printf "%s\n" content;
  let _ = (headers, url) in
  (* let content = `String content in *)
  (* let res = Ezcurl.post ~headers ~url ~content ~params:[] () in *)
  (* let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in *)
  (* let out = *)
  (*   {| *)
  (*     { *)
  (*       "code":"invalid_name", *)
  (*       "error":"invalid database name: name may only contain numbers, lowercase letters, and dashes" *)
  (*     }  *)
  (* |} *)
  (* in *)
  let out =
    {|
{"database":{"DbId":"5942c0a0-49f2-4887-af5c-bc2e06b16149","Hostname":"new-test-db-angelvi13.aws-eu-west-1.turso.io","IssuedCertCount":0,"IssuedCertLimit":0,"Name":"new-test-db"},"password":"","username":"angelvi13"}
    |}
  in
  let out = Yojson.Safe.from_string out in
  (* printf "%s\n" (Yojson.Safe.to_string out); *)

  match CreateDbResponse.t_of_yojson out with
  | t -> Ok t.database
  | exception _ ->
      let err = ErrorResponse.t_of_yojson out in
      Or_error.error_string err.error
