open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module AuthClient = struct
  type t = { client_id : string; client_secret : string }
  [@@deriving show { with_path = false }]

  let make client_id client_secret = { client_id; client_secret }
end

module Auth = struct
  type t = {
    token_type : string;
    access_token : string;
    expires_at : int;
    expires_in : int;
    refresh_token : string;
  }
  [@@deriving yojson] [@@yojson.allow_extra_fields]

  let empty () =
    {
      token_type = "Bearer";
      access_token = "ACCESS_TOKEN";
      expires_at = -1;
      expires_in = 0;
      refresh_token = "REFRESH_TOKEN";
    }
end

let auth_params (auth_client : AuthClient.t) =
  [
    Curl.CURLFORM_CONTENT ("client_id", auth_client.client_id, Curl.DEFAULT);
    Curl.CURLFORM_CONTENT
      ("client_secret", auth_client.client_secret, Curl.DEFAULT);
  ]

let request_access_token auth_client auth_code =
  let url = "https://www.strava.com/oauth/token" in
  let params =
    auth_params auth_client
    @ [
        Curl.CURLFORM_CONTENT ("code", auth_code, Curl.DEFAULT);
        Curl.CURLFORM_CONTENT ("grant_type", "authorization_code", Curl.DEFAULT);
      ]
  in
  let res = Ezcurl.post ~params ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  out

let validate_token_resp resp =
  let json = Yojson.Safe.from_string resp in
  match Yojson.Safe.Util.member "access_token" json with
  | `Null ->
      Or_error.error_s
        [%message "Did not receive access token from strava" (resp : string)]
  | exception Yojson.Safe.Util.Type_error (err_msg, _) ->
      Or_error.error_s
        [%message
          "Did not receive valid json from strava"
            (err_msg : string)
            (resp : string)]
  | token_member ->
      let token = Yojson.Safe.Util.to_string token_member in
      printf "Successfully obtained access_token=%s\n%s\n" token resp;
      Ok json

let obtain_access_token auth_client auth_code filename =
  let open Or_error.Let_syntax in
  let resp = request_access_token auth_client auth_code in
  let%bind json = validate_token_resp resp in
  Yojson.Safe.to_file filename json;
  printf "Saved data to file %s\n" filename;
  Ok ()

let refresh_token auth_client refresh_token =
  let url = "https://www.strava.com/oauth/token" in
  let params =
    auth_params auth_client
    @ [
        Curl.CURLFORM_CONTENT ("grant_type", "refresh_token", Curl.DEFAULT);
        Curl.CURLFORM_CONTENT ("refresh_token", refresh_token, Curl.DEFAULT);
      ]
  in
  let res = Ezcurl.post ~params ~url () in
  let out = match res with Ok c -> c.body | Error (_, s) -> failwith s in
  out

let load_token_file filename =
  match Sys_unix.file_exists filename with
  | `Yes -> Or_error.try_with (fun () -> Yojson.Safe.from_file filename)
  | _ ->
      let url =
        "https://www.strava.com/oauth/authorize?client_id=21710&response_type=code&redirect_uri=http://localhost/exchange_token&approval_prompt=force&scope=read_all,activity:read_all"
      in
      printf "Paste the URL in your browser and Authorize the app: %S\n" url;
      printf
        "Afterwards wait for the browser to redirect you and copy the code \
         after `auth_code` parameter from the url\n";
      printf
        "Next to obtain an access & refresh tokens execute the script:\n\
        \ ./bin/main.exe obtain-access-token --auth-code AUTH_CODE\n";
      Or_error.error_s [%message "Missing tokens"]

let load_and_refresh_tokens auth_client filename =
  let open Or_error.Let_syntax in
  let%bind contents =
    Or_error.try_with (fun () -> Yojson.Safe.from_file filename)
  in
  let%bind auth = Or_error.try_with (fun () -> Auth.t_of_yojson contents) in

  let now_since_epoch =
    (* convert it from ns to s *)
    (Time_ns.now () |> Time_ns.to_int_ns_since_epoch) / 1000_000_000
  in
  printf "%d -- %d : %b\n" auth.expires_at now_since_epoch
    Int.(auth.expires_at < now_since_epoch);
  match Int.(auth.expires_at < now_since_epoch) with
  | false ->
      printf "ACCESS_TOKEN still valid\n";
      Ok auth
  | true ->
      printf "ACCESS_TOKEN expired -> refreshing it\n";
      let resp = refresh_token auth_client auth.refresh_token in
      let%bind json = validate_token_resp resp in
      let%bind auth = Or_error.try_with (fun () -> Auth.t_of_yojson json) in
      Yojson.Safe.to_file filename json;
      printf "Saved new ACCESS_TOKEN to file: %s\n" filename;
      Ok auth

(* let access_token auth_client auth_filename = *)
(**)
(*   | Ok auth -> auth.access_token *)
(*   | Error s -> failwith (Error.to_string_hum s) *)
