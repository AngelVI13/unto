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
  [@@deriving yojson]

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
  Yojson.Safe.from_string out

let obtain_access_token auth_client auth_code filename =
  let json = request_access_token auth_client auth_code in
  (* TODO: verify if this returns correct data
      In case of error response the data looks like this:
      {"message":"Bad Request","errors":[{"resource":"AuthorizationCode","field":"code","code":"invalid"}]}
  *)
  printf "Successfully obtained access_token\n%s\n" (Yojson.Safe.to_string json);
  Yojson.Safe.to_file filename json;
  printf "Saved data to file %s\n" filename

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
  Yojson.Safe.from_string out

let load_and_refresh_tokens auth_client filename =
  match Sys_unix.file_exists filename with
  | `Yes ->
      let contents = Yojson.Safe.from_file filename in
      let auth =
        try Auth.t_of_yojson contents
        with _ ->
          failwith (sprintf "failed to serialize token file to Auth.t")
      in
      let now = Time_ns.now () in
      (* convert from ns to s *)
      let now_since_epoch = Time_ns.to_int_ns_since_epoch now / 1000_000_000 in
      let auth =
        if Int.(auth.expires_at < now_since_epoch) then (
          printf "ACCESS_TOKEN expired -> refreshing it\n";
          let json = refresh_token auth_client auth.refresh_token in
          printf "Response from Strava:\n%s\n" (Yojson.Safe.to_string json);
          let auth =
            try Auth.t_of_yojson json
            with _ ->
              failwith (sprintf "failed to serialize new tokens json to Auth.t")
          in
          Yojson.Safe.to_file filename json;
          printf "Saved new ACCESS_TOKEN to file: %s\n" filename;
          auth)
        else (
          printf "ACCESS_TOKEN still valid\n";
          auth)
      in

      Ok auth
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

let access_token auth_client auth_filename =
  match load_and_refresh_tokens auth_client auth_filename with
  | Ok auth -> auth.access_token
  | Error s -> failwith (Error.to_string_hum s)
