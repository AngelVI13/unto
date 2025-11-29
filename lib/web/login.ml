open Dream_html
open HTML

let page csrf_token =
  html
    [ lang "en" ]
    [
      head []
        [
          meta [ http_equiv `content_type; content "text/html; charset=UTF-8" ];
          meta [ charset "UTF-8" ];
          meta
            [ name "viewport"; content "width=device-width, initial-scale=1.0" ];
          title [] "Unto";
          link
            [
              rel "icon";
              type_ "image/png";
              path_attr href Static.Assets.Images.favicon_small_png;
            ];
          link
            [
              rel "stylesheet";
              type_ "text/css";
              path_attr href Static.Assets.Css.common_css;
            ];
          link
            [
              rel "stylesheet";
              type_ "text/css";
              path_attr href Static.Assets.Css.header_css;
            ];
          link
            [
              rel "stylesheet";
              type_ "text/css";
              path_attr href Static.Assets.Css.login_css;
            ];
        ];
      body []
        [
          Header.header_ ~athlete_name:"" ();
          div
            [ class_ "loginDiv card" ]
            [
              form
                [
                  class_ "loginForm";
                  method_ `POST;
                  path_attr action Paths.login;
                ]
                [
                  input
                    [ type_ "hidden"; name "dream.csrf"; value "%s" csrf_token ];
                  input
                    [ type_ "text"; name "username"; placeholder "Username" ];
                  input
                    [
                      type_ "password"; name "password"; placeholder "Password";
                    ];
                  button [ type_ "submit" ] [ txt "Login" ];
                ];
            ];
        ];
    ]
