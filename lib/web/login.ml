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
          div
            [ class_ "loginDiv card" ]
            [
              div
                [ class_ "loginHeader" ]
                [
                  img
                    [
                      class_ "loginLogo";
                      path_attr src Static.Assets.Images.favicon_small_png;
                    ];
                  h1 [] [ txt "Unto" ];
                ];
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
                    [
                      class_ "formEl";
                      type_ "text";
                      name "username";
                      placeholder "Username";
                    ];
                  input
                    [
                      class_ "formEl";
                      type_ "password";
                      name "password";
                      placeholder "Password";
                    ];
                  button
                    [ class_ "submitBtn formEl"; type_ "submit" ]
                    [ txt "Login" ];
                ];
            ];
        ];
    ]
