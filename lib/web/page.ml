let head_elems () =
  let open Dream_html in
  let open HTML in
  [
    meta [ http_equiv `content_type; content "text/html; charset=UTF-8" ];
    meta [ charset "UTF-8" ];
    meta [ name "viewport"; content "width=device-width, initial-scale=1.0" ];
    title [] "Unto";
    link
      [ rel "stylesheet"; type_ "text/css"; href "/static/styles/styles.css" ];
  ]

let header_ (athlete_name : string) =
  let open Dream_html in
  let open HTML in
  header
    [ class_ "headerMargin" ]
    [
      div
        [ class_ "headerTabs" ]
        [
          h1 [ class_ "active" ] [ a [ href "" ] [ txt "Training Log" ] ];
          h1 [ class_ "inactive" ] [ a [ href "" ] [ txt "Dashboards" ] ];
        ];
      div
        [ class_ "headerSettings" ]
        [
          span
            [ class_ "icon-container" ]
            [
              a
                [ href "" ]
                [
                  img [ class_ "header-img"; src "/static/assets/account.png" ];
                ];
            ];
          span [ class_ "icon-container athlete-txt" ] [ txt "%s" athlete_name ];
          span
            [ class_ "icon-container" ]
            [
              a
                [ href "" ]
                [
                  img [ class_ "header-img"; src "/static/assets/settings.png" ];
                ];
            ];
        ];
    ]

let page (athlete : Models.Strava_models.StravaAthlete.t option) =
  let open Dream_html in
  let open HTML in
  let athlete =
    match athlete with None -> "Unknown" | Some athl -> athl.firstname
  in
  html [ lang "en" ] [ head [] (head_elems ()); body [] [ header_ athlete ] ]
