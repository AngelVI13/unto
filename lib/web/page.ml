open Core

let head_elems () =
  let open Dream_html in
  let open HTML in
  [
    Dream_html.Livereload.script;
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

let weekTableHeader () =
  let open Dream_html in
  let open HTML in
  let days_of_the_week =
    [
      "Monday";
      "Tuesday";
      "Wednesday";
      "Thursday";
      "Friday";
      "Saturday";
      "Sunday";
    ]
  in
  let divs =
    List.map
      ~f:(fun name -> div [ class_ "dayOfTheWeek card" ] [ txt "%s" name ])
      days_of_the_week
  in
  div [ class_ "days" ] divs

let page (athlete : Models.Strava_models.StravaAthlete.t option) =
  let open Dream_html in
  let open HTML in
  let athlete =
    match athlete with None -> "Unknown" | Some athl -> athl.firstname
  in
  html
    [ lang "en" ]
    [ head [] (head_elems ()); body [] [ header_ athlete; weekTableHeader () ] ]
