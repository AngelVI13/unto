let header_ (athlete_name : string) =
  let open Dream_html in
  let open HTML in
  header
    [ class_ "headerMargin" ]
    [
      div
        [ class_ "headerTabs" ]
        [
          span
            [ class_ "headerTxt headerTxtBig active" ]
            [ a [ path_attr href Paths.index ] [ txt "Training Log" ] ];
          span
            [ class_ "headerTxt headerTxtSmall active" ]
            [ a [ path_attr href Paths.index ] [ txt "Training Log" ] ];
          span
            [ class_ "headerTxt headerTxtBig inactive" ]
            [ a [ href "" ] [ txt "Dashboards" ] ];
          span
            [ class_ "headerTxt headerTxtSmall inactive" ]
            [ a [ href "" ] [ txt "Dashboards" ] ];
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
