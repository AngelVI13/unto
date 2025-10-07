open Dream_html
open HTML

let update_icon ?(updated_items_num = None) () =
  let overlay_txt =
    match updated_items_num with
    | None -> ""
    | Some new_activities ->
        (* TODO: this value should be shown in red to indicate errors *)
        if new_activities < 0 then "e"
        else if new_activities = 0 then "-"
        else if new_activities > 9 then "+"
        else Int.to_string new_activities
  in
  span
    [ class_ "icon-container"; id "update-icon" ]
    [
      a
        [
          href "";
          (* NOTE: the htmx attributes are on the `a` cause
             otherwise the `a` captures the clicks and the htmx
             doesnt get executed if we click on the outer span
             for example. *)
          Hx.trigger "click";
          path_attr Hx.get Paths.update;
          Hx.target "#update-icon";
          Hx.swap "outerHTML";
          Hx.indicator "#update-spinner";
        ]
        [
          div
            [ class_ "header-img-wrapper" ]
            [
              img
                [
                  class_ "header-img update-indicator";
                  id "update-spinner";
                  path_attr src Static.Assets.Images.refresh_png;
                ];
              span [ class_ "header-img-txt" ] [ txt "%s" overlay_txt ];
            ];
        ];
    ]

let header_ (athlete_name : string) =
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
          (if athlete_name = "" then span [] [] else update_icon ());
          span
            [ class_ "icon-container" ]
            [
              a
                [ href "" ]
                [
                  img
                    [
                      class_ "header-img";
                      path_attr src Static.Assets.Images.account_png;
                    ];
                ];
            ];
          (if athlete_name = "" then span [] []
           else
             span
               [ class_ "icon-container athlete-txt" ]
               [ txt "%s" athlete_name ]);
          (if athlete_name = "" then span [] []
           else
             span
               [ class_ "icon-container" ]
               [
                 a
                   [ path_attr href Paths.logout ]
                   [
                     img
                       [
                         class_ "header-img";
                         path_attr src Static.Assets.Images.logout_png;
                       ];
                   ];
               ]);
        ];
    ]
