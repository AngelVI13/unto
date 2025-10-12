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

type selectedTab = TrainingLog | Calendar | Dashboard [@@deriving eq]

(* TODO: this should collapse down to hamburger menu on small screens *)
let header_ ?(selected = TrainingLog) ~(athlete_name : string) () =
  let txtLargeActive = class_ "headerTxt headerTxtLarge active" in
  let txtLargeInactive = class_ "headerTxt headerTxtLarge inactive" in
  let txtMediumActive = class_ "headerTxt headerTxtMedium active" in
  let txtMediumInactive = class_ "headerTxt headerTxtMedium inactive" in
  let txtSmallActive = class_ "headerTxt headerTxtSmall active" in
  let txtSmallInactive = class_ "headerTxt headerTxtSmall inactive" in
  header
    [ class_ "headerMargin" ]
    [
      div
        [ class_ "headerTabs" ]
        [
          span
            [
              (if equal_selectedTab selected TrainingLog then txtLargeActive
               else txtLargeInactive);
            ]
            [ a [ path_attr href Paths.index ] [ txt "Training Log" ] ];
          span
            [
              (if equal_selectedTab selected TrainingLog then txtMediumActive
               else txtMediumInactive);
            ]
            [ a [ path_attr href Paths.index ] [ txt "Training Log" ] ];
          span
            [
              (if equal_selectedTab selected TrainingLog then txtSmallActive
               else txtSmallInactive);
            ]
            [ a [ path_attr href Paths.index ] [ txt "Log" ] ];
          span
            [
              (if equal_selectedTab selected Calendar then txtLargeActive
               else txtLargeInactive);
            ]
            [ a [ path_attr href Paths.calendar ] [ txt "Calendar" ] ];
          span
            [
              (if equal_selectedTab selected Calendar then txtMediumActive
               else txtMediumInactive);
            ]
            [ a [ path_attr href Paths.calendar ] [ txt "Calendar" ] ];
          span
            [
              (if equal_selectedTab selected Calendar then txtSmallActive
               else txtSmallInactive);
            ]
            [ a [ path_attr href Paths.calendar ] [ txt "Cal" ] ];
          span
            [
              (if equal_selectedTab selected Dashboard then txtLargeActive
               else txtLargeInactive);
            ]
            [ a [ href "" ] [ txt "Dashboards" ] ];
          span
            [
              (if equal_selectedTab selected Dashboard then txtMediumActive
               else txtMediumInactive);
            ]
            [ a [ href "" ] [ txt "Dashboards" ] ];
          span
            [
              (if equal_selectedTab selected Dashboard then txtSmallActive
               else txtSmallInactive);
            ]
            [ a [ href "" ] [ txt "Dash" ] ];
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
