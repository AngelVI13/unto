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

(* <div class="day dayWithColor"> *)
(*   <div class="activities"> *)
(*     <div class="activity card"> *)
(*       <div class="activityCardStats"> *)
(*         <div class="activityCardStat"> *)
(*           <span class="activityCardStatName statNameText">Time: </span> *)
(*           <span class="activityCardStatName statNameIcon"> *)
(*             <img *)
(*               class="stat-icon-img" *)
(*               title="Duration" *)
(*               src="./duration.png" *)
(*             /> *)
(*           </span> *)
(*           <span class="activityCardStatValue">35:12</span> *)
(*         </div> *)
(*         <div class="activityCardStat"> *)
(*           <span class="activityCardStatName statNameText" *)
(*             >Pace (/km): *)
(*           </span> *)
(*           <span class="activityCardStatName statNameIcon"> *)
(*             <img *)
(*               class="stat-icon-img" *)
(*               title="Pace (min/km)" *)
(*               src="./pace.png" *)
(*             /> *)
(*           </span> *)
(*           <span class="activityCardStatValue">6:45</span> *)
(*         </div> *)
(*         <div class="activityCardStat"> *)
(*           <span class="activityCardStatName statNameText" *)
(*             >Elev. (m): *)
(*           </span> *)
(*           <span class="activityCardStatName statNameIcon"> *)
(*             <img *)
(*               class="stat-icon-img" *)
(*               title="Elevation gain & loss (m)" *)
(*               src="./elevation.png" *)
(*             /> *)
(*           </span> *)
(*           <span class="activityCardStatValue">+120/-55</span> *)
(*         </div> *)
(*       </div> *)
(*     </div> *)
(*   </div> *)
(* </div> *)

let activity_header (activity : Models.Activity.t) =
  let open Dream_html in
  let open HTML in
  let icon_color, img_src =
    match activity.sport_type with
    | Run -> ("gold", "/static/assets/running.png")
    | Ride -> ("coral", "/static/assets/cycling.png")
    | Crossfit -> ("greenyellow", "/static/assets/crosstrain.png")
    | _ -> ("", "")
  in
  let icon_background = sprintf "background: %s;" icon_color in
  div
    [ class_ "activityHeader" ]
    [
      span
        [ class_ "icon-container"; style_ "%s" icon_background ]
        [ img [ class_ "icon-img"; src "%s" img_src ] ];
      span
        [ class_ "activityType" ]
        [ txt "%s" (Models.Strava_models.show_sportType activity.sport_type) ];
    ]

let activity_div (activity : Models.Activity.t) =
  let open Dream_html in
  let open HTML in
  div [ class_ "activity card" ] [ activity_header activity ]

let weekTableActivities (activities : Models.Activity.t list list) =
  let open Dream_html in
  let open HTML in
  let days =
    List.mapi
      ~f:(fun i day_activities ->
        let activity_divs = List.map ~f:activity_div day_activities in
        div
          [ class_ (if i mod 2 = 0 then "day dayWithColor" else "day") ]
          [ div [ class_ "activities" ] activity_divs ])
      activities
  in
  div [ class_ "days scrollable calendarHeight" ] days

let training_log (athlete : Models.Strava_models.StravaAthlete.t option)
    (activities : Models.Activity.t list list) =
  let open Dream_html in
  let open HTML in
  let athlete =
    match athlete with None -> "Unknown" | Some athl -> athl.firstname
  in
  html
    [ lang "en" ]
    [
      head [] (head_elems ());
      body []
        [ header_ athlete; weekTableHeader (); weekTableActivities activities ];
    ]
