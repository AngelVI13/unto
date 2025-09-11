open Core

let activity_icon_and_color (activity : Models.Activity.t) =
  let icon_color, img_src =
    match activity.sport_type with
    | Run -> ("gold", "/static/assets/running.png")
    | Ride -> ("coral", "/static/assets/cycling.png")
    | Crossfit -> ("greenyellow", "/static/assets/crosstrain.png")
    | _ -> ("gray", "/static/assets/unknown.png")
  in
  let icon_background = sprintf "background: %s;" icon_color in
  (icon_background, img_src)
