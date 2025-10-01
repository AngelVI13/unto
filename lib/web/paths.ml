let%path index = "/"
let%path update = "/update"
let%path activity = "/activity/:id"
let%path activity_map = "/activity/:id/map"
let%path activity_map_url = "/activity/%d/map"

(* TODO: this looks like shit cause i need 2 paths one for the dream router and another for the Url builder *)
let%path activity_graph = "/activity/:id/graph"
let%path activity_graph_url = "/activity/%d/graph"
let%path activity_select = "/activity/:id/select/:select"
let%path activity_select_url = "/activity/%d/select/%s"
