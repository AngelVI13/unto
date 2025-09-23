open Core

(* TODO: this is very very hacky. I can attach a script section instead and
   load a js file but how to pass attributes to the js?? *)
let activity_map_script (locations : float list list) : string =
  let latlng_str =
    List.fold ~init:""
      ~f:(fun acc latlng ->
        acc
        ^ sprintf "[%f, %f]," (List.nth_exn latlng 0) (List.nth_exn latlng 1))
      locations
  in
  let pretxt =
    {|
var map = L.map('map');
L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 19,
    attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
}).addTo(map);

// create a red polyline from an array of LatLng points
var latlngs = [
  |}
  in
  let posttext =
    {|
];

var polyline = L.polyline(latlngs, {color: 'red'}).addTo(map);

// zoom the map to the polyline
map.fitBounds(polyline.getBounds());

var routeCenter = map.getCenter();
var routeZoom = map.getZoom();

L.control.resetView({
    position: "topleft",
    title: "Reset view",
    latlng: routeCenter,
    zoom: routeZoom,
}).addTo(map);
  |}
  in
  pretxt ^ latlng_str ^ posttext
