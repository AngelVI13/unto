open! Core
open Dream_html
open HTML

let activity_splits_table (activity : Models.Activity.t) =
  List.iter ~f:(fun split -> Dream.log "%d" split.split_index) activity.splits;
  let _ = activity in
  let nodes = [] in
  div
    [ class_ "splitsTable" ]
    [
      table []
        [
          thead []
            [
              tr []
                [
                  th [] [ txt "" ];
                  th [] [ txt "Avg" ];
                  th [] [ txt "Max" ];
                  th [] [ txt "Min" ];
                ];
            ];
          tbody [] nodes;
        ];
    ]
