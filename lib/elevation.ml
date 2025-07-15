open Core

module ElevResult = struct
  type t = {
    elev_high : int option;
    elev_low : int option;
    elev_gain : int option;
    elev_loss : int option;
  }

  let empty () =
    { elev_high = None; elev_low = None; elev_gain = None; elev_loss = None }

  let fold_elt data i t value =
    let elev_high, elev_low =
      match (t.elev_high, t.elev_low) with
      | None, None -> (Some value, Some value)
      | Some high, Some low ->
          let new_high = if value > high then value else high in
          let new_low = if value < low then value else low in

          (Some new_high, Some new_low)
      | _ -> assert false
    in
    let elev_gain, elev_loss =
      match (t.elev_gain, t.elev_loss) with
      | None, None -> (Some 0, Some 0)
      | Some gain, Some loss -> (
          let prev_elev = List.nth_exn data (i - 1) in
          let diff = value - prev_elev in
          match diff with
          | _ when diff < 0 -> (Some gain, Some (loss + abs diff))
          | _ when diff > 0 -> (Some (gain + diff), Some loss)
          | _ -> (Some gain, Some loss))
      | _ -> assert false
    in
    { elev_high; elev_low; elev_gain; elev_loss }

  let compute ?(max_n = 10) window data =
    let rec advanced_smoothing_aux n prev data =
      if n = 0 then prev
      else
        let smoothed =
          Utils.moving_average (module Utils.FloatOps) window data
        in
        let smoothed_int = List.map ~f:Int.of_float smoothed in
        let compute_fn = fold_elt smoothed_int in
        let results = List.foldi ~init:(empty ()) ~f:compute_fn smoothed_int in
        let gain, loss =
          ( Option.value_exn results.elev_gain,
            Option.value_exn results.elev_loss )
        in
        let prev_gain, prev_loss =
          (Option.value_exn prev.elev_gain, Option.value_exn prev.elev_loss)
        in
        if prev_gain = gain && prev_loss = loss then (
          printf "Smoothing equilibrium reached at depth=%d\n" (max_n - n);
          results)
        else advanced_smoothing_aux (n - 1) results smoothed
    in
    let start_data = empty () in
    let start_data =
      { start_data with elev_gain = Some (-1); elev_loss = Some 1 }
    in
    advanced_smoothing_aux max_n start_data data
end
