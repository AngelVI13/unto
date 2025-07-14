open Core

module ElevResult = struct
  type t = {
    elev_high : float option;
    elev_low : float option;
    elev_gain : float option;
    elev_loss : float option;
  }

  let empty () =
    { elev_high = None; elev_low = None; elev_gain = None; elev_loss = None }

  let gain_loss_threshold = 0.08

  let compute data i t value =
    (* TODO: this seems to work file, verify it produces ok result for multiple activities *)
    let elev_high, elev_low =
      match (t.elev_high, t.elev_low) with
      | None, None -> (Some value, Some value)
      | Some high, Some low ->
          let new_high = if Float.(value > high) then value else high in
          let new_low = if Float.(value < low) then value else low in

          (Some new_high, Some new_low)
      | _ -> assert false
    in
    let elev_gain, elev_loss =
      match (t.elev_gain, t.elev_loss) with
      | None, None -> (Some 0.0, Some 0.0)
      | Some gain, Some loss -> (
          let prev_elev = List.nth_exn data (i - 1) in
          let open Float in
          let diff = value - prev_elev in
          match diff with
          | _ when diff < -gain_loss_threshold ->
              (Some gain, Some (loss + abs diff))
          | _ when diff > gain_loss_threshold -> (Some (gain + diff), Some loss)
          | _ -> (Some gain, Some loss))
      | _ -> assert false
    in
    { elev_high; elev_low; elev_gain; elev_loss }
end

module ElevResultInt = struct
  type t = {
    elev_high : int option;
    elev_low : int option;
    elev_gain : int option;
    elev_loss : int option;
  }

  let empty () =
    { elev_high = None; elev_low = None; elev_gain = None; elev_loss = None }

  let compute data i t value =
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
end
