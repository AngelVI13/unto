open Core

(* Exponential Moving Average *)
(* NOTE: The smoothing works as follows: each data point is represented as %X
   of it's value and 100-%X of the previous value. For example, if we have alfa
   of 0.2 then the value of the Z-th element is 20% of Data[Z] and 80% of Data[Z-1] *)
(* TODO: this looks good but maybe I can achieve the same with List.map or fold so it's more obvious *)
let exponential_moving_average alpha data =
  let open Float in
  match data with
  | [] -> []
  | hd :: tl ->
      let rec aux prev acc = function
        | [] -> List.rev acc
        | x :: xs ->
            let ema = (alpha * x) + ((1.0 - alpha) * prev) in
            aux ema (ema :: acc) xs
      in
      aux hd [ hd ] tl

(* Simple Moving Average *)
let moving_average k data =
  let n = List.length data in
  let arr = Array.of_list data in
  let smoothed = Array.create ~len:n 0.0 in
  for i = 0 to n - 1 do
    let sum = ref 0.0 in
    let count = ref 0 in
    for j = i - k to i + k do
      if j >= 0 && j < n then (
        sum := !sum +. arr.(j);
        incr count)
    done;
    smoothed.(i) <- !sum /. float_of_int !count
  done;
  Array.to_list smoothed
