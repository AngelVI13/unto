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

module type NUMERIC = sig
  type t

  val zero : t
  val add : t -> t -> t
  val div : t -> int -> t
end

module IntOps : NUMERIC with type t = Int.t = struct
  include Int

  let add = Int.( + )
  let div = Int.( / )
end

module FloatOps : NUMERIC with type t = Float.t = struct
  include Float

  let add = Float.add
  let div x n = x /. Float.of_int n
end

(* Simple Moving Average *)
let moving_average (type a) (module N : NUMERIC with type t = a) k data =
  let n = List.length data in
  let arr = Array.of_list data in
  let smoothed = Array.create ~len:n N.zero in
  for i = 0 to n - 1 do
    let sum = ref N.zero in
    let count = ref 0 in
    for j = i - k to i + k do
      if j >= 0 && j < n then (
        sum := N.add !sum arr.(j);
        incr count)
    done;
    smoothed.(i) <- N.div !sum !count
  done;
  Array.to_list smoothed
