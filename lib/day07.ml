open Utils
module Calibrations = Map.Make (Int)

module Input = struct
  type t = (int * int list) list

  let of_string s =
    let lines = String.split_on_char '\n' s in
    List.map
      (fun line ->
        let sum, ops =
          match String.split_on_char ':' line with
          | [ sum; ops ] -> (int_of_string sum, ops)
          | _ -> failwith "Invalid input"
        in
        let nums =
          String.split_on_char ' ' ops
          |> List.filter (fun x -> x <> "")
          |> List.map (fun i -> String.trim i |> int_of_string)
        in
        (sum, nums))
      lines
end

let concat_num a b =
  let digits = int_of_float (log10 (float_of_int b)) + 1 in
  (a * int_of_float (10. ** float_of_int digits)) + b

let can_evaluate_simple (sum, nums) =
  let rec try_evaluate partial rest =
    match rest with
    | [] -> partial = sum
    | h :: t -> try_evaluate (partial + h) t || try_evaluate (partial * h) t
  in
  try_evaluate (List.hd nums) (List.tl nums)

let can_evaluate_complex (sum, nums) =
  let rec try_evaluate partial rest =
    match rest with
    | [] -> partial = sum
    | h :: t ->
        try_evaluate (partial + h) t
        || try_evaluate (partial * h) t
        || try_evaluate (concat_num partial h) t
  in
  try_evaluate (List.hd nums) (List.tl nums)

let runPart1 i =
  List.filter can_evaluate_simple i |> List.map fst |> List.fold_left ( + ) 0

let runPart2 i =
  List.filter can_evaluate_complex i |> List.map fst |> List.fold_left ( + ) 0

let run () =
  let data = read_data_as_string "data/day07.txt" in
  let input = Input.of_string data in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 07: %d %d\n" part1 part2
