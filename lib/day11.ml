open Utils

module Input = struct
  type t = string list

  let of_string s =
    let nums = String.split_on_char ' ' s in
    List.map int_of_string nums
end

module Index = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
end

module Cache = Map.Make (Index)

let digits_in n = int_of_float (log10 (float_of_int n)) + 1
let pow a b = int_of_float (float_of_int a ** float_of_int b)

let split x =
  let p = pow 10 (digits_in x / 2) in
  (x / p, x mod p)

let blink_single x =
  match x with
  | 0 -> [ 1 ]
  | x when digits_in x mod 2 = 0 ->
      let a, b = split x in
      [ a; b ]
  | x -> [ x * 2024 ]

let rec keep_blinking cache no x =
  if no = 0 then (1, cache)
  else
    match Cache.find_opt (x, no) cache with
    | Some v -> (v, cache)
    | None ->
        let next = blink_single x in
        let res, new_cache =
          List.fold_left
            (fun (acc, cache) x ->
              let res, new_cache = keep_blinking cache (no - 1) x in
              (acc + res, new_cache))
            (0, cache) next
        in
        (res, Cache.add (x, no) res new_cache)

let rec blink_raw lst =
  match lst with [] -> [] | x :: xs -> blink_single x @ blink_raw xs

let blink_for no lst =
  List.fold_left
    (fun (acc, cache) x ->
      let res, new_cache = keep_blinking cache no x in
      (acc + res, new_cache))
    (0, Cache.empty) lst
  |> fst

let runPart1 i =
  Seq.init 25 (fun x -> x)
  |> Seq.fold_left (fun acc _ -> blink_raw acc) i
  |> List.length

let runPart2 i = blink_for 75 i

let run () =
  let data = read_data_as_string "data/day11.txt" in
  let input = Input.of_string data in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 11: %d %d\n" part1 part2
