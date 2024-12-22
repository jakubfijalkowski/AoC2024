open Utils

module Input = struct
  type t = int list

  let of_string s =
    let lines = String.split_on_char '\n' s in
    List.map int_of_string lines
end

module FourOfInt = struct
  type t = int * int * int * int

  let compare (a1, b1, c1, d1) (a2, b2, c2, d2) =
    match compare a1 a2 with
    | 0 -> (
        match compare b1 b2 with
        | 0 -> ( match compare c1 c2 with 0 -> compare d1 d2 | c -> c)
        | b -> b)
    | a -> a
end

module FourOfIntSet = Set.Make (FourOfInt)

let mix secret value = secret lxor value
let prune secret = secret mod 16777216
let step1 secret = secret * 64 |> mix secret |> prune
let step2 secret = secret / 32 |> mix secret |> prune
let step3 secret = secret * 2048 |> mix secret |> prune
let step secret = step1 secret |> step2 |> step3

let step_for n secret =
  Seq.init n (fun i -> i) |> Seq.fold_left (fun acc _ -> step acc) secret

let price_of n = n mod 10

let generate_prices n secret =
  Seq.init (n - 1) (fun i -> i)
  |> Seq.scan
       (fun (secret, prev_price, _) _ ->
         let next_secret = step secret in
         let price = price_of next_secret in
         (next_secret, price, price - prev_price))
       (secret, price_of secret, 0)
  |> Seq.map (fun (_, a, b) -> (a, b))
  |> Seq.drop 1 |> List.of_seq

let rec all_fours prices =
  match prices with
  | (_, a) :: (_, b) :: (_, c) :: (_, d) :: _ ->
      (a, b, c, d) :: all_fours (ListExt.drop 1 prices)
  | _ -> []

let rec find_price_after (a, b, c, d) prices =
  match prices with
  | (_, a') :: (_, b') :: (_, c') :: (p, d') :: _
    when a = a' && b = b' && c = c' && d = d' ->
      Some p
  | _ :: ps -> find_price_after (a, b, c, d) ps
  | [] -> None

let task_input () =
  let data = read_data_as_string "data/day22.txt" in
  Input.of_string data

let runPart1 input = List.map (step_for 2000) input |> ListExt.sum

let runPart2 input =
  let prices = List.map (generate_prices 2000) input in
  let find_price q =
    prices |> List.filter_map (find_price_after q) |> ListExt.sum
  in
  let all_fours =
    List.map all_fours prices |> List.concat |> FourOfIntSet.of_list
  in
  FourOfIntSet.elements all_fours |> List.map find_price |> ListExt.max1

let run () =
  let input = task_input () in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 22: %d %d\n" part1 part2
