open Utils

let input_of_string s = String.split_on_char '\n' s

type keyboard = Numeric | Directional

let kb_numeric = function
  | '7' -> (0, 0)
  | '8' -> (1, 0)
  | '9' -> (2, 0)
  | '4' -> (0, 1)
  | '5' -> (1, 1)
  | '6' -> (2, 1)
  | '1' -> (0, 2)
  | '2' -> (1, 2)
  | '3' -> (2, 2)
  | '0' -> (1, 3)
  | 'A' -> (2, 3)
  | _ -> failwith "Invalid key"

let kb_dir = function
  | '^' -> (1, 0)
  | 'A' -> (2, 0)
  | '<' -> (0, 1)
  | 'v' -> (1, 1)
  | '>' -> (2, 1)
  | c -> failwith (Printf.sprintf "Invalid key: %c" c)

let move (x, y) d =
  match d with
  | '<' -> (x - 1, y)
  | '>' -> (x + 1, y)
  | '^' -> (x, y - 1)
  | 'v' -> (x, y + 1)
  | 'A' -> (x, y)
  | _ -> failwith (Printf.sprintf "Invalid direction: %c" d)

let is_allowed kb p =
  match kb with Numeric -> p <> (0, 3) | Directional -> p <> (0, 0)

let is_path_allowed kb start path =
  let rec aux p rest =
    if is_allowed kb p then
      match rest with [] -> true | x :: xs -> aux (move p x) xs
    else false
  in
  aux start path

let kb_pos = function Numeric -> kb_numeric | Directional -> kb_dir

let rec all_combinations seqs =
  match seqs with
  | [] -> [ [] ]
  | first :: rest -> (
      match rest with
      | [] -> List.map (fun x -> [ x ]) first
      | _ ->
          List.map
            (fun x ->
              List.map
                (fun rest_seq -> List.cons x rest_seq)
                (all_combinations rest))
            first
          |> List.flatten)

let rec combine_arrays xs ys =
  match (xs, ys) with
  | [], [] -> [ [] ]
  | x :: xs', [] -> List.map (List.cons x) (combine_arrays xs' ys)
  | [], y :: ys' -> List.map (List.cons y) (combine_arrays xs ys')
  | x :: xs', y :: ys' ->
      List.append
        (List.map (List.cons x) (combine_arrays xs' ys))
        (List.map (List.cons y) (combine_arrays xs ys'))

let get_possible_moves kb c1 c2 =
  let x1, y1 = kb_pos kb c1 in
  let x2, y2 = kb_pos kb c2 in
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let v_moves = List.init (abs dy) (fun _ -> if dy > 0 then 'v' else '^') in
  let h_moves = List.init (abs dx) (fun _ -> if dx > 0 then '>' else '<') in
  combine_arrays h_moves v_moves
  |> List.map (fun s -> s @ [ 'A' ])
  |> List.filter (is_path_allowed kb (x1, y1))

let get_possible_paths' kb input =
  let pairs = ListExt.pairwise input in
  List.map (fun (c1, c2) -> get_possible_moves kb c1 c2) pairs

let get_possible_paths kb input =
  get_possible_paths' kb input |> all_combinations |> List.map List.concat

let rec simulate kbs input =
  match kbs with
  | [] -> List.length input
  | kb :: rest ->
      let acts = get_possible_paths kb (List.cons 'A' input) in
      acts |> List.map (simulate rest) |> ListExt.min1

module CacheKey = struct
  type t = int * char list

  let compare (l1, c1) (l2, c2) =
    match compare l1 l2 with 0 -> compare c1 c2 | x -> x
end

module Cache = Map.Make (CacheKey)

let rec get_moves_required_to_enter kb cache level input =
  if level = 0 then List.length input
  else
    let key = (level, input) in
    match Cache.find_opt key !cache with
    | Some v -> v
    | None ->
        let calc_min inp =
          List.map
            (get_moves_required_to_enter Directional cache (level - 1))
            inp
          |> ListExt.min1
        in
        let moves = get_possible_paths' kb ('A' :: input) in
        let res = List.map calc_min moves |> ListExt.sum in
        cache := Cache.add key res !cache;
        res

let task_input () =
  let data = read_data_as_string "data/day21.txt" in
  input_of_string data

let runPart1 input =
  let keyboards = [ Numeric; Directional; Directional ] in
  let res =
    input
    |> List.map (fun s -> String.to_seq s |> List.of_seq)
    |> List.map (fun s -> simulate keyboards s)
  in
  let paired = ListExt.zip input res in
  List.map (fun (i, s) -> (String.sub i 0 3 |> int_of_string) * s) paired
  |> ListExt.sum

let runPart2 input =
  let levels = 26 in
  let cache = ref Cache.empty in
  let calc_one i =
    get_moves_required_to_enter Numeric cache levels
      (String.to_seq i |> List.of_seq)
  in
  input
  |> List.map (fun s -> (String.sub s 0 3 |> int_of_string) * calc_one s)
  |> ListExt.sum

let run () =
  let input = task_input () in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 21: %d %d\n" part1 part2
