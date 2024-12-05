open Either

module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        | 0 -> Stdlib.compare y0 y1
        | c -> c
  end

module Ordering = Set.Make(IntPairs)
module VertexSet = Set.Make(Int)

module ListExt = struct
  let all_predicate f = List.fold_left (fun acc x -> acc && f x) true

  let split_on_empty l =
    let idx = List.find_index (fun x -> x = "") l |> Option.get in
    let filtered = List.filter (fun x -> x <> "") l in
    let mapped = List.mapi (fun i x -> if i < idx then Left(x) else Right(x)) filtered in
    List.partition_map (fun x -> x) mapped
end

module Input = struct
  type t = { rules: Ordering.t; pages: int list list }

  let page_of_string s = String.split_on_char ',' s |> List.map int_of_string

  let rule_of_string s =
    match String.split_on_char '|' s |> List.map int_of_string with
    | a :: b :: _ -> (a, b)
    | _ -> failwith "Invalid rule format"

  let of_string s =
    let lines = String.split_on_char '\n' s in
    let (rules, pages) = ListExt.split_on_empty lines in
    let rules = List.map rule_of_string rules in
    { rules = rules|> Ordering.of_list;
      pages = List.map page_of_string pages }
end

let read_data_as_string filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let buf = Bytes.create len in
  really_input chan buf 0 len;
  close_in chan;
  String.trim (Bytes.to_string buf)

let is_valid (input:Input.t) page =
  let rec is_valid' page =
    match page with
    | [] -> true
    | [_] -> true
    | x :: xs -> ListExt.all_predicate (fun y -> Ordering.mem (x, y) input.rules) xs && is_valid' xs
  in
  is_valid' page

let topological_sort (input : Input.t) page =
  let subgraph = input.rules |> Ordering.filter (fun (a, m) -> List.mem a page && List.mem m page) in
  let not_visited = ref (VertexSet.of_list page) in
  let graph = ref [] in
  let rec visit n =
    if VertexSet.mem n !not_visited then
      begin
        not_visited := VertexSet.remove n !not_visited;
        Ordering.iter (fun (a, m) -> if a = n then visit m) subgraph;
        graph := n :: !graph
      end
    else
      ()
    in
    while not (VertexSet.is_empty !not_visited) do
      let n = VertexSet.choose !not_visited in
      visit n
    done;
    !graph

let select_middle page =
  let len = List.length page in
  let middle = len / 2 in
  List.nth page middle

let runPart1 input =
  List.filter (is_valid input) input.pages |> List.map select_middle |> List.fold_left (+) 0
  
let runPart2 (input : Input.t) =
  List.filter (fun x -> not (is_valid input x)) input.pages |> List.map (topological_sort input) |> List.map select_middle |> List.fold_left (+) 0

let run () =
  let data = read_data_as_string "data/day05.txt" in
  let input = Input.of_string data in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 05: %d %d\n" part1 part2