let read_data_as_string filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let buf = Bytes.create len in
  really_input chan buf 0 len;
  close_in chan;
  String.trim (Bytes.to_string buf)

let to_tuples line =
  let parts = String.split_on_char ' ' line in
  let parts = List.filter (fun x -> x <> "") parts in
  let first, second = (List.nth parts 0, List.nth parts 1) in
  (int_of_string first, int_of_string second)

let parse_to_lists data =
  let lines = String.split_on_char '\n' data in
  List.map to_tuples lines

let split_tuple_list lst =
  let rec split_tuple_list' lst acc1 acc2 =
    match lst with
    | [] -> (List.rev acc1, List.rev acc2)
    | (a, b) :: tl -> split_tuple_list' tl (a :: acc1) (b :: acc2)
  in
  split_tuple_list' lst [] []

let sortNums = List.sort (fun a b -> b - a)

let runPart1 first second =
  List.fold_left2 (fun acc a b -> acc + Int.abs (a - b)) 0 first second

let runPart2 first second =
  List.fold_left
    (fun acc a -> acc + (a * List.length (List.filter (fun b -> a = b) second)))
    0 first

let run () =
  let data = read_data_as_string "data/day01.txt" in
  let tuples = parse_to_lists data in
  let first, second = split_tuple_list tuples in
  let first = sortNums first in
  let second = sortNums second in
  let part1 = runPart1 first second in
  let part2 = runPart2 first second in
  Printf.printf "Day 01: %d %d\n" part1 part2
