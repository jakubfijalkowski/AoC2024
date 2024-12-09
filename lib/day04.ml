let read_data_as_string filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let buf = Bytes.create len in
  really_input chan buf 0 len;
  close_in chan;
  String.trim (Bytes.to_string buf)

type board = { lines : string array; width : int; height : int }

let dirs =
  [ (0, 1); (0, -1); (1, 0); (-1, 0); (1, 1); (-1, -1); (1, -1); (-1, 1) ]

let rec pairs = function
  | [] -> []
  | x :: xs -> List.map (fun y -> (x, y)) xs @ pairs xs

let parse_board s =
  let lines = String.split_on_char '\n' s in
  let width = String.length (List.hd lines) in
  let height = List.length lines in
  let lines = Array.of_list lines in
  { lines; width; height }

let char_at board x y =
  if x >= 0 && y >= 0 && x < board.width && y < board.height then
    Some board.lines.(y).[x]
  else None

let any = List.fold_left (fun acc x -> acc || x) false
let all = List.fold_left (fun acc x -> acc && x) true
let int_of_bool b = if b then 1 else 0

let test_flat_at board x y =
  let rest = [ 'X'; 'M'; 'A'; 'S' ] in
  List.map
    (fun (dx, dy) ->
      List.mapi
        (fun i c -> char_at board (x + (i * dx)) (y + (i * dy)) = Some c)
        rest
      |> all |> int_of_bool)
    dirs
  |> List.fold_left ( + ) 0

let is_same_dir ((x1, y1), (x2, y2)) = -x1 = x2 && -y1 = y2
let all_diag ((x1, y1), (x2, y2)) = x1 != 0 && y1 != 0 && x2 != 0 && y2 != 0

let test_pattern_at board x y =
  let rest = [ 'M'; 'A'; 'S' ] in
  let all_dirs =
    pairs dirs |> List.filter (fun p -> (not (is_same_dir p)) && all_diag p)
  in
  let test_c dx dy i c =
    char_at board (x + ((i - 1) * dx)) (y + ((i - 1) * dy)) = Some c
  in
  let test_pat ((dx1, dy1), (dx2, dy2)) =
    [
      List.mapi (fun i c -> test_c dx1 dy1 i c) rest |> all;
      List.mapi (fun i c -> test_c dx2 dy2 i c) rest |> all;
    ]
    |> all
  in
  List.map test_pat all_dirs |> List.map int_of_bool |> List.fold_left ( + ) 0

let runPart1 b =
  let for_y = List.init b.height (fun i -> i) in
  let for_x = List.init b.width (fun i -> i) in
  List.map (fun y -> List.map (fun x -> test_flat_at b x y) for_x) for_y
  |> List.flatten |> List.fold_left ( + ) 0

let runPart2 b =
  let for_y = List.init b.height (fun i -> i) in
  let for_x = List.init b.width (fun i -> i) in
  List.map (fun y -> List.map (fun x -> test_pattern_at b x y) for_x) for_y
  |> List.flatten |> List.fold_left ( + ) 0

let run () =
  let data = read_data_as_string "data/day04.txt" in
  let board = parse_board data in
  let part1 = runPart1 board in
  let part2 = runPart2 board in
  Printf.printf "Day 04: %d %d\n" part1 part2
