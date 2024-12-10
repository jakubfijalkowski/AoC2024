open Utils

module Position = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
end

module ReachedTop = Set.Make (Position)

module Map = struct
  type t = { map : int array array; width : int; height : int }

  let zeroc = int_of_char '0'
  let cell_of_char c = int_of_char c - zeroc

  let of_string s =
    let lines = String.split_on_char '\n' s in
    let width = String.length (List.hd lines) in
    let height = List.length lines in
    let map =
      Array.of_list
        (List.map
           (fun l -> Array.of_seq (Seq.map cell_of_char (String.to_seq l)))
           lines)
    in
    { map; width; height }

  let is_in_map m (x, y) = x >= 0 && y >= 0 && x < m.width && y < m.height

  let all_pos m =
    Seq.init m.height (fun y -> Seq.init m.width (fun x -> (x, y)))
    |> Seq.concat

  let next_moves (x, y) =
    [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ] |> List.to_seq

  let can_move m from (x, y) = is_in_map m (x, y) && m.map.(y).(x) = from + 1
  let is_start m (x, y) = m.map.(y).(x) = 0
end

let runPart1 m =
  let rec reach_peaks' (tops, score) current p =
    if current == 9 then
      if ReachedTop.mem p tops then (tops, score)
      else (ReachedTop.add p tops, score + 1)
    else
      Map.next_moves p
      |> Seq.filter (Map.can_move m current)
      |> Seq.fold_left
           (fun acc p -> reach_peaks' acc (current + 1) p)
           (tops, score)
  in
  let reach_peaks p = snd (reach_peaks' (ReachedTop.empty, 0) 0 p) in
  Map.all_pos m
  |> Seq.filter (Map.is_start m)
  |> Seq.map reach_peaks |> SeqExt.sum

let runPart2 m =
  let rec reach_peaks' current p =
    if current == 9 then 1
    else
      Map.next_moves p
      |> Seq.filter (Map.can_move m current)
      |> Seq.map (reach_peaks' (current + 1))
      |> SeqExt.sum
  in
  let reach_peaks p = reach_peaks' 0 p in
  Map.all_pos m
  |> Seq.filter (Map.is_start m)
  |> Seq.map reach_peaks |> SeqExt.sum

let run () =
  let data = read_data_as_string "data/day10.txt" in
  let input = Map.of_string data in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 10: %d %d\n" part1 part2
