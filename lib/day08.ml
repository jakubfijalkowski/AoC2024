open Utils

module Position =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        | 0 -> Stdlib.compare y0 y1
        | c -> c
    let distance (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)
    let offset (x0, y0) (x1, y1) = (x0 - x1, y0 - y1)
    let mul (x0, y0) d = (x0 * d, y0 * d)
    let move (x, y) (dx, dy) = (x + dx, y + dy)
    let gen_antinode m a b = move a (mul (offset a b) m)
  end

module Antennas = Map.Make(Char)
module AntinodesSet = Set.Make(Position)

type antennas = Position.t list Antennas.t

module Input =
  struct
    type t = {antennas: antennas; width: int; height: int}

    let of_string s =
      let lines = String.split_on_char '\n' s in
      let height = List.length lines in
      let width = String.length (List.hd lines) in
      let antennas = Seq.init height (fun y -> Seq.init width (fun x -> (x, y))) |>
        Seq.zip (List.to_seq lines) |>
        Seq.map (fun (l, pos) -> Seq.zip pos (String.to_seq l)) |>
        Seq.concat |>
        Seq.fold_left (fun m (p, c) ->
          match c with
          | '.' -> m
          | _ -> Antennas.update c (function
            | None -> Some [p]
            | Some l -> Some (p :: l)) m) Antennas.empty in
      {antennas; width; height}
    
    let is_inside {width; height; _} (x, y) = x >= 0 && x < width && y >= 0 && y < height

    let max_distance i = i.width + i.height
    let max_multiply i off = max_distance i / (Position.distance (0, 0) off) + 1
  end

let try_add_single_antinode mul input set a b =
  let a' = Position.gen_antinode mul a b in
  let b' = Position.gen_antinode mul b a in
  let a' = if Input.is_inside input a' then AntinodesSet.add a' set else set in
  if Input.is_inside input b' then AntinodesSet.add b' a' else a'

let try_add_all_antinodes input set a b =
  let max_mul = Input.max_multiply input (Position.offset a b) in
  let muls = Seq.init max_mul (fun i -> i) in
  let dir1 = Seq.fold_left (fun acc m -> try_add_single_antinode m input acc a b) set muls in
  let dir2 = Seq.fold_left (fun acc m -> try_add_single_antinode m input acc b a) dir1 muls in
  dir2

let generate_antinodes generator (input: Input.t) =
  let rec add_all_single acc x = function
    | [] -> acc
    | y :: ys -> add_all_single (generator input acc x y) x ys in
  let rec add_all acc = function
    | [] -> acc
    | x :: xs -> add_all (add_all_single acc x xs) xs in
  Antennas.fold (fun _ lst acc -> add_all acc lst) input.antennas AntinodesSet.empty

let runPart1 i =
  generate_antinodes (try_add_single_antinode 1) i |> AntinodesSet.cardinal
  
let runPart2 i =
  generate_antinodes try_add_all_antinodes i |> AntinodesSet.cardinal

let run () =
  let data = read_data_as_string "data/day08.txt" in
  let input = Input.of_string data in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 07: %d %d\n" part1 part2