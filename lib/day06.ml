module Position =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        | 0 -> Stdlib.compare y0 y1
        | c -> c
  end

type direction = North | South | East | West
type state = {pos: Position.t; dir: direction}

module State =
  struct
    type t = state
    let compare a b =
      let (x0, y0) = a.pos in
      let (x1, y1) = b.pos in
      match Stdlib.compare x0 x1 with
        | 0 ->
          begin
            match Stdlib.compare y0 y1 with
            | 0 -> Stdlib.compare a.dir b.dir
            | c -> c
          end
        | c -> c
  end

module PositionSet = Set.Make(Position)
module StateSet = Set.Make(State)

module Map = struct
  type cell = Empty | Obstruction | StartingPos
  type t = {map: cell array array; width: int; height: int}

  let cell_of_char = function
    | '.' -> Empty
    | '#' -> Obstruction
    | '^' -> StartingPos
    | _ -> failwith "Invalid character"

  let of_string s =
      let lines = String.split_on_char '\n' s in
      let width = String.length (List.hd lines) in
      let height = List.length lines in
      let map = Array.of_list (List.map (fun l -> Array.of_seq (Seq.map cell_of_char (String.to_seq l))) lines) in
      {map; width; height}
  
  let is_empty m (x, y) = m.map.(y).(x) = Empty || m.map.(y).(x) = StartingPos
  let can_place_obstruction m (x, y) = m.map.(y).(x) = Empty

  let get_starting_pos m =
    let rec find_starting_pos x y =
      if y >= m.height then failwith "Starting position not found"
      else if x >= m.width then find_starting_pos 0 (y + 1)
      else if m.map.(y).(x) = StartingPos then (x, y)
      else find_starting_pos (x + 1) y
    in
    find_starting_pos 0 0
  
  let is_in_map m (x, y) = x >= 0 && y >= 0 && x < m.width && y < m.height
end

let turn_right = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let move (x, y) = function
  | North -> (x, y - 1)
  | South -> (x, y + 1)
  | East -> (x + 1, y)
  | West -> (x - 1, y)

let next_state m s =
  let new_dir = turn_right s.dir in
  let new_pos = move s.pos s.dir in
  if Map.is_in_map m new_pos then
    if Map.is_empty m new_pos then
      Some({pos = new_pos; dir = s.dir})
    else
      Some({pos = s.pos; dir = new_dir})
  else
    None

let read_data_as_string filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let buf = Bytes.create len in
  really_input chan buf 0 len;
  close_in chan;
  String.trim (Bytes.to_string buf)

let runPart1 m =
  (* let state =  in *)
  let rec simulate visited s =
    match next_state m s with
      | Some(s') -> simulate (PositionSet.add s'.pos visited) s'
      | None -> visited
  in PositionSet.cardinal (simulate (PositionSet.singleton (Map.get_starting_pos m)) {pos = Map.get_starting_pos m; dir = North})
  
let runPart2 map =
  let initial_state = {pos = Map.get_starting_pos map; dir = North} in
  let initial_visited = StateSet.singleton initial_state in
  let rec simulate m visited s i =
    match next_state m s with
      | Some(s') ->
        if StateSet.mem s' visited then
          1
        else
          simulate m (StateSet.add s' visited) s' (i + 1)
      | None -> 0
  in
  let try_single (x, y) =
    if Map.can_place_obstruction map (x, y) then
      let m' = {map with map = Array.map Array.copy map.map} in
      m'.map.(y).(x) <- Map.Obstruction;
      simulate m' initial_visited initial_state 0
    else
      0
  in
  let res = Seq.init map.height (fun y -> Seq.init map.width (fun x -> (x, y))) |> Seq.concat |> Seq.map try_single |> Seq.fold_left (+) 0 in
  res

let run () =
  let data = read_data_as_string "data/day06.txt" in
  let input = Map.of_string data in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 05: %d %d\n" part1 part2