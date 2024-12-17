open Utils

type cell = Wall | Start | Finish | Empty

module Input = struct
  type t = { data : cell array array; width : int; height : int }

  let find map cell =
    Array.find_mapi
      (fun y line ->
        match
          Array.find_mapi (fun x c -> if c = cell then Some x else None) line
        with
        | Some x -> Some (x, y)
        | None -> None)
      map.data
    |> Option.get

  let count_empty map =
    Array.fold_left
      (fun acc line ->
        acc
        + Array.fold_left
            (fun acc c -> if c != Wall then acc + 1 else acc)
            0 line)
      0 map

  let cell_of_char = function
    | '#' -> Wall
    | 'S' -> Start
    | 'E' -> Finish
    | '.' -> Empty
    | _ -> failwith "Invalid cell"

  let of_string s =
    let lines = String.split_on_char '\n' s in
    let width = String.length (List.hd lines) in
    let height = List.length lines in
    let data =
      List.map
        (fun l -> String.to_seq l |> Seq.map cell_of_char |> Array.of_seq)
        lines
      |> Array.of_list
    in
    { data; width; height }

  let all_pos m =
    Seq.init m.height (fun y -> Seq.init m.width (fun x -> (x, y)))
    |> Seq.concat
end

module Direction = struct
  type t = Up | Down | Left | Right

  let all_dirs = [ Up; Down; Left; Right ]

  let move (x, y) = function
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

  let rotate_cw = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

  let rotate_ccw = function
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

  let to_char = function Up -> '^' | Down -> 'v' | Left -> '<' | Right -> '>'
end

module DirectedPoint = struct
  type t = Point.t * Direction.t

  let compare (p1, d1) (p2, d2) =
    match Point.compare p1 p2 with 0 -> compare d1 d2 | c -> c
end

module Graph = struct
  open Direction
  module OpenSet = Psq.Make (DirectedPoint) (Int)
  module DirectedPointMap = Map.Make (DirectedPoint)
  module DirectedPointSet = Set.Make (DirectedPoint)
  module PointSet = Set.Make (Point)

  type t = {
    edges : (int * DirectedPoint.t) list DirectedPointMap.t;
    width : int;
    height : int;
    start : int * int;
    finish : int * int;
  }

  let of_input (input : Input.t) =
    let start = Input.find input Start in
    let finish = Input.find input Finish in
    let width = input.width in
    let height = input.height in
    let is_empty (x, y) = input.data.(y).(x) != Wall in
    let add_empty p dir edges =
      DirectedPointMap.update (p, dir)
        (function None -> Some [] | e -> e)
        edges
    in
    let add p dir e edges =
      DirectedPointMap.update (p, dir)
        (fun lst -> Some (e :: Option.value lst ~default:[]))
        edges
    in
    let add_edge p dir edges = add p dir (1, (move p dir, dir)) edges in
    let add_edge edges (p, dir) =
      if is_empty (move p dir) then add_edge p dir edges
      else add_empty p dir edges
    in
    let edges =
      Input.all_pos input |> Seq.filter is_empty
      |> Seq.map (fun p -> Seq.map (fun d -> (p, d)) (List.to_seq all_dirs))
      |> Seq.concat
      |> Seq.fold_left add_edge DirectedPointMap.empty
    in
    let edges =
      DirectedPointMap.bindings edges
      |> List.fold_left
           (fun acc ((p, d), _) ->
             DirectedPointMap.update (p, d)
               (fun e ->
                 Some
                   ([
                      (1000, (p, rotate_cw d));
                      (2000, (p, rotate_cw (rotate_cw d)));
                      (1000, (p, rotate_ccw d));
                    ]
                   @ Option.get e))
               acc)
           edges
    in
    let edges = edges in
    { edges; width; height; start; finish }

  let find_path_astar graph start finish =
    let rec run openSet gScore cameFrom =
      match OpenSet.pop openSet with
      | None ->
          ( [
              (Up, DirectedPointMap.find_opt (finish, Up) gScore);
              (Right, DirectedPointMap.find_opt (finish, Right) gScore);
              (Down, DirectedPointMap.find_opt (finish, Down) gScore);
              (Left, DirectedPointMap.find_opt (finish, Left) gScore);
            ]
            |> List.filter_map (fun (d, x) -> Option.map (fun s -> (d, s)) x)
            |> ListExt.min_by snd,
            cameFrom )
      | Some (((current, dir), _), newSet) ->
          let neighbors = DirectedPointMap.find (current, dir) graph.edges in
          let curr_score = DirectedPointMap.find (current, dir) gScore in
          let dirs =
            neighbors
            |> List.map (fun (c, dp) -> (curr_score + c, dp))
            |> List.filter_map (fun (c, dp) ->
                   match DirectedPointMap.find_opt dp gScore with
                   | None -> Some (c, dp)
                   | Some s when c <= s -> Some (c, dp)
                   | _ -> None)
          in
          let new_gScore =
            List.fold_left
              (fun acc (c, dp) -> DirectedPointMap.add dp c acc)
              gScore dirs
          in
          let new_openSet =
            List.fold_left (fun acc (c, dp) -> OpenSet.add dp c acc) newSet dirs
          in
          let new_cameFrom =
            List.fold_left
              (fun acc (_, dp) ->
                DirectedPointMap.update dp
                  (fun e -> Some ((current, dir) :: Option.value ~default:[] e))
                  acc)
              cameFrom dirs
          in
          run new_openSet new_gScore new_cameFrom
    in
    run
      (OpenSet.sg (start, Right) (Point.distance start start))
      (DirectedPointMap.singleton (start, Right) 0)
      DirectedPointMap.empty
end

let runPart1 input =
  let g = Graph.of_input input in
  Graph.find_path_astar g g.start g.finish |> fst |> Option.get |> fst

let runPart2 input =
  let g = Graph.of_input input in
  let finish_dir, came_from = Graph.find_path_astar g g.start g.finish in
  let finish_dir = Option.get finish_dir |> snd |> fst in
  let rec visit_all visited cameFrom current start =
    if fst current = start || Graph.DirectedPointSet.mem current visited then
      visited
    else
      Graph.DirectedPointMap.find current cameFrom
      |> List.fold_left
           (fun acc p ->
             Graph.DirectedPointSet.union acc (visit_all acc cameFrom p start))
           (Graph.DirectedPointSet.add current visited)
  in
  let card =
    visit_all Graph.DirectedPointSet.empty came_from (g.finish, finish_dir)
      g.start
    |> Graph.DirectedPointSet.elements
    |> List.map (fun (p, _) -> p)
    |> Graph.PointSet.of_list |> Graph.PointSet.cardinal
  in
  card + 1

let run () =
  let data = read_data_as_string "data/day16.txt" in
  let part1 = runPart1 (Input.of_string data) in
  let part2 = runPart2 (Input.of_string data) in
  Printf.printf "Day 16: %d %d\n" part1 part2
