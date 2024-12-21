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
  module DirectedPointMap = Map.Make (DirectedPoint)
  module Cost = Int
  module Vertex = DirectedPoint

  type t = {
    edges : (int * Vertex.t) list DirectedPointMap.t;
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

  let estimate_distance (p1, _) (p2, _) = Point.distance p1 p2
  let neighbors { edges; _ } (p, d) = DirectedPointMap.find (p, d) edges
end

module GraphSearch = AStar.Make (Graph)
module DirectedPointSet = Set.Make (DirectedPoint)
module PointSet = Set.Make (Point)

let select_finish_dir gScore finish =
  let dirs =
    Direction.all_dirs
    |> List.map (fun d ->
           (d, GraphSearch.VertexMap.find_opt (finish, d) gScore))
  in
  List.filter_map (fun (d, x) -> Option.map (fun s -> (d, s)) x) dirs
  |> ListExt.min_by snd

let runPart1 input =
  let g = Graph.of_input input in
  let gScore = GraphSearch.find_all_paths g (g.start, Direction.Right) |> fst in
  Direction.all_dirs
  |> List.filter_map (fun d ->
         GraphSearch.VertexMap.find_opt (g.finish, d) gScore)
  |> ListExt.min1

let runPart2 input =
  let g = Graph.of_input input in
  let gScore, came_from =
    GraphSearch.find_all_paths g (g.start, Direction.Right)
  in
  let finish_dir = select_finish_dir gScore g.finish in
  let finish_dir = Option.get finish_dir |> snd |> fst in
  let rec visit_all visited cameFrom current start =
    if fst current = start || DirectedPointSet.mem current visited then visited
    else
      GraphSearch.VertexMap.find current cameFrom
      |> List.fold_left
           (fun acc p ->
             DirectedPointSet.union acc (visit_all acc cameFrom p start))
           (DirectedPointSet.add current visited)
  in
  let card =
    visit_all DirectedPointSet.empty came_from (g.finish, finish_dir) g.start
    |> DirectedPointSet.elements
    |> List.map (fun (p, _) -> p)
    |> PointSet.of_list |> PointSet.cardinal
  in
  card + 1

let run () =
  let data = read_data_as_string "data/day16.txt" in
  let part1 = runPart1 (Input.of_string data) in
  let part2 = runPart2 (Input.of_string data) in
  Printf.printf "Day 16: %d %d\n" part1 part2
