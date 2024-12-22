open Utils

type cell = Start | Finish | Wall | Empty

module Input = struct
  type t = { map : cell array array; width : int; height : int }

  let cell_of_char = function
    | '.' -> Empty
    | '#' -> Wall
    | 'S' -> Start
    | 'E' -> Finish
    | _ -> failwith "Invalid character"

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

  let find map cell =
    Array.find_mapi
      (fun y line ->
        match
          Array.find_mapi (fun x c -> if c = cell then Some x else None) line
        with
        | Some x -> Some (x, y)
        | None -> None)
      map.map
    |> Option.get
end

module PointSet = Set.Make (Point)

module Graph = struct
  module Vertex = Point
  module Cost = Int

  type t = {
    walls : PointSet.t;
    width : int;
    height : int;
    start : Point.t;
    finish : Point.t;
    allowed : Point.t;
    allowed_radius : int;
    allow_wall : bool;
  }

  let of_input (input : Input.t) =
    let walls =
      Array.mapi
        (fun y line ->
          Array.mapi (fun x c -> if c = Wall then Some (x, y) else None) line
          |> Array.to_list)
        input.map
      |> Array.to_list |> List.concat
      |> List.filter_map (fun x -> x)
    in
    let start = Input.find input Start in
    let finish = Input.find input Finish in
    {
      walls = PointSet.of_list walls;
      width = input.width;
      height = input.height;
      start;
      finish;
      allowed = (-100, -100);
      allowed_radius = 0;
      allow_wall = false;
    }

  let all_pos g =
    Seq.init g.height (fun y -> Seq.init g.width (fun x -> (x, y)))
    |> Seq.concat

  let in_radius g p1 radius =
    all_pos g |> Seq.filter (fun p2 -> Point.distance p1 p2 <= radius)

  let with_exception g pt radius =
    { g with allowed = pt; allowed_radius = radius }

  let allow_wall g = { g with allow_wall = true }

  let is_on_map { width; height; _ } (x, y) =
    x >= 0 && x < width && y >= 0 && y < height

  let can_go { walls; width; height; allowed; allowed_radius; _ } (x, y) =
    x >= 0 && x < width && y >= 0 && y < height
    && (Point.distance (x, y) allowed < allowed_radius
       || not (PointSet.mem (x, y) walls))

  let is_wall { walls; width; height; _ } (x, y) =
    x >= 0 && x < width && y >= 0 && y < height && PointSet.mem (x, y) walls

  let estimate_distance = Point.distance

  let neighbors g p =
    match (g.allow_wall, is_wall g p) with
    | false, _ ->
        [
          Point.move p (-1, 0);
          Point.move p (1, 0);
          Point.move p (0, -1);
          Point.move p (0, 1);
        ]
        |> List.filter (can_go g)
        |> List.map (fun p -> (1, p))
    | true, false ->
        [
          Point.move p (-1, 0);
          Point.move p (1, 0);
          Point.move p (0, -1);
          Point.move p (0, 1);
        ]
        |> List.filter (is_on_map g)
        |> List.map (fun p -> (1, p))
    | true, true -> []
end

module GraphSearch = AStar.Make (Graph)

let task_input () =
  let data = read_data_as_string "data/day20.txt" in
  let input = Input.of_string data in
  Graph.of_input input

let runPart1 graph =
  let base_cost, _ =
    GraphSearch.find_path graph graph.start graph.finish |> Option.get
  in
  let max_cost = base_cost - 100 in
  let attempt w =
    let g = Graph.with_exception graph w 1 in
    GraphSearch.find_path_constrained g max_cost g.start g.finish
    |> Option.map fst
    |> Option.value ~default:base_cost
  in
  let all_attempts = List.map attempt (PointSet.elements graph.walls) in
  all_attempts |> List.filter (fun x -> base_cost - x >= 100) |> List.length

let runPart2 graph =
  let base_cost, _ =
    GraphSearch.find_path graph graph.start graph.finish |> Option.get
  in
  let costs, _ =
    GraphSearch.find_all_paths (Graph.allow_wall graph) graph.start
  in
  let find_all_paths w =
    Graph.in_radius graph w 20
    |> Seq.filter (fun p -> not (Graph.is_wall graph p))
    |> Seq.filter_map (fun p ->
           GraphSearch.VertexMap.find_opt p costs
           |> Option.map (fun c -> (p, c)))
  in
  let attempt exit =
    let result = GraphSearch.find_path graph exit graph.finish in
    match result with
    | None -> []
    | Some (c1, _) ->
        find_all_paths exit
        |> Seq.map (fun (enter, c2) ->
               (enter, c1 + c2 + Point.distance enter exit))
        |> Seq.map (fun (enter, c) -> (enter, exit, base_cost - c))
        |> Seq.filter (fun (_, _, c) -> c > 0)
        |> List.of_seq
  in
  let pts_to_try =
    Graph.all_pos graph
    |> Seq.filter (fun pt -> not (Graph.is_wall graph pt))
    |> List.of_seq
  in
  List.map attempt pts_to_try
  |> List.concat
  |> List.filter (fun (_, _, x) -> x >= 100)
  |> List.length

let run () =
  let input = task_input () in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 20: %d %d\n" part1 part2
