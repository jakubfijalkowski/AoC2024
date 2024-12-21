open Utils

module Input = struct
  type t = Point.t list

  let of_string s =
    String.split_on_char '\n' s
    |> List.map (String.split_on_char ',')
    |> List.map (function
         | [ x; y ] -> (int_of_string x, int_of_string y)
         | _ -> failwith "invalid input")
end

module Graph = struct
  module Vertex = Point
  module Cost = Int
  module PointSet = Set.Make (Point)

  type t = { walls : PointSet.t; width : int; height : int }

  let of_input input width height =
    { walls = PointSet.of_list input; width; height }

  let of_input take input width height =
    { walls = PointSet.of_list (ListExt.take take input); width; height }

  let can_go { walls; width; height } (x, y) =
    x >= 0 && x < width && y >= 0 && y < height
    && not (PointSet.mem (x, y) walls)

  let estimate_distance = Point.distance

  let neighbors g p =
    [
      Point.move p (-1, 0);
      Point.move p (1, 0);
      Point.move p (0, -1);
      Point.move p (0, 1);
    ]
    |> List.filter (can_go g)
    |> List.map (fun p -> (1, p))
end

module GraphSearch = AStar.Make (Graph)

let task_input () =
  let data = read_data_as_string "data/day18.txt" in
  Input.of_string data

let runPart1 input =
  let graph = Graph.of_input 1024 input 71 71 in
  let cost, _ =
    GraphSearch.find_path graph (0, 0) (graph.width - 1, graph.height - 1)
    |> Option.get
  in
  cost

let runPart2 input =
  let pt =
    List.find_mapi
      (fun i x ->
        let graph = Graph.of_input (i + 1) input 71 71 in
        let result =
          GraphSearch.find_path graph (0, 0) (graph.width - 1, graph.height - 1)
        in
        if Option.is_none result then Some x else None)
      input
  in
  let x, y = Option.get pt in
  Printf.sprintf "%d,%d\n" x y

let run () =
  let input = task_input () in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 18: %d %s\n" part1 part2
