open Utils

module Input = struct
  type t = (string * string) list

  let of_string s =
    let lines = String.split_on_char '\n' s in
    List.map (String.split_on_char '-') lines
    |> List.map (function [ a; b ] -> (a, b) | _ -> failwith "Invalid input")
end

module Group = struct
  type t = string * string * string

  let create a b c =
    [ a; b; c ] |> List.sort compare |> function
    | [ a; b; c ] -> (a, b, c)
    | _ -> failwith "Invalid group"

  let compare (a1, b1, c1) (a2, b2, c2) =
    match String.compare a1 a2 with
    | 0 -> (
        match String.compare b1 b2 with 0 -> String.compare c1 c2 | n -> n)
    | n -> n
end

module EdgeMap = Map.Make (String)
module VertexSet = Set.Make (String)
module GroupSet = Set.Make (Group)

module Graph = struct
  type t = { edges : string list EdgeMap.t; vertices : VertexSet.t }

  let of_input (input : Input.t) =
    let edges =
      List.fold_left
        (fun acc (a, b) ->
          let add_edge map a b =
            EdgeMap.update a
              (function None -> Some [ b ] | Some edges -> Some (b :: edges))
              map
          in
          add_edge (add_edge acc a b) b a)
        EdgeMap.empty input
    in
    let vertices =
      List.fold_left
        (fun acc (a, b) -> VertexSet.add a (VertexSet.add b acc))
        VertexSet.empty input
    in
    { edges; vertices }

  let edges_of graph v = EdgeMap.find v graph.edges
  let has_connection graph a b = edges_of graph a |> List.mem b

  let connected_pairs_for graph vertex =
    let connections = edges_of graph vertex |> List.to_seq in
    Seq.map (fun a -> Seq.map (fun b -> (a, b)) connections) connections
    |> Seq.concat
    |> Seq.filter (fun (a, b) -> a < b)
    |> Seq.filter (fun (a, b) -> has_connection graph a b)

  let all_vertices_in graph group =
    let a, b, c = group in
    let set =
      edges_of graph a @ edges_of graph b @ edges_of graph c
      |> VertexSet.of_list
    in
    set
end

module SubGraph = struct
  type t = {
    group : Group.t;
    vertices : VertexSet.t;
    edges : VertexSet.t EdgeMap.t;
  }

  let of_group graph group =
    let vertices = Graph.all_vertices_in graph group in
    let edges =
      vertices |> VertexSet.elements
      |> List.map (fun v ->
             ( v,
               Graph.edges_of graph v
               |> List.filter (fun b -> VertexSet.mem b vertices)
               |> VertexSet.of_list ))
      |> EdgeMap.of_list
    in
    { group; vertices; edges }

  let edges_of graph v = EdgeMap.find v graph.edges
  let has_connection graph a b = edges_of graph a |> VertexSet.mem b

  let naive_clique subgraph =
    (* Bron-Kerbosch algorithm *)
    let rec bron_kerbosch r p x =
      if VertexSet.is_empty p && VertexSet.is_empty x then [ r ]
      else
        let reported, _, _ =
          VertexSet.fold
            (fun v (acc, p, x) ->
              let r = VertexSet.add v r in
              let n = edges_of subgraph v in
              let acc =
                bron_kerbosch r (VertexSet.inter p n) (VertexSet.inter x n)
                @ acc
              in
              (acc, VertexSet.remove v p, VertexSet.add v x))
            p ([], p, x)
        in
        reported
    in
    bron_kerbosch VertexSet.empty subgraph.vertices VertexSet.empty
    |> ListExt.max_by VertexSet.cardinal
    |> Option.get |> snd
end

let task_input () =
  let data = read_data_as_string "data/day23.txt" in
  Input.of_string data

let runPart1 input =
  let g = Graph.of_input input in
  let filtered =
    VertexSet.elements g.vertices
    |> List.filter (fun v -> v.[0] = 't')
    |> List.to_seq
  in
  filtered
  |> Seq.map (fun a ->
         Graph.connected_pairs_for g a
         |> Seq.map (fun (b, c) -> Group.create a b c))
  |> Seq.concat |> GroupSet.of_seq |> GroupSet.cardinal

let runPart2 input =
  let g = Graph.of_input input in
  let filtered =
    VertexSet.elements g.vertices
    |> List.filter (fun v -> v.[0] = 't')
    |> List.to_seq
  in
  let cliques =
    filtered
    |> Seq.map (fun a ->
           Graph.connected_pairs_for g a
           |> Seq.map (fun (b, c) -> Group.create a b c))
    |> Seq.concat |> GroupSet.of_seq |> GroupSet.elements
    |> List.map (SubGraph.of_group g)
    |> List.map SubGraph.naive_clique
  in
  let _, max_clique = ListExt.max_by VertexSet.cardinal cliques |> Option.get in
  VertexSet.elements max_clique |> List.sort String.compare |> String.concat ","

let run () =
  let input = task_input () in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 23: %d %s\n" part1 part2
