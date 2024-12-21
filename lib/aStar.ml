module type Cost = sig
  type t

  val compare : t -> t -> int
  val add : t -> t -> t
  val zero : t
end

module type Vertex = sig
  type t

  val compare : t -> t -> int
end

module type GraphMap = sig
  type t

  module Cost : Cost
  module Vertex : Vertex

  val estimate_distance : Vertex.t -> Vertex.t -> Cost.t
  val neighbors : t -> Vertex.t -> (Cost.t * Vertex.t) list
end

module type S = sig
  type map
  type cost
  type vertex

  module VertexMap : Map.S with type key = vertex

  val find_all_paths :
    map -> vertex -> cost VertexMap.t * vertex list VertexMap.t

  val find_path : map -> vertex -> vertex -> (cost * vertex list) Option.t
end

module Make (M : GraphMap) :
  S with type map = M.t and type cost = M.Cost.t and type vertex = M.Vertex.t =
struct
  type map = M.t
  type cost = M.Cost.t
  type vertex = M.Vertex.t

  module OpenSet = Psq.Make (M.Vertex) (M.Cost)
  module VertexMap = Map.Make (M.Vertex)

  let reconstruct_path cameFrom start finish =
    let rec aux acc p =
      let prev = VertexMap.find p cameFrom in
      if M.Vertex.compare prev start = 0 then p :: acc else aux (p :: acc) prev
    in
    aux [] finish

  let find_all_paths graph start =
    let rec run openSet gScore cameFrom =
      match OpenSet.pop openSet with
      | None -> (gScore, cameFrom)
      | Some ((p, _), newSet) ->
          let neighbors = M.neighbors graph p in
          let curr_score = VertexMap.find p gScore in
          let dirs =
            neighbors
            |> List.map (fun (c, dp) -> (M.Cost.add curr_score c, dp))
            |> List.filter_map (fun (c, dp) ->
                   match VertexMap.find_opt dp gScore with
                   | None -> Some (c, dp)
                   | Some s when M.Cost.compare c s <= 0 -> Some (c, dp)
                   | _ -> None)
          in
          let new_gScore =
            List.fold_left
              (fun acc (c, dp) -> VertexMap.add dp c acc)
              gScore dirs
          in
          let new_openSet =
            List.fold_left (fun acc (c, dp) -> OpenSet.add dp c acc) newSet dirs
          in
          let new_cameFrom =
            List.fold_left
              (fun acc (_, dp) ->
                VertexMap.update dp
                  (fun e -> Some (p :: Option.value ~default:[] e))
                  acc)
              cameFrom dirs
          in
          run new_openSet new_gScore new_cameFrom
    in
    run
      (OpenSet.sg start (M.estimate_distance start start))
      (VertexMap.singleton start M.Cost.zero)
      VertexMap.empty

  let find_path graph start finish =
    let rec run openSet gScore cameFrom =
      match OpenSet.pop openSet with
      | None -> None
      | Some ((p, _), _) when M.Vertex.compare p finish = 0 ->
          Some
            ( VertexMap.find finish gScore,
              reconstruct_path cameFrom start finish )
      | Some ((p, _), newSet) ->
          let neighbors = M.neighbors graph p in
          let curr_score = VertexMap.find p gScore in
          let dirs =
            neighbors
            |> List.map (fun (c, dp) -> (M.Cost.add curr_score c, dp))
            |> List.filter_map (fun (c, dp) ->
                   match VertexMap.find_opt dp gScore with
                   | None -> Some (c, dp)
                   | Some s when M.Cost.compare c s < 0 -> Some (c, dp)
                   | _ -> None)
          in
          let new_gScore =
            List.fold_left
              (fun acc (c, dp) -> VertexMap.add dp c acc)
              gScore dirs
          in
          let new_openSet =
            List.fold_left
              (fun acc (c, dp) ->
                OpenSet.add dp
                  (M.Cost.add c (M.estimate_distance dp finish))
                  acc)
              newSet dirs
          in
          let new_cameFrom =
            List.fold_left
              (fun acc (_, dp) -> VertexMap.add dp p acc)
              cameFrom dirs
          in
          run new_openSet new_gScore new_cameFrom
    in
    run
      (OpenSet.sg start (M.estimate_distance start start))
      (VertexMap.singleton start M.Cost.zero)
      VertexMap.empty
end
