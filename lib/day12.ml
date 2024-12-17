open Utils

module Map = struct
  type 'a t = { map : 'a array array; width : int; height : int }
  type input = char t

  let of_string s =
    let lines = String.split_on_char '\n' s in
    let width = String.length (List.hd lines) in
    let height = List.length lines in
    let map =
      Array.of_list (List.map (fun l -> Array.of_seq (String.to_seq l)) lines)
    in
    { map; width; height }

  let is_in { width; height; _ } (x, y) =
    x >= 0 && x < width && y >= 0 && y < height

  let get_at m (x, y) = if is_in m (x, y) then Some m.map.(y).(x) else None

  let make_map_based_on m i =
    { m with map = Array.init_matrix m.height m.width (fun _ _ -> i) }

  let all_pos m =
    Seq.init m.height (fun y -> Seq.init m.width (fun x -> (x, y)))
    |> Seq.concat
end

module Point = struct
  let offset (x, y) (dx, dy) = (x + dx, y + dy)
end

module Region = struct
  type t = { area : int; perimeter : int; sides : int }

  let of_regions i =
    Array.init i (fun _ -> { area = 0; perimeter = 0; sides = 0 })

  let add_area regions p =
    match p with
    | Some i ->
        let r = regions.(i) in
        regions.(i) <- { r with area = r.area + 1 };
        regions
    | None -> regions

  let add_perimeter regions p =
    match p with
    | Some i ->
        let r = regions.(i) in
        regions.(i) <- { r with perimeter = r.perimeter + 1 };
        regions
    | None -> regions

  let add_side regions p =
    match p with
    | Some i ->
        let r = regions.(i) in
        regions.(i) <- { r with sides = r.sides + 1 };
        regions
    | None -> regions
end

let flood_fill i map expect filled p =
  let rec flood_fill' filled (x, y) =
    match Map.get_at filled (x, y) with
    | Some -1 when Map.get_at map (x, y) = Some expect ->
        filled.map.(y).(x) <- i;
        [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
        |> List.fold_left flood_fill' filled
    | _ -> filled
  in
  flood_fill' filled p

let fill_all_regions map =
  let fill_if_empty (acc, filled) p =
    if Map.get_at filled p = Some (-1) then
      (acc + 1, flood_fill acc map (Map.get_at map p |> Option.get) filled p)
    else (acc, filled)
  in
  Map.all_pos map
  |> Seq.fold_left fill_if_empty (0, Map.make_map_based_on map (-1))

let is_side a b = not (Option.equal ( = ) a b)

let add_perimeter r m p1 p2 =
  let a = Map.get_at m p1 in
  let b = Map.get_at m p2 in
  if is_side a b then
    let r = Region.add_perimeter r a in
    let r = Region.add_perimeter r b in
    r
  else r

let count_area regions (map : int Map.t) =
  Map.all_pos map
  |> Seq.map (Map.get_at map)
  |> Seq.fold_left Region.add_area regions

let count_perimeter regions (map : int Map.t) =
  let count_top regions =
    Seq.init map.width (fun x -> (x, 0))
    |> Seq.fold_left
         (fun regions p -> add_perimeter regions map p (fst p, -1))
         regions
  in

  let count_left regions =
    Seq.init map.height (fun y -> (0, y))
    |> Seq.fold_left
         (fun regions p -> add_perimeter regions map p (-1, snd p))
         regions
  in
  let count_inner regions =
    Map.all_pos map
    |> Seq.fold_left
         (fun regions p ->
           let x, y = p in
           let regions = add_perimeter regions map p (x + 1, y) in
           let regions = add_perimeter regions map p (x, y + 1) in
           regions)
         regions
  in
  count_inner regions |> count_top |> count_left

let add_side r m p1 p2 d =
  let a = Map.get_at m p1 in
  let b = Map.get_at m p2 in
  let prevA = Map.get_at m (Point.offset p1 d) in
  let prevB = Map.get_at m (Point.offset p2 d) in
  if is_side a b then
    let prev_is_side = is_side prevA prevB in
    let r =
      if (not prev_is_side) || not (Option.equal ( = ) a prevA) then
        Region.add_side r a
      else r
    in
    let r =
      if (not prev_is_side) || not (Option.equal ( = ) b prevB) then
        Region.add_side r b
      else r
    in
    r
  else r

let count_sides regions (map : int Map.t) =
  let count_top regions =
    Seq.init map.width (fun x -> (x, 0))
    |> Seq.fold_left
         (fun regions p -> add_side regions map p (fst p, -1) (-1, 0))
         regions
  in

  let count_left regions =
    Seq.init map.height (fun y -> (0, y))
    |> Seq.fold_left
         (fun regions p -> add_side regions map p (-1, snd p) (0, -1))
         regions
  in
  let count_inner regions =
    Map.all_pos map
    |> Seq.fold_left
         (fun regions p ->
           let x, y = p in
           let regions = add_side regions map p (x + 1, y) (0, -1) in
           let regions = add_side regions map p (x, y + 1) (-1, 0) in
           regions)
         regions
  in
  count_inner regions |> count_top |> count_left

let runPart1 map =
  let no_regions, filled = fill_all_regions map in
  let regions = Region.of_regions no_regions in
  let regions = count_perimeter regions filled in
  let regions = count_area regions filled in
  Array.fold_left
    (fun acc (r : Region.t) -> acc + (r.area * r.perimeter))
    0 regions

let runPart2 map =
  let no_regions, filled = fill_all_regions map in
  let regions = Region.of_regions no_regions in
  let regions = count_sides regions filled in
  let regions = count_area regions filled in
  Array.fold_left (fun acc (r : Region.t) -> acc + (r.area * r.sides)) 0 regions

let run () =
  let data = read_data_as_string "data/day12.txt" in
  let map = Map.of_string data in
  let part1 = runPart1 map in
  let part2 = runPart2 map in
  Printf.printf "Day 11: %d %d\n" part1 part2
