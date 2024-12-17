open Utils

module Machine = struct
  type pt = int * int
  type t = { a : pt; b : pt; target : pt }

  let coords_re = Re.Pcre.regexp ".+: X.(\\d+), Y.(\\d+)"

  let coords_of_string s =
    let m = Re.exec coords_re s in
    (Re.Group.get m 1 |> int_of_string, Re.Group.get m 2 |> int_of_string)

  let of_string = function
    | [ a; b; c ] ->
        let a = coords_of_string a in
        let b = coords_of_string b in
        let target = coords_of_string c in
        { a; b; target }
    | _ -> failwith "Invalid input"

  let all_of_string data =
    String.split_on_char '\n' data
    |> ListExt.split_on_empty |> List.map of_string
end

module Naive = struct
  let solve_for_b (m : Machine.t) (leftx, lefty) =
    let bx, by = m.b in
    let m = leftx / bx in
    if m * bx = leftx && m * by = lefty then Some m else None

  let solve (m : Machine.t) =
    let ax, ay = m.a in
    List.init 101 (fun i ->
        let tx, ty = m.target in
        let leftx, lefty = (tx - (i * ax), ty - (i * ay)) in
        if leftx < 0 || lefty < 0 then (i, None)
        else (i, solve_for_b m (leftx, lefty)))
    |> List.filter_map (fun (a, b) -> Option.map (fun b -> (a, b)) b)
end

let float_of_pair (a, b) = (float_of_int a, float_of_int b)

module Analytical = struct
  let solve (m : Machine.t) =
    let xa, ya = float_of_pair m.a in
    let xb, yb = float_of_pair m.b in
    let xt, yt = float_of_pair m.target in
    let b = (yt -. (ya *. xt /. xa)) /. (yb -. (ya *. xb /. xa)) in
    let a = (xt -. (xb *. b)) /. xa in
    let xa, ya = m.a in
    let xb, yb = m.b in
    let xt, yt = m.target in
    let a = int_of_float (Float.round a) in
    let b = int_of_float (Float.round b) in
    if a >= 0 && b >= 0 && (a * xa) + (b * xb) = xt && (a * ya) + (b * yb) = yt
    then [ (a, b) ]
    else []

  let solve_raw (m : Machine.t) =
    let xa, ya = float_of_pair m.a in
    let xb, yb = float_of_pair m.b in
    let xt, yt = float_of_pair m.target in
    let b = (yt -. (ya *. xt /. xa)) /. (yb -. (ya *. xb /. xa)) in
    let a = (xt -. (xb *. b)) /. xa in
    (a, b)
end

let calc_tokens f (m : Machine.t) =
  match f m |> List.map (fun (a, b) -> (a * 3) + b) with
  | [] -> None
  | l -> Some (ListExt.min1 l)

let runPart1 machines =
  List.filter_map (calc_tokens Naive.solve) machines |> ListExt.sum

let runPart2 machines =
  let expand (m : Machine.t) =
    let tx, ty = m.target in
    { m with target = (10000000000000 + tx, 10000000000000 + ty) }
  in
  List.map expand machines
  |> List.filter_map (calc_tokens Analytical.solve)
  |> ListExt.sum

let all_machines () =
  let data = read_data_as_string "data/day13.txt" in
  let machines = Machine.all_of_string data in
  machines

let run () =
  let machines = all_machines () in
  let part1 = runPart1 machines in
  let part2 = runPart2 machines in
  Printf.printf "Day 13: %d %d\n" part1 part2
