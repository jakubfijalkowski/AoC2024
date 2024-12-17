open Utils

module Point = struct
  type t = int * int

  let distance_to (x0, y0) (x1, y1) =
    let dx = x1 - x0 in
    let dy = y1 - y0 in
    sqrt (float_of_int ((dx * dx) + (dy * dy)))
end

module Robot = struct
  type pt = Point.t
  type t = { p : pt; v : pt }

  let robot_re = Re.Pcre.regexp "p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)"

  let of_string s =
    let m = Re.exec robot_re s in
    let p =
      (Re.Group.get m 1 |> int_of_string, Re.Group.get m 2 |> int_of_string)
    in
    let v =
      (Re.Group.get m 3 |> int_of_string, Re.Group.get m 4 |> int_of_string)
    in
    { p; v }

  let all_of_string data = String.split_on_char '\n' data |> List.map of_string

  let step r =
    let px, py = r.p in
    let vx, vy = r.v in
    { r with p = (px + vx, py + vy) }

  let rec wrap max_x max_y r =
    let wrap' = wrap max_x max_y in
    let px, py = r.p in
    if px < 0 then wrap' { r with p = (max_x + px, py) }
    else if px >= max_x then wrap' { r with p = (px - max_x, py) }
    else if py < 0 then wrap' { r with p = (px, max_y + py) }
    else if py >= max_y then wrap' { r with p = (px, py - max_y) }
    else r

  let which_q max_x max_y r =
    let hx = max_x / 2 in
    let hy = max_y / 2 in
    let px, py = r.p in
    if px < hx && py < hy then Some 0
    else if px > hx && py < hy then Some 1
    else if px < hx && py > hy then Some 2
    else if px > hx && py > hy then Some 3
    else None

  let print max_x max_y robots =
    for y = 0 to max_y do
      for x = 0 to max_x do
        let c =
          if List.exists (fun r -> r.p = (x, y)) robots then '#' else '.'
        in
        Printf.printf "%c" c
      done;
      Printf.printf "\n"
    done;
    Printf.printf "\n%!"
end

let variance_of robots =
  let rec aux a rs =
    let rest = match rs with [] -> 0. | b :: bs -> aux b bs in
    rest
    +. List.fold_left
         (fun acc b -> acc +. Float.pow (Point.distance_to a b) 2.0)
         0.0 rs
  in
  let n = float_of_int (List.length robots) in
  aux (List.hd robots) (List.tl robots) /. Float.pow n 2.0

let runPart1 robots =
  let wrap = Robot.wrap 101 103 in
  let which_q = Robot.which_q 101 103 in
  let result = Array.init 4 (fun _ -> 0) in
  List.init 100 (fun _ -> 0)
  |> List.fold_left
       (fun robots _ -> List.map (fun r -> Robot.step r |> wrap) robots)
       robots
  |> List.filter_map which_q
  |> List.fold_left
       (fun result q ->
         result.(q) <- result.(q) + 1;
         result)
       result
  |> Array.fold_left ( * ) 1

let runPart2 robots =
  let wrap = Robot.wrap 101 103 in
  let next = List.map (fun r -> Robot.step r |> wrap) in
  let select = List.map (fun (r : Robot.t) -> r.p) in
  let rec try_find i robots =
    if i > 10000 then None
    else
      let variance = variance_of (select robots) in
      if variance < 800.0 then Some (i, robots)
      else try_find (i + 1) (next robots)
  in
  match try_find 0 robots with
  | None -> 0
  | Some (i, robots) ->
      Robot.print 101 103 robots;
      i

let all_robots () =
  let data = read_data_as_string "data/day14.txt" in
  Robot.all_of_string data

let run () =
  let robots = all_robots () in
  let part1 = runPart1 robots in
  let part2 = runPart2 robots in
  Printf.printf "Day 14: %d %d\n" part1 part2
