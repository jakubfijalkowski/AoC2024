open Utils

type cell = Box | Wall | Robot | Empty | LeftBox | RightBox
type direction = Up | Down | Left | Right
type map = { data : cell array array; width : int; height : int }

module Input = struct
  type t = { map : map; movements : direction list }

  let cell_of_char = function
    | '#' -> Wall
    | '@' -> Robot
    | '.' -> Empty
    | 'O' -> Box
    | _ -> failwith "Invalid cell"

  let direction_of_char = function
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | _ -> failwith "Invalid direction"

  let movement_of_lines lines =
    String.concat "" lines |> String.to_seq |> Seq.map direction_of_char
    |> List.of_seq

  let find_robot map =
    Array.find_mapi
      (fun y line ->
        match
          Array.find_mapi (fun x c -> if c == Robot then Some x else None) line
        with
        | Some x -> Some (x, y)
        | None -> None)
      map
    |> Option.get

  let map_of_lines lines =
    let width = String.length (List.hd lines) in
    let height = List.length lines in
    let data =
      List.map
        (fun l -> String.to_seq l |> Seq.map cell_of_char |> Array.of_seq)
        lines
      |> Array.of_list
    in
    { data; width; height }

  let of_string str =
    let lines = String.split_on_char '\n' str in
    let parts = ListExt.split_on_empty lines in
    let map, movements =
      match parts with
      | [ map; movements ] -> (map_of_lines map, movement_of_lines movements)
      | _ -> failwith "Invalid input"
    in
    { map; movements }

  let enlarge_row row =
    let new_row = Array.make (Array.length row * 2) Empty in
    Array.iteri
      (fun i c ->
        match c with
        | Wall ->
            new_row.(i * 2) <- Wall;
            new_row.((i * 2) + 1) <- Wall
        | Robot ->
            new_row.(i * 2) <- Robot;
            new_row.((i * 2) + 1) <- Empty
        | Box ->
            new_row.(i * 2) <- LeftBox;
            new_row.((i * 2) + 1) <- RightBox
        | Empty ->
            new_row.(i * 2) <- Empty;
            new_row.((i * 2) + 1) <- Empty
        | _ -> failwith "can't enlarge that")
      row;
    new_row

  let enlarge input =
    let new_data = Array.map enlarge_row input.map.data in
    let new_map =
      { input.map with data = new_data; width = input.map.width * 2 }
    in
    { input with map = new_map }
end

let all_pos m =
  Seq.init m.height (fun y -> Seq.init m.width (fun x -> (x, y))) |> Seq.concat

let offset (x, y) = function
  | Up -> (x, y - 1)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)

let rec can_move m d (x, y) =
  let x', y' = offset (x, y) d in
  match m.data.(y).(x) with
  | Wall -> false
  | Box | Robot -> can_move m d (x', y')
  | (LeftBox | RightBox) when d = Left || d == Right -> can_move m d (x', y')
  | LeftBox -> can_move m d (x', y') && can_move m d (x' + 1, y')
  | RightBox -> can_move m d (x', y') && can_move m d (x' - 1, y')
  | Empty -> true

let rec move m d (x, y) =
  if can_move m d (x, y) then
    let x', y' = offset (x, y) d in
    match m.data.(y).(x) with
    | Wall -> false
    | Box | Robot ->
        move m d (x', y') |> ignore;
        m.data.(y').(x') <- m.data.(y).(x);
        m.data.(y).(x) <- Empty;
        true
    | (LeftBox | RightBox) when d = Left || d = Right ->
        move m d (x', y') |> ignore;
        m.data.(y').(x') <- m.data.(y).(x);
        m.data.(y).(x) <- Empty;
        true
    | LeftBox ->
        move m d (x', y') |> ignore;
        move m d (x' + 1, y') |> ignore;
        m.data.(y').(x') <- m.data.(y).(x);
        m.data.(y').(x' + 1) <- m.data.(y).(x + 1);
        m.data.(y).(x) <- Empty;
        m.data.(y).(x + 1) <- Empty;
        true
    | RightBox ->
        move m d (x', y') |> ignore;
        move m d (x' - 1, y') |> ignore;
        m.data.(y').(x') <- m.data.(y).(x);
        m.data.(y').(x' - 1) <- m.data.(y).(x - 1);
        m.data.(y).(x) <- Empty;
        m.data.(y).(x - 1) <- Empty;
        true
    | Empty -> true
  else false

let print_map m =
  for y = 0 to m.height - 1 do
    for x = 0 to m.width - 1 do
      match m.data.(y).(x) with
      | Wall -> Printf.printf "#"
      | Box -> Printf.printf "O"
      | LeftBox -> Printf.printf "["
      | RightBox -> Printf.printf "]"
      | Robot -> Printf.printf "@"
      | Empty -> Printf.printf "."
    done;
    Printf.printf "\n"
  done;
  Printf.printf "%!"

let run_game (input : Input.t) =
  let robot = ref (Input.find_robot input.map.data) in
  List.iter
    (fun d -> if move input.map d !robot then robot := offset !robot d)
    input.movements;
  all_pos input.map
  |> Seq.fold_left
       (fun acc (x, y) ->
         match input.map.data.(y).(x) with
         | Box | LeftBox -> acc + (100 * y) + x
         | _ -> acc)
       0

let runPart1 input = run_game input
let runPart2 input = run_game (Input.enlarge input)

let run () =
  let data = read_data_as_string "data/day15.txt" in
  let part1 = runPart1 (Input.of_string data) in
  let part2 = runPart2 (Input.of_string data) in
  Printf.printf "Day 15: %d %d\n" part1 part2
