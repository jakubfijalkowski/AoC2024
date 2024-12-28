open Utils

type schematic = Key of int array | Lock of int array

module Input = struct
  type t = { schematics : schematic list }

  let rec count_key_height lines at =
    match lines with
    | [] -> 0
    | line :: rest ->
        if line.[at] = '#' then 1 + count_key_height rest at else 0

  let schematic_of_string lines =
    let mk_heights lines =
      let len = List.hd lines |> String.length in
      Array.init len (fun i -> count_key_height lines i - 1)
    in
    if (List.hd lines).[0] = '#' then Lock (mk_heights lines)
    else Key (mk_heights (List.rev lines))

  let of_string s =
    let lines = String.split_on_char '\n' s in
    let schematics = ListExt.split_on_empty lines in
    { schematics = List.map schematic_of_string schematics }
end

let heights_fit a b = Array.for_all2 (fun a b -> a + b <= 5) a b

let is_match a b =
  match (a, b) with
  | Key a, Lock b -> heights_fit a b
  | Lock a, Key b -> heights_fit a b
  | _ -> false

let task_input () =
  let data = read_data_as_string "data/day25.txt" in
  Input.of_string data

let runPart1 (input : Input.t) =
  let keys, locks =
    List.partition (function Key _ -> true | _ -> false) input.schematics
  in
  List.fold_left
    (fun acc key ->
      acc
      + List.fold_left
          (fun acc lock -> if is_match key lock then acc + 1 else acc)
          0 locks)
    0 keys

let runPart2 _input = 0

let run () =
  let input = task_input () in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 25: %d %d\n" part1 part2
