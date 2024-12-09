let read_data_as_string filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let buf = Bytes.create len in
  really_input chan buf 0 len;
  close_in chan;
  String.trim (Bytes.to_string buf)

type op = Do | Dont | Mul of int * int

let do_mul op = match op with Do -> 0 | Dont -> 0 | Mul (a, b) -> a * b

let do_op (can_do, acc) x =
  match x with
  | Do -> (true, acc)
  | Dont -> (false, acc)
  | Mul (a, b) -> (can_do, acc + if can_do then a * b else 0)

let op_of_group g =
  match Re.Group.get g 1 with
  | "mul" ->
      Mul (int_of_string (Re.Group.get g 2), int_of_string (Re.Group.get g 3))
  | "do" -> Do
  | "don't" -> Dont
  | _ -> failwith "Invalid op"

let all_ops s =
  let r = Re.Pcre.regexp "(mul|do|don't)\\(([0-9]+)?,?([0-9]+)?\\)" in
  let e = Re.all r s in
  List.map op_of_group e

let runPart1 s = List.fold_left (fun acc x -> acc + do_mul x) 0 (all_ops s)
let runPart2 s = List.fold_left do_op (true, 0) (all_ops s) |> snd

let run () =
  let data = read_data_as_string "data/day03.txt" in
  let part1 = runPart1 data in
  let part2 = runPart2 data in
  Printf.printf "Day 03: %d %d\n" part1 part2
