let read_data_as_string filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let buf = Bytes.create len in
  really_input chan buf 0 len;
  close_in chan;
  String.trim (Bytes.to_string buf)

type way = Increasing | Decreasing

let way_to_string = function
  | Increasing -> "Increasing"
  | Decreasing -> "Decreasing"

let to_report line = 
  let parts = String.split_on_char ' ' line in
  let parts = List.filter (fun x -> x <> "") parts in
  List.map int_of_string parts

let detect_way lst =
  let fst = List.hd lst in
  let snd = List.nth lst 1 in
  if fst < snd then Increasing else Decreasing

let is_safe lst =
  let way = detect_way lst in
  fst (List.fold_left (fun acc x ->
    let (r, prev) = acc in
    if r = false then (false, 0)
    else
      match way with
      | Increasing -> if prev < x && (x - prev) <= 3 then (true, x) else (false, x)
      | Decreasing -> if prev > x && (prev - x) <= 3 then (true, x) else (false, x)
  ) (true, List.hd lst) (List.tl lst))

let without_i lst excl =
  let indexed = List.mapi (fun i x -> (i, x)) lst in
  let filtered = List.filter (fun (i, _) -> excl <> i) indexed in
  List.map snd filtered

let is_safe_removing lst =
  if is_safe lst then true
  else
    let with_removed = List.mapi (fun i _ -> without_i lst i) lst in
    List.exists is_safe with_removed

let runPart1 = List.fold_left (fun acc x -> acc + if is_safe x then 1 else 0) 0 

let runPart2 = List.fold_left (fun acc x -> acc + if is_safe_removing x then 1 else 0) 0 

let run () =
  let data = read_data_as_string "data/day02.txt" in
  let lines = String.split_on_char '\n' (String.trim data) in
  let reports = List.map to_report lines in
  let part1 = runPart1 reports in
  let part2 = runPart2 reports in
  Printf.printf "Day 02: %d %d\n" part1 part2