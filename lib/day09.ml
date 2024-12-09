open Utils

module Input = struct
  type file = { tag : int; size : int }
  type t = { files_rev : file list; empty : int list }

  let zeroc = int_of_char '0'

  let of_string s =
    let _, files, empty =
      String.to_seq s
      |> Seq.map (fun c -> int_of_char c - zeroc)
      |> Seq.fold_lefti
           (fun (to_files, files, empty) i c ->
             if to_files then (false, { tag = i / 2; size = c } :: files, empty)
             else (true, files, c :: empty))
           (true, [], [])
    in
    { files_rev = files; empty = List.rev empty }

  let files_to_string files =
    List.map (fun f -> String.make f.size (char_of_int (f.tag + zeroc))) files
    |> String.concat " "
end

module Slotted = struct
  type slot = File of Input.file | Empty of int

  let of_input (i : Input.t) =
    Seq.interleave
      (Seq.map (fun f -> File f) (List.rev i.files_rev |> List.to_seq))
      (Seq.map (fun e -> Empty e) (List.to_seq i.empty))
    |> List.of_seq

  let flatten slots =
    List.fold_left
      (fun acc s -> match s with File f -> f :: acc | _ -> acc)
      [] slots

  let to_string slots =
    List.map
      (fun s ->
        match s with
        | File f -> String.make f.size (char_of_int (f.tag + Input.zeroc))
        | Empty e -> String.make e '.')
      slots
    |> String.concat ""
end

let fragment_the_disk (i : Input.t) =
  let rec fill which (from_beg : Input.file list) (from_end : Input.file list)
      empty =
    match (which, from_beg, from_end, empty) with
    | _, _, _, [] -> from_beg
    | _, b :: _, e :: _, _ when b.tag >= e.tag -> [ e ]
    | true, b :: bs, _, _ -> b :: fill false bs from_end empty
    | false, _, e :: es, f :: fs ->
        if f = 0 then fill true from_beg from_end fs
        else if e.size = f then e :: fill true from_beg es fs
        else if e.size > f then
          { e with size = f }
          :: fill true from_beg ({ e with size = e.size - f } :: es) fs
        else e :: fill false from_beg es ((f - e.size) :: fs)
    | _ -> failwith "can't happen?"
  in
  fill true (List.rev i.files_rev) i.files_rev i.empty

let defragment_the_disk (i : Input.t) =
  let rec combine_empty (slots : Slotted.slot list) =
    match slots with
    | [] -> []
    | Slotted.Empty e1 :: Slotted.Empty e2 :: es ->
        combine_empty (Slotted.Empty (e1 + e2) :: es)
    | e :: es -> e :: combine_empty es
  in
  let replace_with_empty tag (slots : Slotted.slot list) =
    List.map
      (fun s ->
        match s with
        | Slotted.File f -> if f.tag = tag then Slotted.Empty f.size else s
        | _ -> s)
      slots
    |> combine_empty
  in
  let rec try_fit (slots : Slotted.slot list) (file : Input.file) =
    match slots with
    | [] -> []
    | s :: ss -> (
        match s with
        | Slotted.File f ->
            s :: (if f.tag == file.tag then ss else try_fit ss file)
        | Slotted.Empty e ->
            if e = file.size then
              Slotted.File file :: replace_with_empty file.tag ss
            else if e > file.size then
              Slotted.File file
              :: Slotted.Empty (e - file.size)
              :: replace_with_empty file.tag ss
            else s :: try_fit ss file)
  in
  List.fold_left try_fit (Slotted.of_input i) i.files_rev

let checksum (files : Input.file list) =
  let single pos (f : Input.file) =
    Seq.init f.size (fun i -> pos + i)
    |> Seq.fold_left (fun acc p -> acc + (p * f.tag)) 0
  in
  let rec checksum' pos (files : Input.file list) =
    match files with
    | [] -> 0
    | f :: fs -> single pos f + checksum' (pos + f.size) fs
  in
  checksum' 0 files

let checksum_slotted (slots : Slotted.slot list) =
  let single pos (f : Input.file) =
    Seq.init f.size (fun i -> pos + i)
    |> Seq.fold_left (fun acc p -> acc + (p * f.tag)) 0
  in
  let rec checksum' pos (slots : Slotted.slot list) =
    match slots with
    | [] -> 0
    | Slotted.File f :: fs -> single pos f + checksum' (pos + f.size) fs
    | Slotted.Empty e :: fs -> checksum' (pos + e) fs
  in
  checksum' 0 slots

let runPart1 (i : Input.t) = fragment_the_disk i |> checksum
let runPart2 i = defragment_the_disk i |> checksum_slotted

let run () =
  let data = read_data_as_string "data/day09.txt" in
  let input = Input.of_string data in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 09: %d %d\n" part1 part2
