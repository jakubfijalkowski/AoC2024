open Utils

module Input = struct
  type t = { towels : string list; patterns : string list }

  let of_string s =
    let lines = String.split_on_char '\n' s in
    let parts = ListExt.split_on_empty lines in
    let towels, patterns =
      match parts with
      | [ [ towels ]; patterns ] ->
          (String.split_on_char ',' towels |> List.map String.trim, patterns)
      | _ -> failwith "invalid input"
    in
    { towels; patterns }
end

let is_possible (input : Input.t) pattern =
  let rec aux pattern =
    if String.equal pattern "" then true
    else
      input.towels
      |> ListExt.any_predicate (fun t ->
             String.starts_with ~prefix:t pattern
             && aux
                  (String.sub pattern (String.length t)
                     (String.length pattern - String.length t)))
  in
  aux pattern

module Cache = Map.Make (String)

let count_possible (input : Input.t) pattern =
  let cache = ref (Cache.singleton "" 1) in
  let rec aux pattern =
    match Cache.find_opt pattern !cache with
    | Some x -> x
    | None ->
        let result =
          input.towels
          |> List.map (fun t ->
                 if String.starts_with ~prefix:t pattern then
                   aux
                     (String.sub pattern (String.length t)
                        (String.length pattern - String.length t))
                 else 0)
          |> ListExt.sum
        in
        cache := Cache.add pattern result !cache;
        result
  in
  aux pattern

let task_input () =
  let data = read_data_as_string "data/day19.txt" in
  Input.of_string data

let runPart1 input =
  input.patterns |> List.filter (is_possible input) |> List.length

let runPart2 input =
  input.patterns |> List.map (count_possible input) |> ListExt.sum

let run () =
  let input = task_input () in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 19: %d %d\n" part1 part2
