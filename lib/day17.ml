open Utils

module Program = struct
  type t = {
    reg_a : int;
    reg_b : int;
    reg_c : int;
    pc : int;
    program : int array;
    output : int list;
  }

  type instr =
    | Adv of int
    | Bxl of int
    | Bst of int
    | Jnz of int
    | Bxc of int
    | Out of int
    | Bdv of int
    | Cdv of int

  let after_colon s =
    let idx = String.index s ':' in
    String.sub s (idx + 2) (String.length s - idx - 2)

  let of_string s =
    let lines = String.split_on_char '\n' s in
    match lines with
    | [ a; b; c; _; prg ] ->
        {
          reg_a = after_colon a |> int_of_string;
          reg_b = after_colon b |> int_of_string;
          reg_c = after_colon c |> int_of_string;
          pc = 0;
          program =
            after_colon prg |> String.split_on_char ','
            |> List.map int_of_string |> Array.of_list;
          output = [];
        }
    | _ -> failwith "Invalid input"

  let combo_arg prg = function
    | (0 | 1 | 2 | 3) as x -> x
    | 4 -> prg.reg_a
    | 5 -> prg.reg_b
    | 6 -> prg.reg_c
    | 7 -> failwith "Invalid 7"
    | _ -> failwith "Invalid argument"

  let parse_instruction prg =
    let arg = prg.program.(prg.pc + 1) in
    match prg.program.(prg.pc) with
    | 0 -> Adv (combo_arg prg arg)
    | 1 -> Bxl arg
    | 2 -> Bst (combo_arg prg arg)
    | 3 -> Jnz arg
    | 4 -> Bxc arg
    | 5 -> Out (combo_arg prg arg)
    | 6 -> Bdv (combo_arg prg arg)
    | 7 -> Cdv (combo_arg prg arg)
    | x -> failwith (Printf.sprintf "Invalid opcode %d" x)

  let execute_step prg =
    if prg.pc >= Array.length prg.program then None
    else
      let instr = parse_instruction prg in
      let advance prg = { prg with pc = prg.pc + 2 } in
      let new_prg =
        match instr with
        | Adv arg -> advance { prg with reg_a = prg.reg_a / ipow 2 arg }
        | Bxl arg -> advance { prg with reg_b = prg.reg_b lxor arg }
        | Bst arg -> advance { prg with reg_b = arg mod 8 }
        | Jnz arg ->
            if prg.reg_a = 0 then advance prg else { prg with pc = arg }
        | Bxc _ -> advance { prg with reg_b = prg.reg_b lxor prg.reg_c }
        | Out arg -> advance { prg with output = (arg mod 8) :: prg.output }
        | Bdv arg -> advance { prg with reg_b = prg.reg_a / ipow 2 arg }
        | Cdv arg -> advance { prg with reg_c = prg.reg_a / ipow 2 arg }
      in
      Some new_prg

  let rec run prg =
    match execute_step prg with
    | None -> List.rev prg.output
    | Some new_prg -> run new_prg
end

let runPart1 program =
  let output = Program.run program in
  output |> List.map string_of_int |> String.concat ","

let runPart2 (program : Program.t) =
  (*
   Does not work and I'm lost in the bitwise operations world.
  *)
  let dbg_tos result = result |> List.map string_of_int |> String.concat "," in
  let calc_a coeffs for_n use_n =
    ArrayExt.foldi_left
      (fun i acc x -> acc + ((if i = for_n then use_n else x) * ipow 8 i))
      0 coeffs
  in
  let coeffs = Array.make (Array.length program.program) 0 in
  let find_one i expected =
    let rec aux attempted =
      if attempted = 8 then failwith "Not found"
      else
        let new_program = { program with reg_a = calc_a coeffs i attempted } in
        let result = Program.run new_program in
        Printf.printf "Attempted %d for %d: %s\n" attempted i (dbg_tos result);
        let result = List.nth_opt result i |> Option.value ~default:(-1) in
        if result = expected then attempted else aux (attempted + 1)
    in
    let r = aux 0 in
    Printf.printf "Found %d for %d\n" r i;
    r
  in
  for _ = 0 to 0 do
    for i = 0 to Array.length program.program - 1 do
      coeffs.(i) <-
        find_one i program.program.(Array.length program.program - i - 1)
    done
  done;
  let new_program = { program with reg_a = calc_a coeffs (-1) 0 } in
  let result = Program.run new_program in
  result |> List.map string_of_int |> String.concat ","

let read_program () =
  let data = read_data_as_string "data/day17.txt" in
  Program.of_string data

let run () =
  let program = read_program () in
  let part1 = runPart1 program in
  let part2 = runPart2 program in
  Printf.printf "Day 17 - part 1: %s\n" part1;
  Printf.printf "Day 17 - part 2: %s\n" part2
