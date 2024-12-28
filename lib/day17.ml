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

  let instr_to_str = function
    | Adv x -> Printf.sprintf "Adv %10d" x
    | Bxl x -> Printf.sprintf "Bxl %10d" x
    | Bst x -> Printf.sprintf "Bst %10d" x
    | Jnz x -> Printf.sprintf "Jnz %10d" x
    | Bxc x -> Printf.sprintf "Bxc %10d" x
    | Out x -> Printf.sprintf "Out %10d" x
    | Bdv x -> Printf.sprintf "Bdv %10d" x
    | Cdv x -> Printf.sprintf "Cdv %10d" x

  let state_to_str prg =
    Printf.sprintf "A=%d, B=%d, C=%d, O=[%s]" prg.reg_a prg.reg_b prg.reg_c
      (List.rev prg.output |> List.map string_of_int |> String.concat " ")

  let run_dbg prg steps =
    Printf.printf "Initial: %s\n" (state_to_str prg);
    let rec aux prg steps =
      if steps = 0 then Some prg
      else
        let instr = parse_instruction prg in
        Printf.printf "%s " (instr_to_str instr);
        match execute_step prg with
        | None ->
            Printf.printf "(HALT!)\n";
            None
        | Some new_prg ->
            Printf.printf "-> %s\n" (state_to_str new_prg);
            if new_prg.pc = 0 then Printf.printf "\n";
            aux new_prg (steps - 1)
    in
    aux prg steps
end

let runPart1 program =
  let output = Program.run program in
  output |> List.map string_of_int |> String.concat ","

let runPart2 (program : Program.t) =
  let calc_a coeffs =
    let padded =
      List.init (Array.length program.program - List.length coeffs) (fun _ -> 1)
    in
    padded @ coeffs
    |> List.mapi (fun i x -> (i, x))
    |> List.fold_left (fun acc (i, x) -> acc + (x * ipow 8 i)) 0
  in
  let rec try_find i coeffs =
    if i = -1 then Some coeffs
    else
      let att =
        List.init 8 (fun x -> x)
        |> List.filter_map (fun x ->
               let res =
                 Program.run { program with reg_a = calc_a (x :: coeffs) }
               in
               if
                 List.length res = Array.length program.program
                 && List.nth res i = program.program.(i)
               then try_find (i - 1) (x :: coeffs)
               else None)
      in
      match att with x :: _ -> Some x | _ -> None
  in
  let final_coeffs =
    try_find (Array.length program.program - 1) [] |> Option.get
  in
  calc_a final_coeffs

let read_program () =
  let data = read_data_as_string "data/day17.txt" in
  Program.of_string data

let run () =
  let program = read_program () in
  let part1 = runPart1 program in
  let part2 = runPart2 program in
  Printf.printf "Day 17 - part 1: %s\n" part1;
  Printf.printf "Day 17 - part 2: %d\n" part2
