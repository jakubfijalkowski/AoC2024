open Utils
module VariableMap = Map.Make (String)
module VariableSet = Set.Make (String)

type gate_values = { in1 : int option; in2 : int option; out : int option }
type gate_names = { in1 : string; in2 : string; out : string }
type gate_var = { names : gate_names; values : gate_values }
type gate = AND of gate_var | OR of gate_var | XOR of gate_var

type variable = {
  name : string;
  connected_to : string list;
  value : int option;
}

type vertex = Variable of variable | Gate of gate

let bxor a b = if a != b then true else false
let mk_gate_name v = "G_" ^ v
let is_gate v = String.starts_with ~prefix:"G_" v

module Gate = struct
  type t = gate

  let to_string = function
    | AND g ->
        Printf.sprintf "%s AND %s -> %s\n" g.names.in1 g.names.in2 g.names.out
    | OR g ->
        Printf.sprintf "%s OR %s -> %s\n" g.names.in1 g.names.in2 g.names.out
    | XOR g ->
        Printf.sprintf "%s XOR %s -> %s\n" g.names.in1 g.names.in2 g.names.out

  let print g = Printf.printf "%s" (to_string g)

  let map f = function
    | AND g -> AND (f g)
    | OR g -> OR (f g)
    | XOR g -> XOR (f g)

  let map_name f = map (fun g -> { g with names = f g.names })
  let desc = function AND g -> g | OR g -> g | XOR g -> g

  let calc g =
    let gv = desc g in
    match g with
    | AND { values = { in1 = Some i1; in2 = Some i2; _ }; _ } ->
        AND { gv with values = { gv.values with out = Some (i1 land i2) } }
    | OR { values = { in1 = Some i1; in2 = Some i2; _ }; _ } ->
        OR { gv with values = { gv.values with out = Some (i1 lor i2) } }
    | XOR { values = { in1 = Some i1; in2 = Some i2; _ }; _ } ->
        XOR { gv with values = { gv.values with out = Some (i1 lxor i2) } }
    | g -> g

  let output g = (desc g).names.out
  let outputs gs = List.map output gs

  let compare_by_name a b =
    let tag = function AND _ -> -1 | OR _ -> 0 | XOR _ -> 1 in
    let compare_names a b =
      compare
        [ a.names.in1; a.names.in2; a.names.out ]
        [ b.names.in1; b.names.in2; b.names.out ]
    in
    match (a, b) with
    | AND a, AND b -> compare_names a b
    | OR a, OR b -> compare_names a b
    | XOR a, XOR b -> compare_names a b
    | a, b -> compare (tag a) (tag b)
end

let vertex_value = function
  | Variable v -> v.value
  | Gate g -> (Gate.desc g).values.out

let vertex_out = function
  | Variable v -> v.connected_to
  | Gate g -> [ (Gate.desc g).names.out ]

module Input = struct
  type t = { inputs : int VariableMap.t; gates : gate list }

  let parse_variable l =
    match String.split_on_char ':' l with
    | [ name; value ] -> (String.trim name, String.trim value |> int_of_string)
    | _ -> failwith "Invalid input"

  let mk_gate_var a b c =
    {
      names = { in1 = a; in2 = b; out = c };
      values = { in1 = None; in2 = None; out = None };
    }

  let prase_gate l =
    match String.split_on_char ' ' l with
    | [ in1; op; in2; _; out ] -> (
        match op with
        | "AND" -> AND (mk_gate_var in1 in2 out)
        | "OR" -> OR (mk_gate_var in1 in2 out)
        | "XOR" -> XOR (mk_gate_var in1 in2 out)
        | _ -> failwith "Invalid input")
    | _ -> failwith "Invalid input"

  let of_string s =
    let lines = String.split_on_char '\n' s in
    let vars, gates =
      match ListExt.split_on_empty lines with
      | [ a; b ] -> (a, b)
      | _ -> failwith "Invalid input"
    in
    {
      inputs = List.map parse_variable vars |> VariableMap.of_list;
      gates = List.map prase_gate gates;
    }

  let distill_all_variables input =
    let input_vars =
      VariableMap.bindings input.inputs
      |> List.map (fun (name, _) -> name)
      |> VariableSet.of_list
    in
    let inner_variables =
      List.map Gate.desc input.gates
      |> List.map (fun g -> [ g.names.in1; g.names.in2; g.names.out ])
      |> List.flatten |> VariableSet.of_list
    in
    VariableSet.diff inner_variables input_vars

  let switch_gate_output input a b =
    let new_gates =
      input.gates
      |> List.map (fun g ->
             match Gate.output g with
             | n when String.equal n a ->
                 Gate.map_name (fun n -> { n with out = b }) g
             | n when String.equal n b ->
                 Gate.map_name (fun n -> { n with out = a }) g
             | _ -> g)
    in
    { input with gates = new_gates }
end

module Circuit = struct
  type t = {
    bitness : int;
    circuit : vertex VariableMap.t;
    carry_bits : VariableSet.t;
  }

  let of_input (input : Input.t) =
    let fill_connection gate_in gate_name map =
      VariableMap.update gate_in
        (function
          | Some (Variable v) ->
              Some
                (Variable { v with connected_to = gate_name :: v.connected_to })
          | _ -> failwith "Invalid input")
        map
    in
    let carry_bits =
      input.gates
      |> List.filter_map (function OR g -> Some g.names.out | _ -> None)
      |> List.to_seq |> VariableSet.of_seq
    in
    let circuit =
      VariableMap.fold
        (fun name value acc ->
          VariableMap.add name
            (Variable { name; value = Some value; connected_to = [] })
            acc)
        input.inputs VariableMap.empty
    in
    let circuit =
      VariableSet.fold
        (fun name acc ->
          VariableMap.add name
            (Variable { name; value = None; connected_to = [] })
            acc)
        (Input.distill_all_variables input)
        circuit
    in
    let circuit =
      List.fold_left
        (fun acc gate ->
          let g = Gate.desc gate in
          let g_name = mk_gate_name g.names.out in
          let c = fill_connection g.names.in1 g_name acc in
          let c = fill_connection g.names.in2 g_name c in
          VariableMap.add g_name (Gate gate) c)
        circuit input.gates
    in
    { bitness = VariableMap.cardinal input.inputs / 2; circuit; carry_bits }

  let set_input prefix value circuit =
    let bits = circuit.bitness in
    let new_circuit =
      Seq.ints 0 |> Seq.take bits
      |> Seq.fold_left
           (fun (circuit, left) idx ->
             let name = Printf.sprintf "%s%02d" prefix idx in
             let new_circuit =
               VariableMap.update name
                 (function
                   | Some (Variable v) ->
                       Some (Variable { v with value = Some (left land 1) })
                   | _ -> failwith "Invalid input")
                 circuit
             in
             (new_circuit, left lsr 1))
           (circuit.circuit, value)
    in
    { circuit with circuit = fst new_circuit }

  let set_xy x y circuit = circuit |> set_input "x" x |> set_input "y" y

  let calculate circuit =
    let initial_execution_queue =
      VariableMap.bindings circuit.circuit
      |> List.filter_map (fun (n, vert) ->
             vertex_value vert
             |> Option.map (fun v ->
                    List.map (fun v_out -> (n, v_out, v)) (vertex_out vert)))
      |> List.flatten |> List.to_seq |> Queue.of_seq
    in
    let fill_gate v_from value g =
      if String.equal g.names.in1 v_from then
        { g with values = { g.values with in1 = Some value } }
      else { g with values = { g.values with in2 = Some value } }
    in
    let fill_vertex circuit (v_from, v_to, value) =
      match VariableMap.find v_to circuit with
      | Variable v ->
          VariableMap.add v_to (Variable { v with value = Some value }) circuit
      | Gate g ->
          VariableMap.add v_to
            (Gate (Gate.map (fill_gate v_from value) g |> Gate.calc))
            circuit
    in
    let rec aux circuit queue =
      match Queue.take_opt queue with
      | None -> circuit
      | Some action -> (
          let circuit = fill_vertex circuit action in
          let _, v_to, _ = action in
          let vert = VariableMap.find v_to circuit in
          match vert |> vertex_value with
          | Some v ->
              List.iter
                (fun v_out -> Queue.push (v_to, v_out, v) queue)
                (vertex_out vert);
              aux circuit queue
          | _ -> aux circuit queue)
    in
    aux circuit.circuit initial_execution_queue

  let calculate_xy x y circuit = set_xy x y circuit |> calculate

  let backtrack from circuit =
    let rec aux result from =
      let result = from :: result in
      match VariableMap.find from circuit.circuit with
      | Variable v ->
          if v.name.[0] = 'x' || v.name.[0] = 'y' then result
          else aux result (mk_gate_name v.name)
      | Gate g ->
          let gv = Gate.desc g in
          aux (aux result gv.names.in1) gv.names.in2
    in
    aux [] from

  let is_carry { carry_bits; _ } g = VariableSet.mem g carry_bits
  let is_not_carry circuit g = not (is_carry circuit g)
end

let task_input () =
  let data = read_data_as_string "data/day24.txt" in
  Input.of_string data

let interpret_as_binary prefix circuit =
  circuit |> VariableMap.bindings |> List.map snd
  |> List.filter_map (function
       | Variable v when String.starts_with ~prefix v.name ->
           Some (v.name, v.value |> Option.get)
       | _ -> None)
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.map snd

let interpret_as_number prefix circuit =
  interpret_as_binary prefix circuit
  |> List.mapi (fun i v -> v lsl i)
  |> List.fold_left ( lor ) 0

let get_layer (circuit : Circuit.t) (vars : string list) =
  List.map (fun v -> VariableMap.find v circuit.circuit) vars
  |> List.map (function
       | Variable v -> v
       | _ -> failwith "Inputs must be variables")
  |> List.map (fun v -> v.connected_to)
  |> List.flatten
  |> List.map (fun v -> VariableMap.find v circuit.circuit)
  |> List.map (function Gate g -> g | _ -> failwith "Invalid output")
  |> List.sort_uniq Gate.compare_by_name

let get_full_adder (circuit : Circuit.t) i =
  let x = Printf.sprintf "x%02d" i in
  let y = Printf.sprintf "y%02d" i in
  let layer1 = get_layer circuit [ x; y ] in
  let layer1_outs =
    Gate.outputs layer1 |> List.filter (Circuit.is_not_carry circuit)
  in
  let layer2 = get_layer circuit layer1_outs in
  let layer2_outs =
    Gate.outputs layer2 |> List.filter (Circuit.is_not_carry circuit)
  in
  let layer3 = get_layer circuit layer2_outs in
  (layer1, layer2, layer3)

let distill_full_adder_gates layer1 layer2 =
  match (layer1, layer2) with
  | [ and1; xor1 ], [ and2; or1; xor2 ] ->
      ( Gate.desc and1,
        Gate.desc xor1,
        Gate.desc and2,
        Gate.desc or1,
        Gate.desc xor2 )
  | _ -> failwith "Invalid full adder"

let is_correct_full_adder (circuit : Circuit.t) i =
  let z = Printf.sprintf "z%02d" i in
  let layer1, layer2, layer3 = get_full_adder circuit i in
  let is_structure_correct =
    match (layer1, layer2, layer3) with
    | [ AND _; XOR _ ], [ AND _; OR o1; XOR _ ], [ OR o2 ]
      when Gate.compare_by_name (OR o1) (OR o2) = 0 ->
        true
    | _ -> false
  in
  let is_output_correct =
    match layer2 with
    | [ _; _; XOR { names = { out; _ }; _ } ] when String.equal out z -> true
    | _ -> false
  in
  if (not is_structure_correct) || not is_output_correct then false
  else
    let and1, xor1, and2, or1, xor2 = distill_full_adder_gates layer1 layer2 in
    (and2.names.in1 = xor1.names.out || and2.names.in2 = xor1.names.out)
    && (xor2.names.in1 = xor1.names.out || xor2.names.in2 = xor1.names.out)
    && (or1.names.in1 = and1.names.out || or1.names.in2 = and1.names.out)
    && (or1.names.in1 = and2.names.out || or1.names.in2 = and2.names.out)

let print_full_adder (circuit : Circuit.t) i =
  let layer1, layer2, layer3 = get_full_adder circuit i in
  Printf.printf "Layer 1:\n";
  List.iter Gate.print layer1;
  Printf.printf "Layer 2:\n";
  List.iter Gate.print layer2;
  Printf.printf "Layer 3:\n";
  List.iter Gate.print layer3

let all_possible_outputs_of_adder (circuit : Circuit.t) i =
  let layer1, layer2, _ = get_full_adder circuit i in
  Gate.outputs layer1 @ Gate.outputs layer2 |> List.sort_uniq String.compare

let runPart1 input =
  Circuit.of_input input |> Circuit.calculate |> interpret_as_number "z"

let runPart2 input =
  let circuit = Circuit.of_input input in
  let invalid_adders =
    List.init circuit.bitness (fun i -> i)
    |> ListExt.drop 1
    |> List.filter (fun i -> not (is_correct_full_adder circuit i))
  in
  let pairs_to_swap =
    invalid_adders
    |> List.map (fun i ->
           let outputs = all_possible_outputs_of_adder circuit i in
           let pairs = ListExt.pairs outputs in
           pairs
           |> List.find (fun (a, b) ->
                  is_correct_full_adder
                    (Input.switch_gate_output input a b |> Circuit.of_input)
                    i))
    |> List.map (fun (a, b) -> [ a; b ])
  in
  pairs_to_swap |> List.flatten |> List.sort String.compare |> String.concat ","

let run () =
  let input = task_input () in
  let part1 = runPart1 input in
  let part2 = runPart2 input in
  Printf.printf "Day 23: %d %s\n" part1 part2
