open Either

let all_predicate f = List.fold_left (fun acc x -> acc && f x) true

let split_on_empty l =
  let idx = List.find_index (fun x -> x = "") l |> Option.get in
  let filtered = List.filter (fun x -> x <> "") l in
  let mapped =
    List.mapi (fun i x -> if i < idx then Left x else Right x) filtered
  in
  List.partition_map (fun x -> x) mapped
