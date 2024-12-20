let all_predicate f = List.fold_left (fun acc x -> acc && f x) true

let split_on_empty l =
  let acc, last =
    List.fold_left
      (fun (acc, curr) -> function
        | "" -> (List.rev curr :: acc, [])
        | x -> (acc, x :: curr))
      ([], []) l
  in
  List.rev (List.rev last :: acc)

let sum = List.fold_left ( + ) 0

let min_opt = function
  | [] -> None
  | lst -> Some (List.fold_left min max_int lst)

let min1 = List.fold_left min max_int

let min_by f lst =
  let score, e =
    List.fold_left
      (fun (prev_min, prev_e) e ->
        if f e < prev_min then (f e, Some e) else (prev_min, prev_e))
      (max_int, None) lst
  in
  Option.map (fun e -> (score, e)) e

let take n l =
  let rec aux n acc = function
    | [] -> List.rev acc
    | x :: xs when n > 0 -> aux (n - 1) (x :: acc) xs
    | _ -> List.rev acc
  in
  aux n [] l

let any = List.fold_left ( || ) false
let any_predicate f = List.fold_left (fun acc e -> acc || f e) false
