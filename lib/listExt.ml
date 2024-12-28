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
let max1 = List.fold_left max min_int

let min_by f lst =
  let score, e =
    List.fold_left
      (fun (prev_min, prev_e) e ->
        if f e < prev_min then (f e, Some e) else (prev_min, prev_e))
      (max_int, None) lst
  in
  Option.map (fun e -> (score, e)) e

let max_by f lst =
  let score, e =
    List.fold_left
      (fun (prev_max, prev_e) e ->
        if f e > prev_max then (f e, Some e) else (prev_max, prev_e))
      (min_int, None) lst
  in
  Option.map (fun e -> (score, e)) e

let take n l =
  let rec aux n acc = function
    | [] -> List.rev acc
    | x :: xs when n > 0 -> aux (n - 1) (x :: acc) xs
    | _ -> List.rev acc
  in
  aux n [] l

let rec drop n lst =
  match (n, lst) with
  | 0, _ -> lst
  | _, [] -> []
  | n, _ :: xs -> drop (n - 1) xs

let any = List.fold_left ( || ) false
let any_predicate f = List.fold_left (fun acc e -> acc || f e) false

let rec zip a b =
  match (a, b) with x :: xs, y :: ys -> (x, y) :: zip xs ys | _ -> []

let pairwise s = zip s (drop 1 s)

let rec pairs = function
  | [] -> []
  | x :: xs -> List.map (fun y -> (x, y)) xs @ pairs xs
