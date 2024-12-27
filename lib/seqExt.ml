let all_predicate f = Seq.fold_left (fun acc x -> acc && f x) true
let sum = Seq.fold_left ( + ) 0
let min1 = Seq.fold_left min max_int

let min_by f lst =
  let score, e =
    Seq.fold_left
      (fun (prev_min, prev_e) e ->
        if f e < prev_min then (f e, Some e) else (prev_min, prev_e))
      (max_int, None) lst
  in
  Option.map (fun e -> (score, e)) e

let pairwise s = Seq.zip s (Seq.drop 1 s)
