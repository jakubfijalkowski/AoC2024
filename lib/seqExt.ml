let all_predicate f = Seq.fold_left (fun acc x -> acc && f x) true
let sum = Seq.fold_left ( + ) 0
let min1 = Seq.fold_left min max_int
