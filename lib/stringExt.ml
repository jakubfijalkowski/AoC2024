let pairwise s =
  let rec aux i =
    if i >= String.length s then [] else (s.[i - 1], s.[i]) :: aux (i + 1)
  in
  aux 1
