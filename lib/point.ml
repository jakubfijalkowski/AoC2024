type t = int * int

let compare (x0, y0) (x1, y1) =
  match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c

let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
