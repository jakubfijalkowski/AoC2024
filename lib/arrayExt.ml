open Array

let foldi_right f a x =
  let r = ref x in
  for i = length a - 1 downto 0 do
    r := f i (unsafe_get a i) !r
  done;
  !r

let foldi_left f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f i !r (unsafe_get a i)
  done;
  !r
