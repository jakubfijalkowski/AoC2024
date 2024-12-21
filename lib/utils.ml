let read_data_as_string filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let buf = Bytes.create len in
  really_input chan buf 0 len;
  close_in chan;
  String.trim (Bytes.to_string buf)

let rec ipow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = ipow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a
