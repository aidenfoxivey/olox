let report line where message =
  let msg = string_of_int line ^ " " ^ where ^ " " ^ message ^ "\n" in
  prerr_string msg

let error line message = report line "" message
let is_digit c = match c with '0' .. '9' -> true | _ -> false

let is_alphanumeric c =
  match c with 'a' .. 'z' | '_' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false
