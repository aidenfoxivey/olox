let error_log : string list ref = ref []

let report line where message =
  let msg = string_of_int line ^ " " ^ where ^ " " ^ message in
  error_log := msg :: !error_log

let error line message = report line "" message

let is_digit c = match c with '0' .. '9' -> true | _ -> false

let is_alphanumeric c =
  match c with 'a' .. 'z' | '_' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false
