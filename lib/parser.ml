open Tokens
open Expr

module Parser = struct
  type t = { tokens : token list; mutable current : int }

  let create tokens = { tokens; current = 0 }
  let peek parser = List.nth parser.tokens parser.current
  let previous parser = List.nth parser.tokens (parser.current - 1)
  let is_at_end parser = (peek parser).type_ = EOF

  let check parser t =
    if is_at_end parser then false else (peek parser).type_ = t

  let advance parser =
    if is_at_end parser then parser.current <- parser.current + 1;
    previous parser

  let equality () =
    let rec aux parser =
      if match_tokens 
    let expr = comparison () in
    expr

  let match_tokens parser tokens =
    let rec aux = function
      | [] -> false
      | t :: ts ->
          if check parser t then (
            ignore (advance parser);
            true)
          else aux ts
    in
    aux tokens
end
