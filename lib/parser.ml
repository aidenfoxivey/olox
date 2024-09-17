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

  exception LoxError of string

  let consume scanner tokentype msg =
    if check scanner tokentype then advance scanner else raise (LoxError msg)

  let rec comparison parser =
    let expr = ref (term parser) in
    let rec aux () =
      if match_tokens parser [ GREATER; GREATER_EQUAL; LESS; LESS_EQUAL ] then (
        let op = previous parser in
        let right = term parser in
        expr := Expr.Binary (!expr, op, right);
        aux ())
    in
    aux ();
    !expr

  and equality parser =
    let expr = ref (comparison parser) in
    let rec aux () =
      if match_tokens parser [ EQUAL_EQUAL; EQUAL ] then (
        let op = previous parser in
        let right = comparison parser in
        expr := Expr.Binary (!expr, op, right);
        aux ())
    in
    aux ();
    !expr

  and term parser =
    let expr = ref (factor parser) in
    let rec aux () =
      if match_tokens parser [ PLUS; MINUS ] then (
        let op = previous parser in
        let right = factor parser in
        expr := Expr.Binary (!expr, op, right);
        aux ())
    in
    aux ();
    !expr

  and factor parser =
    let expr = ref (unary parser) in
    let rec aux () =
      if match_tokens parser [ SLASH; STAR ] then (
        let op = previous parser in
        let right = unary parser in
        expr := Expr.Binary (!expr, op, right);
        aux ())
    in
    aux ();
    !expr

  and unary parser =
    if match_tokens parser [ BANG; MINUS ] then
      let op = previous parser in
      let right = unary parser in
      Expr.Unary (op, right)
    else primary parser

  and primary parser =
    if match_tokens parser [ FALSE ] then Expr.Literal (LitBool false)
    else if match_tokens parser [ TRUE ] then Expr.Literal (LitBool true)
    else if match_tokens parser [ NIL ] then Expr.Literal LitNil
    else if match_tokens parser [ NUMBER; STRING ] then
      let l = (previous parser).literal in
      match l with
      | Some x -> Expr.Literal x
      | None -> raise (Failure "Number or string should have literal value.")
    else if match_tokens parser [ LEFT_PAREN ] then (
      let expr = expression parser in
      ignore (consume parser RIGHT_PAREN "Expect ')' after expression.");
      Expr.Grouping expr)
    else 
      raise (Failure "Unexpected token in primary expression.")

  and expression parser = equality parser

  let parse parser =
    expression parser
end
