open Tokens
open Utils

module Scanner = struct
  type t = {
    source : string;
    mutable tokens : token list;
    mutable start : int;
    mutable current : int;
    mutable line : int;
  }

  let create source = { source; tokens = []; start = 0; current = 0; line = 1 }
  let is_at_end scanner = scanner.current >= String.length scanner.source

  let add_token scanner token_type =
    let lexeme =
      String.sub scanner.source scanner.start (scanner.current - scanner.start)
    in
    let t =
      { type_ = token_type; lexeme; literal = None; line = scanner.line }
    in
    scanner.tokens <- t :: scanner.tokens

  let add_token_with_literal scanner token_type literal =
    let lexeme =
      String.sub scanner.source scanner.start (scanner.current - scanner.start)
    in
    let t = { type_ = token_type; lexeme; literal; line = scanner.line } in
    scanner.tokens <- t :: scanner.tokens

  let advance scanner =
    let c = scanner.source.[scanner.current] in
    scanner.current <- scanner.current + 1;
    c

  (* Oddly, OCaml uses the NULL character here. *)
  let peek scanner =
    if is_at_end scanner then '\000' else scanner.source.[scanner.current]

  let match_token scanner expected =
    if is_at_end scanner || scanner.source.[scanner.current] != expected then
      false
    else (
      scanner.current <- scanner.current + 1;
      true)

  (* Consume characters in the source until newline. *)
  let rec consume_til_endline scanner =
    if peek scanner != '\n' && not (is_at_end scanner) then
      let _ = advance scanner in
      consume_til_endline scanner

  let consume_string scanner =
    let rec consume_string_helper () =
      if peek scanner <> '"' && not (is_at_end scanner) then (
        if peek scanner = '\n' then scanner.line <- scanner.line + 1;
        ignore (advance scanner);
        consume_string_helper ())
    in
    consume_string_helper ();

    if is_at_end scanner then error scanner.line "Unterminated string."
    else (
      ignore (advance scanner);

      let value =
        String.sub scanner.source (scanner.start + 1)
          (scanner.current - scanner.start - 2)
      in
      let lit = LitString value in
      add_token_with_literal scanner STRING (Some lit))

  let peek_next scanner =
    if scanner.current + 1 >= String.length scanner.source then '\000'
    else scanner.source.[scanner.current + 1]

  let consume_number scanner =
    let rec consume_number_helper () =
      if is_digit (peek scanner) then (
        ignore (advance scanner);
        consume_number_helper ())
    in
    consume_number_helper ();

    if peek scanner = '.' && is_digit (peek_next scanner) then (
      ignore (advance scanner);
      consume_number_helper ());

    let value =
      String.sub scanner.source scanner.start (scanner.current - scanner.start)
    in
    let lit = LitNumber (float_of_string value) in
    add_token_with_literal scanner NUMBER (Some lit)

  let consume_identifier scanner =
    let rec consume_identifier_helper () =
      if is_alphanumeric (peek scanner) = true then (
        ignore (advance scanner);
        consume_identifier_helper ())
    in
    consume_identifier_helper ();

    let text =
      String.sub scanner.source scanner.start (scanner.current - scanner.start)
    in
    match StringMap.find_opt text keywords with
    | Some keyword -> add_token scanner keyword
    | None -> add_token scanner IDENTIFIER

  let scan_token scanner =
    let c = advance scanner in
    match c with
    | '(' -> add_token scanner LEFT_PAREN
    | ')' -> add_token scanner RIGHT_PAREN
    | '{' -> add_token scanner LEFT_BRACE
    | '}' -> add_token scanner RIGHT_BRACE
    | ',' -> add_token scanner COMMA
    | '.' -> add_token scanner DOT
    | '-' -> add_token scanner MINUS
    | '+' -> add_token scanner PLUS
    | ';' -> add_token scanner SEMICOLON
    | '*' -> add_token scanner STAR
    | '!' ->
        if match_token scanner '=' then add_token scanner BANG_EQUAL
        else add_token scanner BANG
    | '=' ->
        if match_token scanner '=' then add_token scanner EQUAL_EQUAL
        else add_token scanner EQUAL
    | '<' ->
        if match_token scanner '=' then add_token scanner LESS_EQUAL
        else add_token scanner LESS
    | '>' ->
        if match_token scanner '=' then add_token scanner GREATER_EQUAL
        else add_token scanner GREATER
    | '/' ->
        if match_token scanner '/' then consume_til_endline scanner
        else add_token scanner SLASH
    | ' ' | '\r' | '\t' -> () (* Skip whitespace. *)
    | '\n' -> scanner.line <- scanner.line + 1
    | '"' -> consume_string scanner
    | '0' .. '9' -> consume_number scanner
    | 'a' .. 'z' | '_' | 'A' .. 'Z' -> consume_identifier scanner
    | _ -> error scanner.line "Unexpected character."

  let rec scan_tokens scanner =
    if not (is_at_end scanner) then (
      scanner.start <- scanner.current;
      scan_token scanner;
      scan_tokens scanner)
    else
      let t =
        { type_ = EOF; lexeme = ""; literal = None; line = scanner.line }
      in
      scanner.tokens <- t :: scanner.tokens;
      List.rev
        scanner.tokens (* Reverse the list to maintain the correct order *)
end
