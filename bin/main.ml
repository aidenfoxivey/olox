let had_error = ref false

module StringMap = Map.Make (String)

let report line where message =
  let msg = string_of_int line ^ " " ^ where ^ " " ^ message ^ "\n" in
  prerr_string msg

let error line message = report line "" message

type tokentype =
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | IDENTIFIER
  | STRING
  | NUMBER
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF

let tokentype_as_string = function
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | IDENTIFIER -> "IDENTIFIER"
  | STRING -> "STRING"
  | NUMBER -> "NUMBER"
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
  | FOR -> "FOR"
  | IF -> "IF"
  | NIL -> "NIL"
  | OR -> "OR"
  | PRINT -> "PRINT"
  | RETURN -> "RETURN"
  | SUPER -> "SUPER"
  | THIS -> "THIS"
  | TRUE -> "TRUE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"
  | EOF -> "EOF"

type literal =
  | LitString of string
  | LitNumber of float
  | LitBool of bool
  | LitNil

let literal_as_string literal =
  match literal with
  | LitString x -> x
  | LitNumber x -> string_of_float x
  | LitBool x -> string_of_bool x
  | LitNil -> "NIL"

let lit_option_as_string = function
  | Some value -> literal_as_string value
  | None -> "None"

type token = {
  type_ : tokentype;
  lexeme : string;
  literal : literal option;
  line : int;
}

let new_token type_ lexeme literal line = { type_; lexeme; literal; line }

let token_as_string token =
  string_of_int token.line ^ " " ^ token.lexeme ^ " "
  ^ lit_option_as_string token.literal

let keywords =
  StringMap.of_seq
  @@ List.to_seq
       [
         ("and", AND);
         ("class", CLASS);
         ("else", ELSE);
         ("false", FALSE);
         ("for", FOR);
         ("fun", FUN);
         ("if", IF);
         ("nil", NIL);
         ("or", OR);
         ("print", PRINT);
         ("return", RETURN);
         ("super", SUPER);
         ("this", THIS);
         ("true", TRUE);
         ("var", VAR);
         ("while", WHILE);
       ]

type scanner = {
  source : string;
  mutable tokens : token list;
  mutable start : int;
  mutable current : int;
  mutable line : int;
}

let new_scanner source tokens =
  { source; tokens; start = 0; current = 0; line = 1 }

let is_at_end scanner = scanner.current >= String.length scanner.source

let add_token scanner token_type =
  let lexeme =
    String.sub scanner.source scanner.start (scanner.current - scanner.start)
  in
  let t = { type_ = token_type; lexeme; literal = None; line = scanner.line } in
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

let is_digit c = match c with '0' .. '9' -> true | _ -> false

let is_alphanumeric c =
  match c with 'a' .. 'z' | '_' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false

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
    let t = { type_ = EOF; lexeme = ""; literal = None; line = scanner.line } in
    scanner.tokens <- t :: scanner.tokens;
    List.rev scanner.tokens (* Reverse the list to maintain the correct order *)

let run source =
  let scanner = new_scanner source [] in
  let tokens = scan_tokens scanner in

  (* Print each token *)
  List.iter
    (fun token ->
      Printf.printf "Type: %s, Lexeme: %s, Line: %d"
        (tokentype_as_string token.type_)
        token.lexeme token.line;
      (match token.literal with
      | Some lit -> Printf.printf ", Literal: %s" (literal_as_string lit)
      | None -> ());
      Printf.printf "\n")
    tokens

let run_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  try
    let content = really_input_string ic len in
    close_in ic;
    run content;
    if !had_error then exit 65
  with e ->
    close_in_noerr ic;
    raise e

let sigint_handler _ =
  print_endline "\nUse 'quit' or 'exit' to end the session, or press Ctrl+D.";
  flush stdout

let rec run_prompt () =
  print_string "> ";
  flush stdout;
  try
    let input = read_line () in
    if input == "quit" || input == "exit" then exit 0
    else (
      run input;
      had_error := false;
      run_prompt ())
  with End_of_file -> exit 0

let () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle sigint_handler);
  match Array.length Sys.argv with
  | 1 -> run_prompt ()
  | 2 -> run_file Sys.argv.(1)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64
