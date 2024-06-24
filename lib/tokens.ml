module StringMap = Map.Make (String)

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

let literal_option_as_string = function
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
  ^ literal_option_as_string token.literal
