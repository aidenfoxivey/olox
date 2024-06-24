open Olox.Scanner
open Olox.Tokens

let had_error = ref false

let run source =
  let scanner = Scanner.create source in
  let tokens = Scanner.scan_tokens scanner in

  (* Print each token *)
  List.iter
    (fun token ->
      Printf.printf "%s \"%s\" on line %d"
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
