open OUnit2
open Olox.Scanner

(* Inspired by bisect_ppx, which is licensed under MIT. *)
let workspace_root =
  let rec loop path =
    let parent = Filename.dirname path in
    let parent_result =
      if parent <> path && not (Filename.is_relative parent) then loop parent
      else None
    in
    match parent_result with
    | Some _ -> parent_result
    | None ->
        if Sys.file_exists (Filename.concat path "dune-project") then Some path
        else None
  in
  loop (Sys.getcwd ())

let examples_dir =
  let rt = match workspace_root with Some root -> root | None -> "." in
  Filename.concat rt "examples"

let rec get_files path =
  if Sys.is_directory path then
    Sys.readdir path |> Array.to_list
    |> List.map (fun file -> get_files (Filename.concat path file))
    |> List.flatten
  else [ path ]

let get_test_files dir =
  get_files dir
  |> List.filter (fun f -> Filename.check_suffix f ".lox")
  |> List.filter (fun f -> not (Filename.check_suffix f "unterminated.lox"))
  |> List.filter (fun f ->
         not (Filename.check_suffix f "unexpected_character.lox"))

let make_test_case file_path _ =
  let input = In_channel.with_open_bin file_path In_channel.input_all in
  try
    let scanner = Scanner.create input in
    let _ = Scanner.scan_tokens scanner in
    () (* If no exception is raised, test passes *)
  with e ->
    let error_msg = Printexc.to_string e in
    assert_failure
      (Printf.sprintf "File %s raised exception: %s" file_path error_msg)

let starts_with l starts =
  match l with hd :: _ when hd = starts -> true | _ -> false

let unterminated _ =
  let rt = match workspace_root with Some root -> root | None -> "." in
  let file_path = Filename.concat rt "examples/string/unterminated.lox" in
  let input = In_channel.with_open_bin file_path In_channel.input_all in
  try
    let scanner = Scanner.create input in
    let _ = Scanner.scan_tokens scanner in
    let log = !Olox.Utils.error_log in
    let starts = "2  Unterminated string." in
    let result = starts_with log starts in
    if not result then
      Printf.printf "\nError log did not start with '%s'.\nLog: '%s'\n" starts
        (String.concat "\n" log);
    assert_bool "Doesn't match" result
  with e ->
    let error_msg = Printexc.to_string e in
    assert_failure
      (Printf.sprintf "File %s raised exception: %s" file_path error_msg)

let unexpected_character _ =
  let rt = match workspace_root with Some root -> root | None -> "." in
  let file_path =
    Filename.concat rt "examples/unexpected_character.lox"
  in
  let input = In_channel.with_open_bin file_path In_channel.input_all in
  try
    let scanner = Scanner.create input in
    let _ = Scanner.scan_tokens scanner in
    let log = !Olox.Utils.error_log in
    let starts = "3  Unexpected character." in
    let result = starts_with log starts in
    if not result then
      Printf.printf "\nError log did not start with '%s'.\nLog: '%s'\n" starts
        (String.concat "\n" log);
    assert_bool "Doesn't match" result
  with e ->
    let error_msg = Printexc.to_string e in
    assert_failure
      (Printf.sprintf "File %s raised exception: %s" file_path error_msg)

let suite =
  "Scanner Tests"
  >::: (get_test_files examples_dir
       |> List.map (fun file ->
              let test_name = Filename.basename file in
              test_name >:: make_test_case file))
       @ [
           "unterminated" >:: unterminated;
           "unexpected_character" >:: unexpected_character;
         ]
;;

run_test_tt_main suite
