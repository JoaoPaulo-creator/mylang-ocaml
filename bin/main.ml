open Interpreter
open Errors
(* open Stmt *)
open Token
open Environment
open Parser
open Scanner

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let run source =
  let scanner = make source in
  let tokens = scan_tokens scanner in
  let stmts = parse tokens in
  let env = Env.create () in
  interpret stmts env

let () =
  try
    let source =
      if Array.length Sys.argv > 1 then
        read_file Sys.argv.(1)
      else
        (print_string "Enter code, then Ctrl+D:\n"; read_file "/dev/stdin")
    in
    run source
  with
  | Sys_error msg ->
      prerr_endline ("Error: " ^ msg); exit 1
  | Parser.ParseError (tok, msg) ->
      prerr_endline ("Parse error at line " ^ string_of_int tok.line ^ ": " ^ msg);
      exit 1
  | RuntimeError (tok, msg) ->
      prerr_endline ("Runtime error at line " ^ string_of_int tok.line ^ ": " ^ msg);
      exit 1
