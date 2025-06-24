open Interpreter
open Errors
open Stmt
open Token
open Environment
open Parser
open Scanner

let read_file filename =
  let file = open_in filename in
  let content =
    let rec read_all acc =
      try
        let line = input_line file in
        read_all (acc ^ line ^ "\n")
      with End_of_file -> acc
    in
    read_all ""
  in
  close_in file;
  content

let rec run source =
  let scanner = make source in
  let tokens = scan_tokens scanner in
  let statements = parse tokens in
  let env = Env.create () in
  List.iter (fun stmt ->
    match stmt with
    | Expression expr -> ignore (evaluate expr env)
    | Print expr ->
        let value = evaluate expr env in
        print_endline (stringify value)
    | Var (name, init) ->
        let value = match init with
          | Some expr -> evaluate expr env
          | None -> Lit_nil
        in
        Env.define env name.lexeme value
    | Block stmts ->
        let new_env = Env.create_enclosing env in
        List.iter (fun s -> ignore (evaluate_stmt s new_env)) stmts
  ) statements

and evaluate_stmt stmt env =
  match stmt with
  | Expression expr -> evaluate expr env
  | Print expr ->
      let value = evaluate expr env in
      print_endline (stringify value); Lit_nil
  | Var (name, init) ->
      let value = match init with
        | Some expr -> evaluate expr env
        | None -> Lit_nil
      in
      Env.define env name.lexeme value; Lit_nil
  | Block stmts ->
      let new_env = Env.create_enclosing env in
      List.iter (fun s -> ignore (evaluate_stmt s new_env)) stmts; Lit_nil

let () =
  try
    let source = read_file "file.my" in
    run source
  with
  | Sys_error msg -> prerr_endline ("Error: " ^ msg); exit 1
  | Parser.ParseError (tok, msg) -> prerr_endline ("Parse error at line " ^ string_of_int tok.line ^ ": " ^ msg); exit 1
  | RuntimeError (tok, msg) -> prerr_endline ("Runtime error at line " ^ string_of_int tok.line ^ ": " ^ msg); exit 1
