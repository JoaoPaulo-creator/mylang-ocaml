open Expr
open Token
open Ast_printer
open Interpreter
open Environment

let () =
  let expr =
    Binary (
      Unary (
        { token_type = Minus; lexeme = "-"; literal = None; line = 1 },
        Literal (lit_number 123.0)
      ),
      { token_type = Star; lexeme = "*"; literal = None; line = 1 },
      Grouping (Literal (lit_number 45.67))
    )
  in

  let env = Env.create () in
  let result = evaluate expr env in
  Printf.printf "Ast: %s\n" (Ast_printer.print expr);
  Printf.printf "Evaluated: %s\n" (stringify result)
