open Expr
open Stmt
open Token
open Environment
open Errors

let is_truthy = function
  | Lit_nil -> false
  | Lit_bool b -> b
  | _ -> true

let is_equal l1 l2 =
  match l1, l2 with
  | Lit_nil, Lit_nil -> true
  | Lit_nil, _ | _, Lit_nil -> false
  | Lit_number a, Lit_number b -> a = b
  | Lit_string a, Lit_string b -> a = b
  | Lit_bool a, Lit_bool b -> a = b
  | _ -> false

let stringify = function
  | Lit_nil -> "nil"
  | Lit_bool b -> if b then "true" else "false"
  | Lit_string s -> s
  | Lit_number n -> string_of_float n

let check_number_operand token = function
  | Lit_number _ -> ()
  | _ -> raise (RuntimeError (token, "Operand must be a number."))

let check_number_operands token left right =
  match left, right with
  | Lit_number _, Lit_number _ -> ()
  | _ -> raise (RuntimeError (token, "Operands must be numbers."))

let rec evaluate expr env =
  match expr with
  | Literal v -> v
  | Grouping e -> evaluate e env
  | Unary (op, right) ->
      let right_val = evaluate right env in
      (match op.token_type, right_val with
       | Minus, Lit_number n -> Lit_number (-. n)
       | Bang, _ -> Lit_bool (not (is_truthy right_val))
       | _ -> raise (RuntimeError (op, "Unknown unary operator")))
  | Binary (left, op, right) ->
      let l = evaluate left env in
      let r = evaluate right env in
      (match op.token_type, l, r with
       | Plus, Lit_number a, Lit_number b -> Lit_number (a +. b)
       | Plus, Lit_string a, Lit_string b -> Lit_string (a ^ b)
       | Minus, Lit_number a, Lit_number b -> Lit_number (a -. b)
       | Star, Lit_number a, Lit_number b -> Lit_number (a *. b)
       | Slash, Lit_number a, Lit_number b -> Lit_number (a /. b)
       | Greater, Lit_number a, Lit_number b -> Lit_bool (a > b)
       | GreaterEqual, Lit_number a, Lit_number b -> Lit_bool (a >= b)
       | Less, Lit_number a, Lit_number b -> Lit_bool (a < b)
       | LessEqual, Lit_number a, Lit_number b -> Lit_bool (a <= b)
       | BangEqual, _, _ -> Lit_bool (not (is_equal l r))
       | EqualEqual, _, _ -> Lit_bool (is_equal l r)
       | _ -> raise (RuntimeError (op, "Operands not a same type")))
  | Variable name ->
      Env.get env name
  | Assign (name, value_expr) ->
      let value = evaluate value_expr env in
      Env.assign env name value;
      value

and execute_stmt stmt env =
  match stmt with
  | Expression e ->
      let _ = evaluate e env in
      ()
  | Print e ->
      let v = evaluate e env in
      print_endline (stringify v)
  | Var (name, initializer_opt) ->
      let value =
        match initializer_opt with
        | Some init -> evaluate init env
        | None -> Lit_nil
      in
      Env.define env name.lexeme value
  | Block stmts ->
      let new_env = Env.create_enclosing env in
      List.iter (fun s -> execute_stmt s new_env) stmts
  | If (condition, then_branch, else_branch_opt) ->
      if is_truthy (evaluate condition env) then
        execute_stmt then_branch env
      else
        (match else_branch_opt with
         | Some else_branch -> execute_stmt else_branch env
         | None -> ())

(** Entry point: execute all statements in order *)
let interpret stmts env =
  List.iter (fun stmt -> execute_stmt stmt env) stmts
