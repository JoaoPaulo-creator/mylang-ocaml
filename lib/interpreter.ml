open Expr
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
      (match op.token_type with
       | Minus ->
           check_number_operand op right_val;
           (match right_val with Lit_number n -> Lit_number (-.n) | _ -> assert false)
       | Bang -> Lit_bool (not (is_truthy right_val))
       | _ -> raise (RuntimeError (op, "Unknown unary operator")))
  | Binary (left, op, right) ->
      let l = evaluate left env in
      let r = evaluate right env in
      (match op.token_type, l, r with
       | Plus, Lit_number a, Lit_number b -> Lit_number (a +. b)
       | Plus, Lit_string a, Lit_string b -> Lit_string (a ^ b)
       | Minus, _, _ -> check_number_operands op l r; Lit_number (num l -. num r)
       | Star, _, _ -> check_number_operands op l r; Lit_number (num l *. num r)
       | Slash, _, _ -> check_number_operands op l r; Lit_number (num l /. num r)
       | Greater, _, _ -> check_number_operands op l r; Lit_bool (num l > num r)
       | GreaterEqual, _, _ -> check_number_operands op l r; Lit_bool (num l >= num r)
       | Less, _, _ -> check_number_operands op l r; Lit_bool (num l < num r)
       | LessEqual, _, _ -> check_number_operands op l r; Lit_bool (num l <= num r)
       | BangEqual, _, _ -> Lit_bool (not (is_equal l r))
       | EqualEqual, _, _ -> Lit_bool (is_equal l r)
       | _ -> raise (RuntimeError (op, "Unknown binary operator")))
  | Variable name ->
      Env.get env name
  | Assign (name, value_expr) ->
      let value = evaluate value_expr env in
      Env.assign env name value;
      value

and num = function
  | Lit_number n -> n
  | _ -> failwith "Expected number"
