open Expr
open Token

let rec print expr =
        match expr with
        | Binary (left, oper, right) ->
                parenthesize oper.lexeme [left; right]
        | Grouping expression ->
                parenthesize "group" [expression]
        | Literal value ->
                (match value with
                        | Lit_nil -> "nil"
                        | Lit_string s -> s 
                        | Lit_number n -> string_of_float n 
                        | Lit_bool b -> if b then "true" else "false")
        
        | Unary (oper, right) ->
                parenthesize oper.lexeme [right]
        | Variable tok ->
                tok.lexeme
        | Assign (tok, value) -> 
                parenthesize ("assign " ^ tok.lexeme) [value]

and parenthesize name exprs =
        let inner = List.map print exprs |> String.concat " " in
        "(" ^ name ^ " " ^ inner ^ ")"
