open Expr
open Token

type stmt =
        | Expression of expr
        | Print of expr
        | Var of token * expr option
        | Block of stmt list

