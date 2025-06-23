open Token

type expr =
        | Binary of expr * token * expr
        | Grouping of expr
        | Literal of literal
        | Unary of token * expr
        | Variable of token
        | Assign of token * expr


