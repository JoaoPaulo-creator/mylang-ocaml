type token_type = 
        | LeftParen
        | RightParen
        | LeftBrace
        | RightBrace
        | LeftBracket
        | RightBracket
        | Comma
        | Dot
        | Minus
        | Plus
        | Semicolon
        | Slash
        | Star
        | Bang
        | BangEqual
        | Equal
        | EqualEqual
        | Greater
        | GreaterEqual
        | Less
        | LessEqual
        | Identifier
        | String
        | Number
        | And
        | Class
        | Else
        | False
        | Fun
        | For
        | If
        | Nil
        | Or
        | Print
        | Return
        | Super
        | This 
        | True 
        | Var 
        | While 
        | Assign
        | Eof


type literal =
        | Lit_string of string
        | Lit_number of float
        | Lit_bool of bool
        | Lit_nil

type token = {
        token_type: token_type;
        lexeme: string;
        literal: literal option;
        line: int; 
}

