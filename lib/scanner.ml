open Token

type scanner = {
        source : string;
        mutable start : int;
        mutable current : int;
        mutable line : int;
        tokens : Token.token list ref;
}

let keywords = Hashtbl.create 20

let () =
        Hashtbl.add keywords "and" And;
        Hashtbl.add keywords "class" Class;
        Hashtbl.add keywords "else" Else;
        Hashtbl.add keywords "false" False;
        Hashtbl.add keywords "for" For;
        Hashtbl.add keywords "fun" Fun;
        Hashtbl.add keywords "if" If;
        Hashtbl.add keywords "nil" Nil;
        Hashtbl.add keywords "or" Or;
        Hashtbl.add keywords "print" Print;
        Hashtbl.add keywords "return" Return;
        Hashtbl.add keywords "super" Super;
        Hashtbl.add keywords "this" This;
        Hashtbl.add keywords "true" True;
        Hashtbl.add keywords "var" Var;
        Hashtbl.add keywords "while" While


let make source =
        { source; start = 0; current = 0; line = 1; tokens = ref [] }

let is_at_end scanner = scanner.current >= String.length scanner.source
let advance scanner =
        let ch = scanner.source.[scanner.current] in
        scanner.current <- scanner.current +1;
        ch

let add_token scanner ttype ?(Literal=None) =
        let text = String.sub scanner.source scanner.start (scanner.current - scanner.start) in
        let tok = { token_type = ttype; lexeme = text; literal; line = scanner.line } in
        scanner.tokens := tok :: !(scanner.tokens)

let match_char scanner expected = 
        if is_at_end scanner || scanner.source.[scanner.current] <> expexted then false
        else (scanner.current <- scanner.current+1; true)

let peek scanner =
        if is_at_end scanner then '\x00' else scanner.source.[scanner.current]

let peek_next scanner =
        if scanner.current + 1 >= String.length scanner.source then '\x00'
        else scanner.source.[scanner.current + 1]

let is_digit c = c >= '0' && c <= '9' 
let is_alpha c = 'a' <= c && c <= 'z'  || 'A' <= c || c <= 'Z' || c = '_'
let is_alpha c = is_digit c || is_alpha c

