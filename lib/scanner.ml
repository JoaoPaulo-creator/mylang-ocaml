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
  scanner.current <- scanner.current + 1;
  ch

let add_token ?(literal = None) scanner ttype =
  let text = String.sub scanner.source scanner.start (scanner.current - scanner.start) in
  let tok = { 
                token_type = ttype; 
                lexeme = text; 
                literal = literal; 
                line = scanner.line
        } in
  scanner.tokens := tok :: !(scanner.tokens)

let match_char scanner expected =
  if is_at_end scanner || scanner.source.[scanner.current] <> expected then false
  else (scanner.current <- scanner.current + 1; true)

let peek scanner =
  if is_at_end scanner then '\x00' else scanner.source.[scanner.current]

let peek_next scanner =
  if scanner.current + 1 >= String.length scanner.source then '\x00'
  else scanner.source.[scanner.current + 1]

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'
let is_alphanumeric c = is_alpha c || is_digit c

let rec scan_token scanner =
  let c = advance scanner in
  match c with
  | '(' -> add_token scanner LeftParen
  | ')' -> add_token scanner RightParen
  | '{' -> add_token scanner LeftBrace
  | '}' -> add_token scanner RightBrace
  | ',' -> add_token scanner Comma
  | '.' -> add_token scanner Dot
  | '-' -> add_token scanner Minus
  | '+' -> add_token scanner Plus
  | ';' -> add_token scanner Semicolon
  | '*' -> add_token scanner Star
  | '!' -> add_token scanner (if match_char scanner '=' then BangEqual else Bang)
  | '=' -> add_token scanner (if match_char scanner '=' then EqualEqual else Equal)
  | '<' -> add_token scanner (if match_char scanner '=' then LessEqual else Less)
  | '>' -> add_token scanner (if match_char scanner '=' then GreaterEqual else Greater)
  | '/' ->
      if match_char scanner '/' then
        while peek scanner <> '\n' && not (is_at_end scanner) do ignore (advance scanner) done
      else
        add_token scanner Slash
  | ' ' | '\r' | '\t' -> ()
  | '\n' -> scanner.line <- scanner.line + 1
  | '"' -> scan_string scanner
  | c when is_digit c -> scan_number scanner
  | c when is_alpha c -> scan_identifier scanner
  | _ -> Printf.printf "Unexpected character '%c' at line %d\n" c scanner.line

and scan_string scanner =
  while peek scanner <> '"' && not (is_at_end scanner) do
    if peek scanner = '\n' then scanner.line <- scanner.line + 1;
    ignore (advance scanner)
  done;

  if is_at_end scanner then
    Printf.printf "Unterminated string at line %d\n" scanner.line
  else (
                ignore (advance scanner);
    let value = String.sub scanner.source (scanner.start + 1) (scanner.current - scanner.start - 2) in
    add_token scanner String ~literal:(Some (Lit_string value))
  )

and scan_number scanner =
  while is_digit (peek scanner) do ignore (advance scanner) done;

  if peek scanner = '.' && is_digit (peek_next scanner) then (
    ignore (advance scanner);
    while is_digit (peek scanner) do ignore (advance scanner) done
  );

  let value = float_of_string (String.sub scanner.source scanner.start (scanner.current - scanner.start)) in
  add_token scanner Number ~literal:(Some (Lit_number value))

and scan_identifier scanner =
  while is_alphanumeric (peek scanner) do ignore (advance scanner) done;
  let text = String.sub scanner.source scanner.start (scanner.current - scanner.start) in
  let ttype = match Hashtbl.find_opt keywords text with Some t -> t | None -> Identifier in
  add_token scanner ttype

let scan_tokens scanner =
  while not (is_at_end scanner) do
    scanner.start <- scanner.current;
    scan_token scanner
  done;
  add_token scanner Eof;
  List.rev !(scanner.tokens)
