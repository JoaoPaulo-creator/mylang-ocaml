open Token
open Expr
open Stmt

exception ParseError of token * string

type parser_state = {
        tokens : token array;
        mutable current : int;
}

let make_parser tokens =
        { tokens = Array.of_list tokens; current = 0 }

let peek state = state.tokens.(state.current)
let previous state = state.tokens.(state.current - 1)
let is_at_end state = (peek state).token_type = Eof

let advance state =
        if not (is_at_end state) then state.current <- state.current + 1;
        previous state

let check state ttype =
        if is_at_end state then false else (peek state).token_type = ttype

let consume state ttype message =
        if check state ttype then advance state
        else raise (ParseError (peek state, message))

let match_token state types =
        List.exists (check state) types && (ignore (advance state); true)

(* Expression parsing *)

let rec expression state = assignment state

and assignment state =
        let expr = equality state in
        if match_token state [Equal] then
                let equals = previous state in
                let value = assignment state in
                match expr with
                | Variable name -> Assign (name, value)
                | _ -> raise (ParseError (equals, "Invalid assignment target."))
        else expr

and equality state =
        let rec loop expr =
                if match_token state [BangEqual; EqualEqual] then
                        let op = previous state in
                        let right = comparison state in
                        loop (Binary (expr, op, right))
                else expr
        in
        loop (comparison state)

and comparison state =
        let rec loop expr =
                if match_token state [Greater; GreaterEqual; Less; LessEqual] then
                        let op = previous state in
                        let right = term state in
                        loop (Binary (expr, op, right))
                else expr
        in
        loop (term state)

and term state =
        let rec loop expr =
                if match_token state [Plus; Minus] then
                        let op = previous state in
                        let right = factor state in
                        loop (Binary (expr, op, right))
                else expr
        in
        loop (factor state)

and factor state =
        let rec loop expr =
                if match_token state [Star; Slash] then
                        let op = previous state in
                        let right = unary state in
                        loop (Binary (expr, op, right))
                else expr
        in
        loop (unary state)

and unary state =
        if match_token state [Bang; Minus] then
                let op = previous state in
                let right = unary state in
                Unary (op, right)
        else primary state

and primary state =
        if match_token state [False] then Literal (Lit_bool false)
        else if match_token state [True] then Literal (Lit_bool true)
        else if match_token state [Nil] then Literal Lit_nil
        else if match_token state [Number; String] then
                let lit = Option.get (previous state).literal in
                Literal lit
        else if match_token state [Identifier] then Variable (previous state)
        else if match_token state [LeftParen] then
                let expr = expression state in
                ignore (consume state RightParen "Expect ')' after expression.");
                Grouping expr
        else
                raise (ParseError (peek state, "Expected expression."))

(* Statement parsing *)

let rec declaration state =
        try
                if match_token state [Var] then var_declaration state
                else statement state
        with ParseError (tok, msg) ->
                synchronize state;
                Printf.eprintf "Parse error at line %d: %s\n" tok.line msg;
                Expression (Literal Lit_nil)

and var_declaration state =
        let name = consume state Identifier "Expected variable name." in
        let init =
                if match_token state [Equal] then Some (expression state) else None
        in
        ignore (consume state Semicolon "Expected ';' after variable declaration.");
        Var (name, init)

and statement state =
        if match_token state [Print] then print_statement state
        else if match_token state [If] then if_statement state
        else if match_token state [LeftBrace] then Block (block state)
        else expression_statement state

and print_statement state =
        let value = expression state in
        ignore (consume state Semicolon "Expected ';' after value.");
        Print value

and expression_statement state =
        let expr = expression state in
        ignore (consume state Semicolon "Expected ';' after expression.");
        Expression expr

and block state =
        let rec loop acc =
                if check state RightBrace || is_at_end state then List.rev acc
                else loop (declaration state :: acc)
        in
        let result = loop [] in
        ignore (consume state RightBrace "Expected '}' after block.");
        result

and if_statement state  =
        ignore ( consume state LeftParen "Expected '(' after 'if'");
        let cond = expression state in 
        ignore ( consume state LeftParen "Expected ')' after 'if'");
        let then_branch = statement state in
        let else_branch = 
                if match_token state [Else] then
                        Some (statement state)
                else
                        None
        in
        If (cond, then_branch, else_branch)

and synchronize state =
        advance state |> ignore;
        let rec loop () =
                if is_at_end state then () else
                        let prev = previous state in
                        let next = peek state in
                        match prev.token_type, next.token_type with
                        | _, (Class | Fun | Var | For | If | While | Print | Return) -> ()
                        | _ -> ignore (advance state); loop ()
        in
        loop ()

let parse tokens =
        let state = make_parser tokens in
        let rec collect acc =
                if is_at_end state then List.rev acc
                else collect (declaration state :: acc)
        in
        collect []
