open Token

module Env = struct
        type t = {
                values : (string, literal) Hashtbl.t;
                enclosing: t option;
        }

        let create () = { values = Hashtbl.create 16; enclosing = None }
        let create_enclosing outer =
                { values = Hashtbl.create 16; enclosing = Some outer }

        let define env name value = 
                Hashtbl.replace env.values name value

        let rec get env token =
                match Hashtbl.find_opt env.values token.lexeme with
                | Some v -> v
                | None -> raise (Interpreter.RuntimeError (token, "Undefined variable: '" ^ token.lexeme ^ "'."))

        let rec assign env token value =
                if Hashtbl.mem env.values token.lexeme then
                        Hashtbl.replace env.values token.lexeme value

                else 
                        match env.enclosing  with
                        | Some parent -> assign parent token value
                        | None -> raise (Interpreter.RuntimeError (token, "Undefined variable: '" ^ token.lexeme ^ "'."))
end
