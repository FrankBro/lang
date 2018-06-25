(* display.ml *)
open Lexer
open Parser
open Lang

let string_of_value (v: value) : string =
    match v with
    | VLit (LInt i) -> string_of_int i
    | VLit (LBool b) -> string_of_bool b
    | VFun (e1, e2) -> ("(fun " ^ (Display.string_of_exp e1) ^ " " (Display.string_of_exp e2) ^ ")")
    | VFix (e1, e2, e3) -> "<fix>"

let string_of_op (op: bin_op) : string =
    match op with
    | OAdd -> "+"
    | OSubtract -> "-"
    | OMultiply -> "*"
    | ODivide -> "/"
    | OLessThanEq -> "<="
    | OGreaterThanEq -> ">="
    | OLessThan -> "<"
    | OGreaterThan -> ">"
    | OEquals -> "="

let string_of_token (t: token) : string =
    match t with
    | LPAREN -> "("
    | RPAREN -> ")"
    | PLUS   -> "+"
    | MINUS  -> "-"
    | TIMES  -> "*"
    | DIVIDE -> "/"
    | LEQ        -> "<="
    | LSTHN  -> "<"
    | GTTHN  -> ">"
    | GEQ        -> ">="
    | IF         -> "if"
    | THEN   -> ""
    | ELSE   -> ""
    | LET        -> "let"
    | EQUALS -> "="
    | IN         -> "in"
    | FUN        -> "fun"
    | ARROW  -> "->"
    | FIX        -> "fix"
    | INT i  -> string_of_int i
    | BOOL b -> string_of_bool b
    | NAME s    -> s
    | EOF        -> ""

(* Copied from Matthias Braun at
 * https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings *)
let explode str =
    let rec explode_inner cur_index chars =
        if (cur_index < String.length str) then
            let new_char = str.[cur_index] in
            explode_inner (cur_index + 1) (chars @ [new_char])
        else chars in
            explode_inner 0 []

let rec implode chars =
    match chars with
    | [] -> ""
    | h :: t -> (Char.escaped h) ^ (implode t)
(* End copied portion *)

let string_of_token_list (tlist: token list) : string =
    let rec process_token (so_far: string) (remaining: token list) : string =
        match remaining with
        | [] -> so_far ^ ""
        | t :: rest -> process_token (so_far ^ (string_of_token t ) ^ ", ") rest
        in
        match explode (process_token "[" tlist) with
        | '[' :: [] -> "[]"
        | tstring -> (String.sub (implode tstring) 0
            ((String.length (implode tstring)) - 2)) ^ "]"

let string_of_bin_op (op: bin_op) =
    match op with
    | OAdd -> "+"
    | OSubtract -> "-"
    | OMultiply -> "*"
    | ODivide -> "/"
    | OLessThan -> "<"
    | OLessThanEq -> "<="
    | OGreaterThan -> ">"
    | OGreaterThanEq -> ">="
    | OEquals -> "="

let rec string_of_exp (e: exp) : string =
    match e with
    | EVal v -> string_of_value v
    | EBinOp (op, exp1, exp2) ->
        "(" ^ (string_of_bin_op op) ^ " " ^ (string_of_exp exp1) ^ " "
            ^ (string_of_exp exp2) ^ ")"
    | EIf (exp1, exp2, exp3) ->
        "(if (" ^ (string_of_exp exp1) ^ ") (" ^ (string_of_exp exp2) ^ ") ("
            ^ (string_of_exp exp3) ^ "))"
    | EVar s -> s
    | ELet (expr1, expr2, expr3) -> ("(let " ^ (string_of_exp expr1) ^ " = "
        ^ (string_of_exp expr2) ^ " in (" ^ (string_of_exp expr3) ^ "))")
    | EFunCall (expr1, expr2) -> ((string_of_exp expr1) ^ " " ^ (string_of_exp expr2))
