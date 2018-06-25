{
open Lexing
open Parser

exception Lexer_error of string

let symbols : (string * Parser.token) list =
  [ ("(", LPAREN)
  ; (")", RPAREN)
  ; ("+", PLUS)
  ; ("-", MINUS)
  ; ("*", TIMES)
  ; ("/", DIVIDE)
  ; ("<=", LEQ)
  ; (">=", GEQ)
  ; ("<", LSTHN)
  ; (">", GTTHN)
  ; ("if", IF)
  ; ("then", THEN)
  ; ("else", ELSE)
  ; ("let", LET)
  ; ("=", EQUALS)
  ; ("in", IN)
  ; ("fun", FUN)
  ; ("->", ARROW)
  ; ("fix", FIX)
  ; (":", COLON)
  ; ("int", TINT)
  ; ("bool", TBOOL)
  ; ("()", UNIT)
  ; ("unit", TUNIT)
  ; (",", COMMA)
  ; ("[", LSQUARE)
  ; ("]", RSQUARE)
  ; ("[]", EMPTYLIST)
  ; ("::", CONS)
  ; ("hd", HEAD)
  ; ("tl", TAIL)
  ; ("empty?", EMPTY)
  ; ("ref", REF)
  ; (":=", SET)
  ; ("!", BANG)
  ; (";", SEMI)
  ; ("while", WHILE)
  ; ("do", DO)
  ; ("end", END)
  ]

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  List.assoc str symbols

let create_var lexbuf =
  let str = lexeme lexbuf in
  NAME (str)
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let digit      = ['0'-'9']
let var_name   = ['a'-'z'  'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ]*

rule token = parse
  | eof                       { EOF }
  | '-'? digit+               { INT (int_of_string (lexeme lexbuf)) }
  | '-'? digit+ '.' digit*    { FLOAT (float_of_string (lexeme lexbuf)) }
  | whitespace+ | newline+    { token lexbuf }
  | '(' | ')' | '+' | '-'
  | '*' | '/' | '=' | '<'
  | '>' | ':' | ',' | '['
  | ']' | '!' | ';'           { create_symbol lexbuf }
  | "<=" | "if" | "then"
  | "else" | "let" | "in"
  | "in" | "fun" | "->"
  | ">=" | "fix" | "int"
  | "bool" | "()" | "unit"
  | "[]"
  | "::" | "hd" | "tl"
  | "empty?" | "ref" | ":="
  | "while" | "do" | "end"    { create_symbol lexbuf }
  | "true" | "false"          { BOOL (bool_of_string (lexeme lexbuf)) }
  | var_name                  { create_var lexbuf }
  | _ as c                    { raise @@ Lexer_error ("Unexpected character: " ^ Char.escaped c) }

