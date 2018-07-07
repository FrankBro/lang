{

open Parser

exception Error

}


let ident = ['_' 'A'-'Z' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let digit = ['0'-'9']

rule token = parse
	| [' ' '\t' '\r' '\n']      { token lexbuf }
	| "fun"                     { FUN }
	| "let"                     { LET }
	| "in"                      { IN }
	| "forall"                  { FORALL }
    | "true" | "false"          { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
	| "match"                   { MATCH }
	| ident                     { IDENT (Lexing.lexeme lexbuf) }
    | '-'? digit+               { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | '-'? digit+ '.' digit*    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
	| '('                       { LPAREN }
	| ')'                       { RPAREN }
	| '['                       { LBRACKET }
	| ']'                       { RBRACKET }
	| '{'                       { LBRACE }
	| '}'                       { RBRACE }
	| '='                       { EQUALS }
	| "->"                      { ARROW }
	| ','                       { COMMA }
	| '.'     { DOT }
	| '-'     { MINUS }
	| '|'     { PIPE }
	| ':'     { COLON }
	| eof                       { EOF }
	| _                         { raise Error }


{

let string_of_token = function
	| FUN -> "fun"
	| LET -> "let"
	| IN -> "in"
	| FORALL -> "forall"
	| MATCH -> "match"
	| IDENT ident -> ident
    | INT i -> string_of_int i
    | BOOL b -> string_of_bool b
    | FLOAT f -> string_of_float f
	| LPAREN -> "("
	| RPAREN -> ")"
	| LBRACKET -> "["
	| RBRACKET -> "]"
	| LBRACE -> "{"
	| RBRACE -> "}"
	| EQUALS -> "="
	| ARROW -> "->"
	| COMMA -> ","
	| DOT -> "."
	| MINUS -> "-"
	| PIPE -> "|"
	| COLON -> ":"
	| EOF -> "<eof>"

}

