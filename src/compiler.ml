open Pervasives
open Eval
open Expr

let arg_list = []

let usage_msg =
    "Usage: compiler [flag] [filename.arith]\nAvailable flags:"

let main () =
  Arg.parse
  (Arg.align arg_list)
  (fun x -> (open_in x
    |> Lexing.from_channel
    |> Parser.expr_eof Lexer.token
    |> eval
    |> string_of_value
    |> print_endline))
  usage_msg

let _ = if !Sys.interactive then () else main ()

