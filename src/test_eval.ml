open OUnit2
open Eval
open Expr

type result =
    | OK of value
    | Fail

let ok value = OK value

let test_cases = [
    ("1", OK (Int 1))
]

let string_of_result = function
    | Fail -> "Fail"
    | OK value -> "OK (" ^ string_of_value value ^ ")"

let make_single_test_case (code, expected_result) =
    String.escaped code >:: fun _ ->
        let result =
            try
                Lexing.from_string code
                |> (Parser.expr_eof Lexer.token)
                |> eval
                |> ok
            with Parsing.Parse_error ->
                Fail
        in
        assert_equal ~printer:string_of_result expected_result result

let suite =
    "test_eval" >::: List.map make_single_test_case test_cases

