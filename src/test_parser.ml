open OUnit2
open Expr

type result =
	| OK of expr
	| Fail

let record label_expr_list record = Value (RecordExtend(label_map_from_list label_expr_list, record))

let test_cases = [
	("", Fail);
	("a", OK (Var "a"));
	("f(x, y)", OK (Call(Var "f", [Var "x"; Var "y"])));
	("f(x)(y)", OK (Call(Call(Var "f", [Var "x"]), [Var "y"])));
	("let f = fun x y -> g(x, y) in f(a, b)",
		OK (Let("f", Value (Fun(["x"; "y"], Call(Var "g", [Var "x"; Var "y"]))),
			Call(Var "f", [Var "a"; Var "b"]))));
	("let x = a in " ^
	 "let y = b in " ^
	 "f(x, y)", OK (Let("x", Var "a", Let("y", Var "b", Call(Var "f", [Var "x"; Var "y"])))));
	("f x", Fail);
	("let a = one", Fail);
	("a, b", Fail);
	("a = b", Fail);
	("()", Fail);
	("fun x, y -> y", Fail);
    ("1", OK (Value (Int 1)));
    ("-1", OK (Value (Int (-1))));
    ("true", OK (Value (Bool true)));
    ("false", OK (Value (Bool false)));
    ("3.14", OK (Value (Float 3.14)));
    ("-3.14", OK (Value (Float (-3.14))));
    ("1.", OK (Value (Float 1.)));
    ("let f = fun a -> a in f(1)", 
        OK (Let("f", Value (Fun(["a"], Var "a")),
            Call(Var "f", [Value (Int 1)]))));
	(* records *)
	("{}", OK (Value RecordEmpty));
	("{ }", OK (Value RecordEmpty));
	("{", Fail);
	("a.x", OK (Value (RecordSelect(Var "a", "x"))));
	("{m - a}", OK (Value (RecordRestrict(Var "m", "a"))));
	("{m - a", Fail);
	("m - a", Fail);
	("{a = x}", OK (record [("a", [Var "x"])] (Value RecordEmpty)));
	("{a = x", Fail);
	("{a=x, b = y}", OK (record [("a", [Var "x"]); ("b", [Var "y"])] (Value RecordEmpty)));
	("{b = y ,a=x}", OK (record [("a", [Var "x"]); ("b", [Var "y"])] (Value RecordEmpty)));
	("{a=x,h=w,d=y,b=q,g=z,c=t,e=s,f=r}",
		OK (record [("a", [Var "x"]); ("b", [Var "q"]); ("c", [Var "t"]); ("d", [Var "y"]);
			("e", [Var "s"]); ("f", [Var "r"]); ("g", [Var "z"]); ("h", [Var "w"])] (Value RecordEmpty)));
	("{a = x|m}", OK (record [("a", [Var "x"])] (Var "m")));
	("{a | m}", Fail);
	("{ a = x, b = y | m}", OK (record [("a", [Var "x"]); ("b", [Var "y"])] (Var "m")));
	("{ a = x, b = y | {m - a} }",
		OK (record [("a", [Var "x"]); ("b", [Var "y"])] (Value (RecordRestrict(Var "m", "a")))));
	("{ b = y | m - a }", Fail);
	("let x = {a = f(x), b = y.b} in { a = fun z -> z | {x - a} }",
		OK (Let("x", record [("a", [Call(Var "f", [Var "x"])]); ("b", [Value (RecordSelect(Var "y", "b"))])] (Value RecordEmpty), record [("a", [Value (Fun(["z"], Var "z"))])]
		(Value (RecordRestrict (Var "x", "a"))))));
]

let string_of_result = function
	| Fail -> "Fail"
	| OK expr -> "OK (" ^ string_of_expr expr ^ ")"

let rec cmp_value value1 value2 = match (value1, value2) with
    | Bool b1, Bool b2 -> b1 = b2
    | Int i1, Int i2 -> i1 = i2
    | Float f1, Float f2 -> f1 = f2
	| Fun(params1, body1), Fun(params2, body2) ->
			params1 = params2 && cmp_expr body1 body2
	| RecordSelect(r1, label1), RecordSelect(r2, label2) ->
			label1 = label2 && cmp_expr r1 r2
	| RecordExtend(label_expr_map1, r1), RecordExtend(label_expr_map2, r2) ->
			LabelMap.equal (List.for_all2 cmp_expr) label_expr_map1 label_expr_map2 && cmp_expr r1 r2
	| RecordRestrict(r1, label1), RecordRestrict(r2, label2) ->
			label1 = label2 && cmp_expr r1 r2
	| RecordEmpty, RecordEmpty -> true
    | _, _ -> false

and cmp_expr expr1 expr2 = match (expr1, expr2) with
    | Value value1, Value value2 -> cmp_value value1 value2
	| Var name1, Var name2 -> name1 = name2
	| Call(fn1, args1), Call(fn2, args2) ->
			cmp_expr fn1 fn2 && List.for_all2 cmp_expr args1 args2
	| Let(name1, expr1, body1), Let(name2, expr2, body2) ->
			name1 = name2 && cmp_expr expr1 expr2 && cmp_expr body1 body2
	| _, _ -> false

let cmp_result result1 result2 = match (result1, result2) with
	| Fail, Fail -> true
	| OK expr1, OK expr2 -> cmp_expr expr1 expr2
	| _ -> false

let make_single_test_case (code, expected_result) =
	String.escaped code >:: fun _ ->
		let result =
			try
				OK (Parser.expr_eof Lexer.token (Lexing.from_string code))
			with Parsing.Parse_error ->
				Fail
		in
		assert_equal ~printer:string_of_result ~cmp:cmp_result expected_result result

let suite =
	"test_parser" >::: List.map make_single_test_case test_cases

