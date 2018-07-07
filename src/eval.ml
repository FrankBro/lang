open Expr

module EvalEnv = Map.Make (String)

let rec eval_expr env (expr: expr) : value =
    match expr with
    | Value v -> v
    | Var name -> eval_expr env (Value (EvalEnv.find name env))
    | Call (fn_expr, arg_exprs) -> eval_call env fn_expr arg_exprs
    | Let (name, value_expr, body_expr) ->
        let (value: value) = eval_expr env value_expr in
        let env = EvalEnv.add name value env in
        eval_expr env body_expr
	| Case _ -> failwith "TODO"

and eval_call env (fn_expr: expr) (arg_exprs: expr list) : value =
    let fn_value =
        match fn_expr with
        | Var name -> EvalEnv.find name env
        | _ -> failwith "Call first arg is not a variable"
    in
    match fn_value with
    | Fun (arg_names, body_expr) ->
        let initial_env = env in
        let fn_env =
            List.fold_right2 (fun arg_name arg_expr env ->
                let arg_value = eval_expr initial_env arg_expr in
                EvalEnv.add arg_name arg_value env
            ) arg_names arg_exprs env
        in
        eval_expr fn_env body_expr
    | _ -> failwith "fn_value was not a fun"

let eval expr : value =
    eval_expr EvalEnv.empty expr

