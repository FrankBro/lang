open Expr

module EvalEnv = Map.Make (String)

let rec eval_expr env (expr: expr) : value =
    match expr with
    | Value v -> v
    | Var name -> eval_expr env (Value (EvalEnv.find name env))
    | Call _ -> failwith "TODO"
    | Let (name, value_expr, body_expr) ->
        let (value: value) = eval_expr env value_expr in
        let env = EvalEnv.add name value env in
        eval_expr env body_expr

let eval expr : value =
    eval_expr EvalEnv.empty expr

