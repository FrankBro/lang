open List

 type name = string

type lit =
    | LInt of int
    | LBool of bool
    | LFloat of float

type bin_op =
    | OAdd
    | OSubtract
    | OMultiply
    | ODivide
    | OLessThanEq
    | OGreaterThanEq
    | OLessThan
    | OGreaterThan
    | OEquals

 type id = int
 type level = int

type typ =
    | TConst of name (* type constants: `unit`, `int`, `float` or `bool` *)
    | TApp of typ * typ list (* type application: `list`, `tuple` or `ref` *)
    | TArrow of typ * typ (* function type: `int -> int` *)
    | TVar of tvar ref (* type variable *)
and tvar =
    | Unbound of id * level
    | Link of typ
    | Generic of id

type value =
    | VLit of lit
    | VFun of exp * exp * typ * typ
    | VFix of exp * exp * exp * typ * typ
    | VUnit
    | VTuple of exp list
    | VEmptyList of typ
    | VCons of exp * exp
    | VPtr of int
and exp =
    | EVal of value
    | EBinOp of bin_op * exp * exp
    | EIf of exp * exp * exp
    | EVar of string
    | ELet of exp * exp * exp * typ
    | EFunCall of exp * exp
    | EHead of exp
    | ETail of exp
    | EEmpty of exp
    | ERef of exp
    | EAssign of exp * exp
    | EBang of exp
    | ESequence of exp * exp
    | EWhile of exp * exp * exp
    | ENth of exp * exp
    | EInferLet of exp * exp * exp

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

let string_of_type (ty: typ) : string =
	let id_name_map = Hashtbl.create 10 in
	let count = ref 0 in
	let next_name () =
		let i = !count in
		incr count ;
		let name = String.make 1 (Char.chr (97 + i mod 26)) ^
			if i >= 26 then string_of_int (i / 26) else ""
		in
		name
	in
	let rec f is_simple x = 
        match x with
		| TConst name -> name
		| TApp (ty, ty_arg_list) ->
            begin
                match ty_arg_list with
                | [ty_arg] -> f true ty_arg ^ " " ^ f false ty
                | _ ->
                    let xs = String.concat ", " (List.map (f false) ty_arg_list) in
                    let x = 
                        match f false ty with
                        | "tuple" -> ""
                        | x -> x
                    in
                    "(" ^ xs ^ ")" ^ x
            end
		| TArrow (in_ty, out_ty) ->
            begin
                let in_str = f true in_ty in
                let out_str = f true out_ty in
                in_str ^ " -> " ^ out_str
            end
		| TVar {contents = Generic id} -> 
            begin
                try
                    Hashtbl.find id_name_map id
                with Not_found ->
                    let name = next_name () in
                    Hashtbl.add id_name_map id name ;
                    name
            end
		| TVar {contents = Unbound(id, _)} -> "_" ^ string_of_int id
		| TVar {contents = Link ty} -> f is_simple ty
	in
	let ty_str = f false ty in
	if !count > 0 then
		let var_names = Hashtbl.fold (fun _ value acc -> value :: acc) id_name_map [] in
		"forall[" ^ String.concat " " (List.sort String.compare var_names) ^ "] " ^ ty_str
	else
		ty_str

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

let rec string_of_exp_parsed (e: exp) : string =
    match e with
    | EVal v -> string_of_value_parsed v
    | EBinOp (op, exp1, exp2) ->
        "(" ^ (string_of_bin_op op) ^ " " ^ (string_of_exp_parsed exp1) ^ " "
            ^ (string_of_exp_parsed exp2) ^ ")"
    | EIf (exp1, exp2, exp3) ->
        "(if " ^ (string_of_exp_parsed exp1) ^ " " ^ (string_of_exp_parsed exp2) ^ " "
            ^ (string_of_exp_parsed exp3) ^ ")"
    | EVar s -> s
    | ELet (expr1, expr2, expr3, t) -> ("(let " ^ (string_of_exp_parsed expr1)
            ^ " : " ^ (string_of_type t) ^ " = "
            ^ (string_of_exp_parsed expr2) ^ " in " ^ (string_of_exp_parsed expr3) ^ ")")
    | EFunCall (expr1, expr2) -> ("(" ^ (string_of_exp_parsed expr1) ^ " "
        ^ (string_of_exp_parsed expr2) ^ ")")
    | EHead expr -> ("(head " ^ (string_of_exp_parsed expr) ^ ")")
    | ETail expr -> ("(tail " ^ (string_of_exp_parsed expr) ^ ")")
    | EEmpty expr -> ("(empty? " ^ (string_of_exp_parsed expr) ^ ")")
    | ERef expr -> ("(ref " ^ (string_of_exp_parsed expr) ^ ")")
    | EAssign (expr1, expr2) -> ("(:= " ^ (string_of_exp_parsed expr1) ^ " " ^ (string_of_exp_parsed expr2) ^ ")")
    | EBang expr -> ("(! " ^ (string_of_exp_parsed expr) ^ ")")
    | ESequence (expr1, expr2) -> ("(; " ^ (string_of_exp_parsed expr1) ^ " " ^ (string_of_exp_parsed expr2) ^ ")")
    | EWhile (expr1, expr2, expr3) -> ("(while " ^ (string_of_exp_parsed expr1) ^ " " ^ (string_of_exp_parsed expr3) ^ ")")
    | ENth (expr1, expr2) -> ("(nth " ^ (string_of_exp_parsed expr1) ^ " " ^ (string_of_exp_parsed expr2) ^ ")")
    | EInferLet (e1, e2, e3) -> ("(let " ^ (string_of_exp_parsed e1)
        ^ " = " ^ (string_of_exp_parsed e2) ^ " in " ^ (string_of_exp_parsed e3) ^ ")")
and string_of_value_parsed (v: value) : string =
    match v with
    | VLit (LInt i) -> string_of_int i
    | VLit (LBool b) -> string_of_bool b
    | VLit (LFloat f) -> string_of_float f
    | VFun (e1, e2, t1, t2) -> ("(fun (" ^ (string_of_exp_parsed e1) ^ ": "
        ^ (string_of_type t1) ^ ") : " ^ (string_of_type t2) ^ " -> "
        ^ (string_of_exp_parsed e2) ^ ")")
    | VFix (e1, e2, e3, t1, t2) -> ("(fix " ^ (string_of_exp_parsed e1) ^ " ("
        ^ (string_of_exp_parsed e2) ^ ": " ^ (string_of_type t1) ^ ") : "
        ^ (string_of_type t2) ^ " -> " ^ (string_of_exp_parsed e3) ^ ")")
    | VUnit -> "()"
    | VTuple ([]) -> "()T"
    | VTuple (e :: rest) -> ("(" ^ (string_of_exp_parsed e) ^ (String.concat "" (map (fun x -> ", " ^ x) (map string_of_exp_parsed rest))) ^ ")")
    | VEmptyList t -> ("([] : " ^ (string_of_type t) ^ ")")
    | VCons (e1, e2) -> ("(" ^ (string_of_exp_parsed e1) ^ " :: " ^ (string_of_exp_parsed e2) ^ ")")
    | VPtr i -> ("(ptr, address: " ^ (string_of_int i) ^ ")")

let rec string_of_exp (e: exp) : string =
    match e with
    | EVal v -> string_of_value v
    | EBinOp (op, e1, e2) ->
        ((string_of_exp e1) ^ " " ^ (string_of_bin_op op) ^ " " ^ (string_of_exp e2))
    | EIf (e1, e2, e3) ->
        ("if " ^ (string_of_exp e1) ^ " then " ^ (string_of_exp e2) ^ "\n        else "
        ^ (string_of_exp e3))
    | EVar s -> s
    | ELet (e1, e2, e3, t) ->
        ("let " ^ (string_of_exp e1) ^ " : " ^ (string_of_type t) ^ " = "
        ^ (string_of_exp e2) ^ " in\n        " ^ (string_of_exp e3))
    | EFunCall (e1, e2) -> ((string_of_exp e1) ^ " " ^ (string_of_exp e2))
    | EHead expr -> ("head " ^ (string_of_exp expr))
    | ETail expr -> ("tail " ^ (string_of_exp expr))
    | EEmpty expr -> ("empty? " ^ (string_of_exp expr))
    | ERef expr -> ("ref " ^ (string_of_exp expr))
    | EAssign (e1, e2) -> ((string_of_exp e1) ^ " := " ^ (string_of_exp e2))
    | EBang e -> ("! " ^ (string_of_exp e))
    | ESequence (e1, e2) -> ((string_of_exp e1) ^ "; " ^ (string_of_exp e2))
    | EWhile (e1, e2, e3) -> ("while " ^ (string_of_exp e1) ^ " do " ^ (string_of_exp e3) ^ " end")
    | ENth (e1, e2) -> ("nth " ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2) ^ ")")
    | EInferLet (e1, e2, e3) -> ("let " ^ (string_of_exp_parsed e1)
        ^ " = " ^ (string_of_exp_parsed e2) ^ " in\n        " ^ (string_of_exp_parsed e3))
and string_of_value (v: value) : string =
    match v with
    | VLit (LInt i) -> string_of_int i
    | VLit (LBool b) -> string_of_bool b
    | VLit (LFloat f) -> string_of_float f
    | VFun (e1, e2, t1, t2) -> ("fun (" ^ (string_of_exp e1) ^ ": "
        ^ (string_of_type t1) ^ ") : " ^ (string_of_type t2) ^ " -> " ^ (string_of_exp e2))
    | VFix (e1, e2, e3, t1, t2) -> ("fix " ^ (string_of_exp e1) ^ " (" ^ (string_of_exp e2) ^ ": "
        ^ (string_of_type t2) ^ ") : " ^ (string_of_type t2) ^ " -> "
        ^ (string_of_exp e3))
    | VUnit -> "()"
    | VTuple ([]) -> "()T"
    | VTuple (e :: rest) -> ("(" ^ (string_of_exp_parsed e) ^ (String.concat "" (map (fun x -> ", " ^ x) (map string_of_exp_parsed rest))) ^ ")")
    | VEmptyList t -> ("[] : " ^ (string_of_type t))
    | VCons (e1, e2) -> ((string_of_exp e1) ^ " :: " ^ (string_of_exp e2))
    | VPtr i -> ("(ptr, address: " ^ (string_of_int i) ^ ")")

let rec type_equals (t1: typ) (t2: typ) : bool =
    match t1, t2 with
    | TConst n1, TConst n2 -> n1 = n2
    | TApp (ty1, tys1), TApp (ty2, tys2) -> type_equals ty1 ty2 && List.for_all (fun (t1, t2) -> type_equals t1 t2) (List.combine tys1 tys2)
    | TArrow (ta1, ta2), TArrow (tb1, tb2) -> type_equals ta1 tb1 && type_equals ta2 tb2
    | TVar {contents = Unbound (i1, l1)}, TVar {contents = Unbound (i2, l2)} -> i1 = i2 && l1 = l2
    | TVar {contents = Link t1}, TVar {contents = Link t2} -> type_equals t1 t2
    | TVar {contents = Generic id1}, TVar {contents = Generic id2} -> id1 = id2
    | _ -> false

let type_of_bin_op_in (op: bin_op) (t1: typ) (t2: typ) : typ =
    match op, t1, t2 with
    | _, TConst "int", TConst "int" -> TConst "int"
    | _, TConst "float", TConst "float" -> TConst "float"
    | _ -> failwith "Error, type_of_bin_op_in does not have two matching types"

let type_of_bin_op_out (op: bin_op) (t1: typ) (t2: typ) : typ =
    match op with
    | OAdd | OSubtract | OMultiply | ODivide -> 
        (match t1, t2 with
        | TConst "int", TConst "int" -> TConst "int"
        | TConst "float", TConst "float" -> TConst "float"
        | _ -> failwith "Error, type_of_bin_op_out does not have two matching types")
    | _ -> TConst "bool"

let unpack_int_val v =
    match v with
    | VLit (LInt i) -> i
    | _ -> failwith "Error, unpack_int_val needs an int val"

let unpack_bool_val v =
    match v with
    | VLit (LBool b) -> b
    | _ -> failwith "Error, unpack_bool_val needs a bool val"

let unpack_float_val v =
    match v with
    | VLit (LFloat f) -> f
    | _ -> failwith "Error, unpack_float_val needs a float val"

let exp_to_value (e: exp) : value =
    match e with
    | EVal v -> v
    | _ -> failwith ("exp_to_value called with non-value expression: " ^ (string_of_exp e))

let rec typecheck (ctx: (string * typ) list) (e: exp) : typ =
    match e with
    | ESequence (e1, e2) ->
        let _  = (typecheck ctx e1) in
        (typecheck ctx e2)
    | EVal (VLit (LInt _)) -> TConst "int"
    | EVal (VLit (LBool _)) -> TConst "bool"
    | EVal (VLit (LFloat _)) -> TConst "float"
    | EVal (VFun (EVar s, e', t1, t2)) ->
        let e_type = typecheck (cons (s, t1) ctx) e' in
        if type_equals e_type t2 then TArrow (t1, t2)
        else failwith ("Function typechecking failed, expected return type: "
        ^ (string_of_type (TArrow (t1, t2))) ^ ", actual: " ^ (string_of_type (TArrow (t1, e_type))))
    | EVal (VFix (EVar f, EVar s, e, t1, t2)) ->
        let e_type = typecheck (cons (f, TArrow(t1,t2)) (cons (s, t1) ctx)) e in
        if type_equals e_type t2 then TArrow (t1, t2)
        else failwith ("Fixpoint typechecking failed, expected return type: "
        ^ (string_of_type (TArrow (t1, t2))) ^ ", actual: " ^ (string_of_type (TArrow (t1, e_type))))
    | EVal (VUnit) -> TConst "unit"
    | EVal (VTuple []) -> TApp (TConst "tuple", [])
    | EVal (VTuple (ex :: rest)) ->
        begin
            match (typecheck ctx (EVal (VTuple rest))) with
            | TApp (TConst "tuple", rest') -> (TApp (TConst "tuple", (typecheck ctx ex) :: rest'))
            | _ -> failwith "Typechecking missed tuples"
        end
    | EVal (VEmptyList t) -> TApp (TConst "list", [t])
    | EVal (VCons (e1, e2)) ->
        begin
        let e1_type = (typecheck ctx e1) in
        let e2_type = (typecheck ctx e2) in
        match e1_type, e2_type with
        | t1, TApp (TConst "list", [t2]) -> (if (type_equals t1 t2) then TApp (TConst "list", [t1])
            else failwith ("Cons typechecking failed, e1 should have type of contents"
            ^ " of e2, actual: " ^ (string_of_type e1_type) ^ ", " ^ (string_of_type e2_type)))
        | _ -> failwith ("Cons typechecking failed, e2 should have type list, "
            ^ "actual: " ^ (string_of_type e2_type))
        end
    | EVal (VPtr i) -> failwith("Typechecking error: should not have encountered a pointer value")
    | EBinOp (op, e1, e2) ->
        let e1_type = typecheck ctx e1 in
        let e2_type = typecheck ctx e2 in
        let in_type = type_of_bin_op_in op e1_type e2_type in
        if (type_equals e1_type in_type) && (type_equals e2_type in_type) then type_of_bin_op_out op e1_type e2_type
        else failwith ("Binary op typechecking failed, expected input type: "
        ^ (string_of_type in_type) ^ ", actual: " ^ (string_of_type e1_type)
        ^ " and " ^ (string_of_type e2_type))
    | EIf (e1, e2, e3) ->
        let e1_type = typecheck ctx e1 in
        let e2_type = typecheck ctx e2 in
        let e3_type = typecheck ctx e3 in
        if (type_equals e1_type (TConst "bool")) && (type_equals e2_type e3_type)
        then e2_type
        else failwith
        ("If typechecking failed, expected format: if <bool> then <t> else <t>, "
        ^ "actual: " ^ (string_of_type e1_type) ^ ", " ^ (string_of_type e2_type)
        ^ ", " ^ (string_of_type e3_type))
    | EVar s -> List.assoc s ctx
    | ELet (EVar s, e1, e2, t) ->
        let e1_type = typecheck ctx e1 in
        let e2_type = typecheck (cons (s, t) ctx) e2 in
        if (type_equals t e1_type) then e2_type
        else failwith ("Let typechecking failed, expected binding type: "
        ^ (string_of_type t) ^ ", actual: " ^ (string_of_type e1_type))
    | EFunCall (e1, e2) ->
        begin
        let e1_type = typecheck ctx e1 in
        let e2_type = typecheck ctx e2 in
        match e1_type with
        | TArrow (t1, t2) ->
            begin
                if type_equals t1 e2_type then t2
                else failwith ("Fun call typechecking failed, expected input type: "
                ^ (string_of_type t1) ^ ", actual: " ^ (string_of_type e2_type) ^ (string_of_exp e1) ^ " " ^ (string_of_exp e2))
            end
        | _ -> failwith ("Fun call typechecking error, first expression should be"
            ^ " of type function, actual: " ^ (string_of_type e1_type))
        end
    | EHead ex ->
        begin
        let e_type = (typecheck ctx ex) in
        match e_type with
        | TApp (TConst "list", [t]) -> t
        | _ -> failwith ("Head typechecking failed, exp should be of type list, "
            ^ "actual: " ^ (string_of_type e_type))
        end
    | ETail ex ->
        begin
        let e_type = (typecheck ctx ex) in
        match e_type with
        | TApp (TConst "list", [t]) -> TApp (TConst "list", [t])
        | _ -> failwith ("Tail typechecking failed, exp should be of type list, "
            ^ "actual: " ^ (string_of_type e_type))
        end
    | EEmpty ex ->
        begin
        let e_type = (typecheck ctx ex) in
        match e_type with
        | TApp (TConst "list", [t]) -> TConst "bool"
        | _ -> failwith ("empty? typechecking failed, exp should be of type list, "
            ^ "actual: " ^ (string_of_type e_type))
        end
    | ERef ex -> TApp (TConst "ref", [typecheck ctx ex])
    | EAssign (e1, e2) ->
        begin
        let e1_type = (typecheck ctx e1) in
        let e2_type = (typecheck ctx e2) in
        match e1_type, e2_type with
        | TApp (TConst "ref", [t1]), t2 -> if (type_equals t1 t2) then TConst "unit"
            else failwith ("Assignment typechecking failed, type of ref should be same as "
            ^ "type of value, actual: " ^ (string_of_type t1) ^ ", " ^ (string_of_type t2))
        | t, _ -> failwith ("Assignment typechecking failed, first value should "
            ^ "have type <t>, actual: " ^ (string_of_type t))
        end
    | EBang ex ->
        begin
        let e_type = (typecheck ctx ex) in
        match e_type with
        | TApp (TConst "ref", [t]) -> t
        | _ -> failwith ("Bang typechecking failed, should take ref type, actual "
            ^ (string_of_type e_type))
        end
    | EWhile (e1, e2, e3) ->
        begin
        let e1_type = (typecheck ctx e1) in
        match e1_type with
        | TConst "bool" -> TConst "unit"
        | _ -> failwith ("While typechecking failed, e1 should be of type bool, "
            ^ "actual: " ^ (string_of_type e1_type))
        end
    | ENth (e1, e2) ->
        if (not (type_equals (typecheck ctx e2) (TConst "int"))) then (failwith ("Nth typechecking failed, expects int type second"))
        else
        begin
         match (typecheck ctx e1) with
            | TApp (TConst "tuple", l) -> (nth l (unpack_int_val (exp_to_value e2)))
            | t -> failwith ("Nth typechecking failed, should take tuple first, "
                ^ "actual: " ^ (string_of_type t))
        end
    | EInferLet (EVar s, e1, e2) ->
        let e1_type = typecheck ctx e1 in
        let e2_type = typecheck (cons (s, e1_type) ctx) e2 in e2_type
    | _ -> failwith ("Typechecking failed, unrecognized formatting" ^ (string_of_exp e))

let rec subst (v: value) (s: string) (e: exp) : exp =
    match e with
    | EVal v' ->
        begin
            match v' with
            | VLit l -> e
            | VFun (EVar str, e', t1, t2) -> (if (compare str s) = 0
                then e
                else EVal (VFun (EVar str, (subst v s e'), t1, t2)))
            | VFix (EVar f, EVar x, e', t1, t2) ->
                begin
                match (compare f s), (compare x s) with
                | 0,_ -> e
                | _,0 -> e
                | _ -> EVal (VFix (EVar f, EVar x, (subst v s e'), t1, t2))
                end
            | VUnit -> e
            | VTuple [] -> EVal (VTuple [])
            | VTuple (ex :: rest) ->
                begin
                    match (subst v s (EVal (VTuple rest))) with
                    | EVal (VTuple rest') -> EVal (VTuple ((subst v s ex) :: rest'))
                    | _ -> failwith "Tuple substitution unexpected value"
                end
            | VEmptyList t -> EVal (VEmptyList t)
            | VCons (e1, e2) -> EVal (VCons ((subst v s e1), (subst v s e2)))
            | VPtr i -> e
            | _ -> failwith "Substitution: unexpected value"
        end
    | EBinOp (op, e1, e2) -> EBinOp (op, (subst v s e1 ), (subst v s e2))
    | EIf (e1, e2, e3) -> EIf ((subst v s e1), (subst v s e2), (subst v s e3))
    | EVar str -> (if (compare str s) = 0 then EVal v else e)
    | ELet (EVar str, e1, e2, t) -> (if (compare str s) = 0 then e
        else ELet (EVar str, (subst v s e1), (subst v s e2), t))
    | EFunCall (f, e2) -> EFunCall ((subst v s f), (subst v s e2))
    | EHead ex -> EHead (subst v s ex)
    | ETail ex -> ETail (subst v s ex)
    | EEmpty ex -> EEmpty (subst v s ex)
    | ERef ex -> ERef (subst v s ex)
    | EAssign (e1, e2) -> EAssign ((subst v s e1), (subst v s e2))
    | EBang ex -> EBang (subst v s ex)
    | ESequence (e1, e2) -> ESequence ((subst v s e1), (subst v s e2))
    | EWhile (e1, e2, e3) -> EWhile ((subst v s e1), (subst v s e2), (subst v s e3))
    | ENth (e1, e2) -> (ENth ((subst v s e1), (subst v s e2)))
    | EInferLet (EVar str, e1, e2) -> (if (compare str s) = 0 then e
        else EInferLet (EVar str, (subst v s e1), (subst v s e2)))
    | _ -> failwith "Substitution: unrecognized expression"

let interpret_int_bin_exp (op: bin_op) (v1: value) (v2: value) : value =
    let i1 = unpack_int_val v1 in
    let i2 = unpack_int_val v2 in
    match op with
    | OAdd              -> VLit (LInt (i1 + i2))
    | OSubtract         -> VLit (LInt (i1 - i2))
    | OMultiply         -> VLit (LInt (i1 * i2))
    | ODivide           -> VLit (LInt (i1 / i2))
    | OLessThanEq       -> VLit (LBool (i1 <= i2))
    | OGreaterThanEq    -> VLit (LBool (i1 >= i2))
    | OLessThan         -> VLit (LBool (i1 < i2))
    | OGreaterThan      -> VLit (LBool (i1 > i2))
    | OEquals           -> VLit (LBool (i1 = i2))

let interpret_float_bin_exp (op: bin_op) (v1: value) (v2: value) : value =
    let f1 = unpack_float_val v1 in
    let f2 = unpack_float_val v2 in
    match op with
    | OAdd              -> VLit (LFloat (f1 +. f2))
    | OSubtract         -> VLit (LFloat (f1 -. f2))
    | OMultiply         -> VLit (LFloat (f1 *. f2))
    | ODivide           -> VLit (LFloat (f1 /. f2))
    | OLessThanEq       -> VLit (LBool (f1 <= f2))
    | OGreaterThanEq    -> VLit (LBool (f1 >= f2))
    | OLessThan         -> VLit (LBool (f1 < f2))
    | OGreaterThan      -> VLit (LBool (f1 > f2))
    | OEquals           -> VLit (LBool (f1 = f2))

let interpret_bin_exp (op: bin_op) (v1: value) (v2: value) : value =
    match v1, v2 with
    | VLit (LInt _), VLit (LInt _) -> interpret_int_bin_exp op v1 v2
    | VLit (LFloat _), VLit (LFloat _) -> interpret_float_bin_exp op v1 v2
    | _ -> failwith "interpret_bin_exp: invalid bin exp"

let rec is_value (e: exp) : bool =
    match e with
    | EVal (VTuple []) -> true
    | EVal (VTuple (ex :: rest)) -> (is_value ex) && (is_value (EVal (VTuple rest)))
    | EVal (VCons (e1, e2)) -> (is_value e1) && (is_value e2)
    | EVal _ -> true
    | _ -> false

let rec step (env: (int * value) list) (e: exp) : (int * value) list * exp =
    match e with
    | ESequence (e1, e2) ->
        let interp_e1 = not (is_value e1) in
        if interp_e1 then
        begin
            match (step env e1) with
            | env', e1' -> (env', ESequence (e1', e2))
        end
        else (env, e2)
    | EVal (VTuple []) -> (env, e)
    | EVal (VTuple (ex :: rest)) ->
        let interp_e1 = not (is_value ex) in
        if interp_e1 then
            let (env', ex') = (step env ex) in (env', EVal (VTuple (ex' :: rest)))
        else
        begin
            match (step env (EVal (VTuple rest))) with
            | env', EVal (VTuple rest') -> (env', EVal (VTuple (ex :: rest')))
            | _ -> failwith "Unexpected"
        end
    | EVal (VCons (e1, e2)) ->
        let interp_e1 = not (is_value e1) in
        let interp_e2 = not (is_value e2) in
        if interp_e1 then
            let (env', e1') = (step env e1) in (env', EVal (VCons (e1', e2)))
        else if interp_e2 then
            let (env', e2') = (step env e2) in (env', EVal (VCons (e1, e2')))
        else (env, e)
    | EVal v -> (env, e)
    | EBinOp (op, e1, e2) ->
        begin
            let interp_e1 = not (is_value e1) in
            let interp_e2 = not (is_value e2) in
            match interp_e1, interp_e2 with
            | true, _ ->
            begin
                match (step env e1) with
                | env', e1' -> (env', EBinOp (op, e1', e2))
            end
            | false, true ->
            begin
                match (step env e2) with
                | env', e2' -> (env', EBinOp (op, e1, e2'))
            end
            | false, false -> (env, EVal (interpret_bin_exp op (exp_to_value e1) (exp_to_value e2)))
        end
    | EIf (e1, e2, e3) ->
        begin
            let interp_e1 = not (is_value e1) in
            match interp_e1 with
            | true ->
            begin
                match (step env e1) with
                | env', e1' -> (env', EIf (e1', e2, e3))
            end
            | false -> if (unpack_bool_val (exp_to_value e1))
                then (step env e2) else (step env e3)
        end
    | ELet (EVar s, e1, e2, t) ->
        begin
            let interp_e1 = not (is_value e1) in
            if interp_e1 then
            begin
                match (step env e1) with
                | env', e1' -> (env', ELet ((EVar s), e1', e2, t))
            end
            else (env, (subst (exp_to_value e1) s e2))
        end
    | EVar s -> failwith ("Unbound variable binding: " ^ s);
    | EFunCall (e1, e2) ->
        begin
        let interp_e2 = not (is_value e2) in
        if interp_e2 then
        begin
            match (step env e2) with
            | env', e2' -> (env', EFunCall (e1, e2'))
        end
        else
            match e1 with
            | EFunCall _ ->
                begin
                match (step env e1) with
                | env', e1' -> (env', EFunCall (e1', e2))
                end
            | EVal (VFun (EVar s, f, t1, t2)) -> (env, subst (exp_to_value e2) s f)
            | EVal (VFix (EVar f, EVar x, e, t1, t2)) ->
                let subst_x = subst (exp_to_value e2) x e in
                (env, subst (exp_to_value e1) f subst_x)
            | _ -> failwith ("Interpretation: issue with function formatting")
        end
    | EHead ex ->
        let interp_ex = not (is_value ex) in
        if interp_ex then
        begin
            match (step env ex) with
            | env', ex' -> (env', EHead ex')
            end
        else
        begin
            match (exp_to_value ex) with
            | VEmptyList _ -> failwith ("Attempted to take hd of empty list")
            | VCons (e1, e2) -> (env, e1)
            | _ -> failwith ("Typechecking missed hd")
        end
    | ETail ex ->
        let interp_ex = not (is_value ex) in
        if interp_ex then
        begin
            match (step env ex) with
            | env', ex' -> (env', ETail ex')
            end
        else
        begin
            match (exp_to_value ex) with
            | VEmptyList _ -> failwith ("Attempted to take tl of empty list")
            | VCons (e1, e2) -> (env, e2)
            | _ -> failwith ("Typechecking missed tl")
        end
    | EEmpty ex ->
        let interp_ex = not (is_value ex) in
        if interp_ex then
        begin
            match (step env ex) with
            | env', ex' -> (env', EEmpty ex')
            end
        else
        begin
            match (exp_to_value ex) with
            | VEmptyList _ -> (env, EVal (VLit (LBool true)))
            | VCons (e1, e2) -> (env, EVal (VLit (LBool false)))
            | _ -> failwith ("Typechecking missed empty?")
        end
    | ERef ex ->
        begin
        let interp_ex = not (is_value ex) in
        if (interp_ex) then
        begin
            match (step env ex) with
            | env', ex' -> (env', ERef ex')
        end
        else
        let new_i = (length env) in
        ((cons (new_i, (exp_to_value ex)) env), EVal (VPtr new_i))
        end
    | EAssign (e1, e2) ->
        let interp_e1 = not (is_value e1) in
        let interp_e2 = not (is_value e2) in
        if interp_e1 then
        begin
            match (step env e1) with
            | env', e1' -> (env', EAssign (e1', e2))
        end
        else
        begin
            if interp_e2 then
                begin
                    match (step env e2) with
                    | env', e2' -> (env', EAssign (e1, e2'))
                    end
            else
                begin
                    match ((exp_to_value e1), (exp_to_value e2)) with
                    | VPtr i, v -> ((cons (i, v) env), EVal (VUnit))
                    | v, _ -> failwith("Typechecking missed :=, actual: " ^ (string_of_exp e1))
                end
        end
    | EBang ex ->
        let interp_ex = not (is_value ex) in
        if (interp_ex) then
        begin
            match (step env ex) with
            | env', ex' -> (env', EBang ex')
        end
        else
        begin
            match (exp_to_value ex) with
            | VPtr i -> (env, EVal (assoc i env))
            | _ -> failwith ("Typechecking missed !")
        end
    | EWhile (e1, e2, e3) ->
        let interp_e1 = not (is_value e1) in
        if (interp_e1) then
        begin
            match (step env e1) with
            | env', e1' -> (env', EWhile (e1', e2, e3))
        end
        else
        begin
            match (exp_to_value e1) with
            | VLit (LBool true) -> (env, ESequence(e3, EWhile (e2, e2, e3)))
            | VLit (LBool false) -> (env, EVal (VUnit))
            | _ -> failwith ("Typechecking missed while")
        end
    | ENth (e1, e2) ->
        let interp_e1 = not (is_value e1) in
        let interp_e2 = not (is_value e2) in
        if interp_e1 then
        let (env', e1') = (step env e1) in (env', (ENth (e1', e2)))
        else
        begin
            if interp_e2 then let (env', e2') = (step env e2) in (env', (ENth (e1, e2')))
            else
            let n = (unpack_int_val (exp_to_value e2)) in
            begin
            match e1 with
            | EVal (VTuple l) -> (env, (nth l n))
            | _ -> failwith ("Typechecking missed nth")
            end
        end
    | EInferLet (EVar s, e1, e2) ->
        begin
            let interp_e1 = not (is_value e1) in
            if interp_e1 then
            begin
                match (step env e1) with
                | env', e1' -> (env', EInferLet ((EVar s), e1', e2))
            end
            else (env, (subst (exp_to_value e1) s e2))
        end
    | _ -> failwith "Interpretation: unrecognized expression"

let eval (expr: exp) : value =
    let rec eval' (env: (int * value) list) (e: exp) : exp =
        if is_value e then e else
        begin
        match (step env e) with
        | env', e' -> (eval' env' e')
        end in
    let _ = typecheck [] expr in
    exp_to_value (eval' [] expr)

let eval_print (expr: exp) : unit =
    let rec eval' (env: (int * value) list) (e: exp) : exp =
        if is_value e then e
        else
            (let env', result = (step env e) in
            print_endline ("--> " ^ (string_of_exp result));
            (eval' env' result)) in
    let _ = typecheck [] expr in
    print_endline ("        " ^ string_of_exp expr);
    let _ = eval' [] expr in
    ()

