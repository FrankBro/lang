%{
  open Lang
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> NAME

%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token PLUS       (* + *)
%token MINUS      (* - *)
%token TIMES      (* * *)
%token DIVIDE     (* / *)
%token LEQ        (* <= *)
%token GEQ        (* >= *)
%token LSTHN      (* < *)
%token GTTHN      (* > *)
%token IF         (* if *)
%token THEN       (* then *)
%token ELSE       (* else *)
%token LET        (* let *)
%token EQUALS     (* = *)
%token IN         (* in *)
%token FUN        (* fun *)
%token ARROW      (* -> *)
%token FIX        (* fix *)
%token COLON      (* : *)
%token TINT       (* int *)
%token TBOOL      (* bool *)
%token TFLOAT     (* float *)
%token UNIT       (* () *)
%token TUNIT      (* unit *)
%token COMMA      (* , *)
%token LSQUARE    (* [ *)
%token RSQUARE    (* ] *)
%token EMPTYLIST  (* [] *)
%token CONS       (* :: *)
%token HEAD       (* hd *)
%token TAIL       (* tl *)
%token EMPTY      (* empty? *)
%token REF        (* ref *)
%token SET        (* := *)
%token BANG       (* ! *)
%token SEMI       (* ; *)
%token WHILE
%token DO
%token END

%token EOF

%start <Lang.exp> prog

%left LET IN
%left SEMI
%nonassoc WHILE DO END
%left SET
%left LSTHN GTTHN LEQ GEQ EQUALS
%left PLUS MINUS
%left DIVIDE TIMES
%right CONS LSQUARE RSQUARE
%nonassoc HEAD TAIL EMPTY
%left COLON
%nonassoc BANG
%nonassoc REF
%%

prog:
  | e=exp EOF  { e }

(* Look back at function application *)

exp:
  | e=base_exp                                       { e }
  | e1=exp SEMI e2=exp                               { ESequence (e1, e2) }
  | WHILE e1=exp DO e2=exp END e3=exp                { ESequence ((EWhile (e1, e1, e2)), e3) }
  | f=base_exp e=exp                                 { EFunCall (f, e) }
  | e1=exp SET e2=exp                                { EAssign (e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp                { EIf (e1, e2, e3) }
  | LET n=NAME COLON t=typ EQUALS e1=exp IN e2=exp   { ELet (EVar n, e1, e2, t) }
  | LET n=NAME EQUALS e1=exp IN e2=exp               { EInferLet (EVar n, e1, e2) }
  | e1=exp PLUS e2=exp                               { (EBinOp (OAdd, e1, e2)) }
  | e1=exp MINUS e2=exp                              { (EBinOp (OSubtract, e1, e2)) }
  | e1=exp TIMES e2=exp                              { (EBinOp (OMultiply, e1, e2)) }
  | e1=exp DIVIDE e2=exp                             { (EBinOp (ODivide, e1, e2)) }
  | e1=exp LEQ e2=exp                                { (EBinOp (OLessThanEq, e1, e2)) }
  | e1=exp GEQ e2=exp                                { (EBinOp (OGreaterThanEq, e1, e2)) }
  | e1=exp GTTHN e2=exp                              { (EBinOp (OGreaterThan, e1, e2)) }
  | e1=exp LSTHN e2=exp                              { (EBinOp (OLessThan, e1, e2)) }
  | e1=exp EQUALS e2=exp                             { (EBinOp (OEquals, e1, e2)) }
(*   | PLUS   { OAdd }
  | LSTHN  { OLessThan } *)
(*  | e1=exp op=bin_op e2=exp                        { EBinOp (op, e1, e2) }*)
  | BANG e=exp                                       { EBang e }
  | HEAD e=exp                                       { EHead e }
  | TAIL e=exp                                       { ETail e }
  | EMPTY e=exp                                      { EEmpty e }
  | REF e=exp                                        { ERef e }
  | e1=exp LSQUARE e2=exp RSQUARE                    { ENth (e1, e2) }

(*bin_op:
  | PLUS   { OAdd }
  | MINUS  { OSubtract }
  | TIMES  { OMultiply }
  | DIVIDE { ODivide }
  | LEQ    { OLessThanEq }
  | GEQ    { OGreaterThanEq }
  | LSTHN  { OLessThan }
  | GTTHN  { OGreaterThan }
  | EQUALS { OEquals } *)

typ:
  | TINT                               { TConst "int" }
  | TBOOL                              { TConst "bool" }
  | TFLOAT                             { TConst "float" }
  | t1=typ ARROW t2=typ                { TArrow (t1, t2) }
  | TUNIT                              { TConst "unit" }
  | LPAREN t1=typ TIMES t2=ttyp RPAREN { TApp (TConst "tuple", t1 :: t2) }
  | LSQUARE t=typ RSQUARE              { TApp (TConst "list", [t]) }
  | LSTHN t=typ GTTHN                  { TApp (TConst "ref", [t]) }
  | LPAREN t=typ RPAREN                { t }

ttyp:
  | t=typ                              { (t :: []) }
  | t1=typ TIMES t2=ttyp               { (t1 :: t2) }

base_exp:
  | FUN LPAREN n=NAME COLON t1=typ RPAREN
    COLON t2=typ ARROW e=exp              { EVal (VFun (EVar n, e, t1, t2)) }
  | FIX n1=NAME LPAREN n2=NAME COLON t1=typ RPAREN
    COLON t2=typ ARROW e=exp              { EVal (VFix (EVar n1, EVar n2, e, t1, t2)) }
  | e1=exp CONS e2=exp                    { EVal (VCons (e1, e2)) }
  | EMPTYLIST COLON t=typ                 { EVal (VEmptyList t) }
  | i=INT                                 { EVal (VLit (LInt i)) }
  | b=BOOL                                { EVal (VLit (LBool b)) }
  | f=FLOAT                               { EVal (VLit (LFloat f)) }
  | n=NAME                                { EVar n }
  | UNIT                                  { EVal VUnit }
  | LPAREN e=exp RPAREN                   { e }
  | LPAREN e=exp COMMA t=tuple RPAREN     { EVal (VTuple (e :: t)) }

tuple:
  | e=exp                                 { (e :: []) }
  | e=exp COMMA t=tuple                   { (e :: t) }

