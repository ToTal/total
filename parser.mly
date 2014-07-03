%{
  open Input

  (* Build nested lambdas *)
  let rec make_lambda e = function
    | [] -> e
    | ((xs, t), loc) :: lst ->
      let e = make_lambda e lst in
        List.fold_right (fun x e -> (Lambda (x, t, e), loc)) xs e

  (* Build nested pies *)
  let rec make_pi e = function
    | [] -> e
    | ((xs, t), loc) :: lst ->
      let e = make_pi e lst in
        List.fold_right (fun x e -> (Pi (x, t, e), loc)) xs e

%}

%token ON YIELDS
%token FORALL FUN TYPE IMPOSSIBLE
%token <int> NUMERAL
%token <string> NAME
%token LPAREN RPAREN
%token COLON COMMA PERIOD COLONEQUAL BAR
%token ARROW DARROW
%token QUIT HELP VERSION AXIOM CHECK EVAL WHNF CONTEXT DEFINITION 
%token INDUCTIVE RECURSIVE SPLIT
%token LOAD RESET
%token <string> STRING
%token <string> OPTION
%token SET GET SET_ON SET_OFF
%token EOF

%start <Input.directive list> directives

%%

(* Toplevel syntax *)

directives:
  | dir = directive PERIOD EOF
     { [dir] }
  | dir = directive PERIOD lst = directives
     { dir :: lst }

directive: mark_position(plain_directive) { $1 }
plain_directive:
  | QUIT
    { Quit }
  | HELP
    { Help }
  | VERSION
    { Version }
  | AXIOM x = NAME COLON e = expr
    { Axiom (x, e) }
  | CHECK e = expr
    { Check e }
  | EVAL e = expr
    { Eval e}
  | WHNF e = expr
    { Whnf e}
  | DEFINITION x = NAME t = option(preceded(COLON,expr)) COLONEQUAL e = expr
    { Definition (x, t, e) }
  | RECURSIVE f = NAME COLON t = expr COLONEQUAL cs = clause*
    { Recursive (f, t, cs) }
  | SPLIT f = NAME COLON t = expr COLONEQUAL nd = node
    { Split (f, t, nd) }
  | CONTEXT
    { Context }
  | INDUCTIVE x = NAME COLON e = expr COLONEQUAL cs = constructor*
    { Inductive (x,  e, List.rev cs)} (* We want the constructors in the same order as the file *)
  | opt = OPTION
    { Option opt }
  | RESET
    { Reset }
  | SET n = NAME COLONEQUAL o = set_value
    { Set (n, o) }
  | SET
    { PrintSettings }
  | GET n = NAME
    { Get n }
  | LOAD file = STRING
    { Load file }

constructor :
  | BAR c = NAME COLON e = expr
    { (c, e) }

clause :
  | BAR ps = simple_expr* DARROW e = expr
    { Clause (ps, e) }
  | BAR ps = simple_expr* IMPOSSIBLE x = NAME
    { Impossible (ps, x) }

node : 
  | ON x = NAME COLON t = expr YIELDS bs = branch*
    { NodeSplit (x, t, bs) }

branch :
  | BAR params = simple_pattern*  rhs = st_rhs
    { Branch (params, rhs) }

pattern :
  | PERIOD e = simple_expr
    { Dot e }
  | p1 = pattern p2 = simple_pattern
    { PApp (p1, p2) }
  | n = NAME
    { PVar n }

simple_pattern :
  | PERIOD e = simple_expr
    { Dot e }
  | n = NAME
    { PVar n }
  | LPAREN p =  pattern RPAREN
    { p }

st_rhs :
  | DARROW e = expr
    { Expr e }
  | DARROW LPAREN n = node RPAREN
    { Node n }
  | IMPOSSIBLE x = NAME
    { ImpossibleRhs x }

set_value :
  | SET_ON
    { Config.BoolSetting true }
  | SET_OFF
    { Config.BoolSetting false }
  | s = STRING
    { Config.StringSetting s }

(* Main syntax tree *)
expr: mark_position(plain_expr) { $1 }
plain_expr:
  | e = plain_app_expr
    { e }
  | FORALL lst = abstraction COMMA e = expr
    { fst (make_pi e lst) }
  | FUN lst = abstraction DARROW e = expr
    { fst (make_lambda e lst) }
  | t1 = app_expr ARROW t2 = expr
    { Pi (Common.none (), t1, t2) }

app_expr: mark_position(plain_app_expr) { $1 }
plain_app_expr:
  | e = plain_simple_expr
    { e }
  | TYPE k = NUMERAL
    { Universe k }
  | e1 = app_expr COLON e2 = simple_expr
    { Ann (e1, e2) }
  | e1 = app_expr e2 = simple_expr
    { App (e1, e2) }

simple_expr: mark_position(plain_simple_expr) { $1 }
plain_simple_expr:
  | n = NAME
    { Var n }
  | LPAREN e = plain_expr RPAREN
    { e }

abstraction:
  | b = bind1
    { [b] }
  | bs = binds
    { bs }

bind1: mark_position(plain_bind1) { $1 }
plain_bind1:
  | xs = nonempty_list(NAME) COLON t = expr
    { (List.map (fun x-> Common.some x) xs, t) }

binds:
  | LPAREN b = bind1 RPAREN
    { [b] }
  | LPAREN b = bind1 RPAREN lst = binds
    { b :: lst }

mark_position(X):
  x = X
  { x, Common.Position ($startpos, $endpos) }

%%
