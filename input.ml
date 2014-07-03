(** Abstract syntax of input files. *)

(** Abstract syntax of expressions as given by the user. *)
type expr = expr' * Common.position
and expr' =
  | Var of Common.name
  | Universe of int
  | Pi of abstraction
  | Lambda of abstraction
  | App of expr * expr
  | Ann of expr * expr
 
(** An abstraction [(x,t,e)] indicates that [x] of type [t] is bound in [e]. *)
and abstraction = Common.variable * expr * expr

type clause = 
  | Clause of expr list * expr
  | Impossible of expr list * Common.name

type node = 
  | NodeSplit of Common.name * expr * branch list
and branch = 
  | Branch of pattern list * rhs
and pattern = 
  | PVar of Common.name
  | Dot of expr
  | PApp of pattern * pattern

and rhs =
  | Expr of expr
  | ImpossibleRhs of Common.name
  | Node of node

(** Toplevel directives. *)
type directive = directive' * Common.position
and directive' =
  | Quit
  | Help
  | Version
  | Context
  | Reset
  | Load of string
  | Axiom of Common.name * expr
  | Definition of Common.name * expr option * expr
  | Recursive of Common.name * expr * clause list
  | Split of Common.name * expr * node
  | Check of expr
  | Eval of expr
  | Whnf of expr
  | Inductive of Common.name * expr * (Common.name * expr) list
  | Set of Common.name * Config.setting_value
  | Get of Common.name
  | PrintSettings
  | Option of string
