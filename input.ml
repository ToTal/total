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

and clause = 
  | Clause of expr list * expr
  | Impossible of expr list * Common.name

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
  | Check of expr
  | Eval of expr
  | Whnf of expr
  | Inductive of Common.name * expr * (Common.name * expr) list
  | Option of string
