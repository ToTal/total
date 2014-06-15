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

(** Toplevel directives. *)
type directive = directive' * Common.position
and directive' =
  | Quit
  | Help
  | Context
  | Axiom of Common.name * expr
  | Definition of Common.name * expr option * expr
  | Check of expr
  | Eval of expr
  | Inductive of Common.name * expr * (Common.name * expr) list
  | Option of string
