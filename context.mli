(* Signature *)

type declaration = 
  | Axiom of Syntax.expr
  | Constr of Syntax.expr
  | Definition of Syntax.expr * Syntax.expr

type signature

val empty_signature : signature

val lookup_ty : Common.variable -> signature -> Syntax.expr

val lookup_definition :
  Common.variable -> signature -> Syntax.expr option

val add_axiom : string -> Syntax.expr -> signature -> signature
val add_constr : string -> Syntax.expr -> signature -> signature

val add_definition :
  string -> Syntax.expr -> Syntax.expr -> signature -> signature

val mem : string -> signature -> bool

val combine : signature -> (string * declaration) list

val sig_fold : ('a -> Common.variable * declaration -> 'a) -> 'a -> signature -> 'a

(* Context *)

type 'a ctx
val empty_context : 'a ctx
val extend : 'a ctx -> 'a -> 'a ctx
val lookup_idx : loc:Common.position -> int -> 'a ctx -> 'a
val ctx_fold : ('a -> 'b -> 'a) -> 'a -> 'b ctx -> 'a
type cctx = (Common.variable * Syntax.expr) ctx
val index : loc:Common.position -> string -> (string * 'a) ctx -> int
val lookup_idx_ty : loc:Common.position -> int -> cctx -> Syntax.expr
val lookup_idx_name : loc:Common.position -> int -> cctx -> Common.variable

(* Both *)

val names : signature * cctx -> string list
