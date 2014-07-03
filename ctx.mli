
type origin =
  | User
  | Synth

(* Eliminators *)

type eliminator = 
    { t : Syntax.expr 	                    (* The type of the eliminator. *)
    ; arity : int		            (* The arity of the eliminator *)
    ; t_name : Common.name	            (* The name of the type it eliminates *)
    }

(* Signature *)

type declaration = 
  | Axiom of Syntax.expr
  | Constr of Syntax.expr * int (* all the constructors are numbered *)
  | Elim of eliminator
  | Definition of Syntax.expr * Syntax.expr

type signature

val empty_signature : signature

val lookup_ty : Common.name -> signature -> Syntax.expr

val lookup_definition :
  Common.name -> signature -> Syntax.expr option

val lookup_constr_number : Common.name -> signature -> int option

val lookup_elim :
  Common.name -> signature -> eliminator option

val lookup_elim_for_ty :
  Common.name -> signature -> Common.name option

val add_axiom : Common.name -> Syntax.expr -> origin -> signature -> signature
val add_constr : Common.name -> Syntax.expr -> int -> origin -> signature -> signature
val add_elim : Common.name -> Syntax.expr -> Common.name -> origin -> signature -> signature

val add_definition :
  Common.name -> Syntax.expr -> Syntax.expr -> origin -> signature -> signature

val mem : string -> signature -> bool

val combine : signature -> (string * declaration) list

val combine_user : signature -> (string * declaration) list

val sig_fold : ('a -> Common.name * (declaration * origin) -> 'a) -> 'a -> signature -> 'a

val is_elim : signature -> Common.name -> bool

(* Context *)

type 'a ctx
val empty_context : 'a ctx
val extend : 'a ctx -> 'a -> 'a ctx
val lookup_idx : loc:Common.position -> int -> 'a ctx -> 'a
val ctx_fold : ('a -> 'b -> 'a) -> 'a -> 'b ctx -> 'a
type cctx = (Common.variable * Syntax.expr) ctx
val index : loc:Common.position -> Common.name -> cctx -> int
val lookup_idx_ty : loc:Common.position -> int -> cctx -> Syntax.expr
val lookup_idx_name : loc:Common.position -> int -> cctx -> Common.variable


(* Both *)

val names : signature * cctx -> string list
val refresh_context : signature * cctx -> signature * cctx
