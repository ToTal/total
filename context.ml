(** Signature management *)

(** The signature is represented as an associative list which maps a variable [x] to a pair
   [(t,e)] where [t] is its type and [e] is its value (optional).
*)

(** The entries in the signature are declarations of parameters or definitions.
    A parameter declaration carries its type, while a definition carries the type and
    the defining expression. *)
type declaration = 
  | Axiom of Syntax.expr
  | Constr of Syntax.expr
  | Definition of Syntax.expr * Syntax.expr

(** A signature consists of a list of names, used for pretty-printing and
    desugaring of variable names to de Bruijn indices, and a list of
    declarations. *)
type signature = (Common.variable * declaration) list

(** On the zeroth day there was the empty signature. *)
let empty_signature = []
    
(** [lookup_ty k ctx] returns the type of [Var k] in signature [ctx]. *)
let lookup_ty x sigma =
  match List.assoc x sigma with
    | Axiom t | Constr t | Definition (t, _) -> t

(** [lookup_definition k ctx] returns the definition of [Var k] in signature [ctx]. *)
let rec lookup_definition x sigma = 
  match List.assoc x sigma with
    | Definition (_, e) -> Some e
    | Axiom _ | Constr _ -> None

(** [add_parameter x t ctx] returns [ctx] with the parameter [x] of type [t]. *)
let add_axiom x t ctx = (x, Axiom t) :: ctx
let add_constr x t ctx = (x, Constr t) :: ctx

(** [add_definition x t e ctx] returns [ctx] with [x] of type [t] defined as [e]. *)
let add_definition x t e ctx = (x, Definition (t, e)) :: ctx

let combine sigma = sigma 

let mem = List.mem_assoc

(** Local context management *)

type 'a ctx =
  | Empty
  | Cons of 'a ctx * 'a

let empty_context = Empty

(** Left fold on contexts *)
let rec ctx_fold f b = function
  | Empty -> b
  | Cons (gamma, x) -> f (ctx_fold f b gamma) x

let extend ctx x = Cons (ctx, x)

let rec lookup_idx ~loc k gamma = 
  match gamma, k with
  | Empty, _ -> Error.scoping ~loc "unknown index" (* Is this a violation? I think it is *)
  | Cons (gamma, e), 0 -> e
  | Cons (gamma, _), k -> lookup_idx ~loc (k - 1) gamma

type cctx = (Common.variable * Syntax.expr) ctx

(** [index ~loc x gamma] finds the location of x in the cctx gamma. *)
let index ~loc x =
  let rec index k = function
    | Empty                -> Error.scoping ~loc "unknown identifier %s" x
    | Cons (gamma, (y, _)) -> if x = y then k else index (k + 1) gamma
  in
  index 0

let rec lookup_idx_ty ~loc k gamma = Syntax.shift (k+1) (snd (lookup_idx ~loc k gamma))

let rec lookup_idx_name ~loc k gamma = fst (lookup_idx ~loc k gamma)

(** Signature and context functions *)

let names (sigma, gamma) = (List.map fst sigma) @ (ctx_fold (fun ns (n,_) -> n::ns) [] gamma)
