(** Signature management *)

type origin =
  | User
  | Synth

(** The signature is represented as an associative list which maps a variable [x] to a pair
   [(t,e)] where [t] is its type and [e] is its value (optional).
*)
type eliminator = 
    { t : Syntax.expr 	                    (* The type of the eliminator. *)
    ; arity : int		            (* The arity of the eliminator *)
    ; t_name : Common.name	            (* The name of the type it eliminates *)
    }

(** The entries in the signature are declarations of parameters or definitions.
    A parameter declaration carries its type, while a definition carries the type and
    the defining expression. *)
type declaration = 
  | Axiom of Syntax.expr
  | Constr of Syntax.expr * int (* all the constructors are numbered *)
  | Elim of eliminator
  | Definition of Syntax.expr * Syntax.expr

(** A signature consists of a list of names, declarations and an origin.
    The origin says whether the entry was synthesized or not.*)

type signature = (Common.name * (declaration * origin)) list

(** On the zeroth day there was the empty signature. *)
let empty_signature = []
    
(** [lookup_ty k ctx] returns the type of [Var k] in signature [ctx]. *)
let lookup_ty x sigma =
  match fst(List.assoc x sigma) with
    | Axiom t| Constr (t,_) | Definition (t, _) -> t
    | Elim el -> el.t

(** [lookup_definition k ctx] returns the definition of [Var k] in signature [ctx]. *)
let lookup_definition x sigma = 
  match fst(List.assoc x sigma) with
    | Definition (_, e) -> Some e
    | Axiom _ | Constr _  | Elim _-> None

let lookup_constr_number x sigma = 
  match fst(List.assoc x sigma) with
  | Constr (_, n) -> Some n
  | _ -> None

let lookup_elim x sigma = 
  match fst(List.assoc x sigma) with
    | Elim el -> Some el
    | Axiom _ | Constr _  | Definition _ -> None

(** Looks up the name of the eliminator for a given type *)
let rec lookup_elim_for_ty d = function
  | [] -> None
  | (x, (Elim el, _))::_ when d = el.t_name -> Some x
  | _::rest -> lookup_elim_for_ty d rest


(** [add_parameter x t ctx] returns [ctx] with the parameter [x] of type [t]. *)
let add_axiom x t origin ctx = (x, (Axiom t, origin)) :: ctx
let add_constr x t n origin ctx = (x, (Constr (t, n), origin)) :: ctx
let add_elim x t d origin ctx = 
  let rec tp_arity n = function 
    | Syntax.Pi (_,_, e), l -> tp_arity (n+1) e
    | _ -> n
  in
  let n = tp_arity 0 t in
  (x, (Elim {t = t ; arity = n ; t_name = d}, origin)) :: ctx

(** [add_definition x t e ctx] returns [ctx] with [x] of type [t] defined as [e]. *)
let add_definition x t e origin ctx = (x, (Definition (t, e), origin)) :: ctx

let combine sigma = 
  List.map (fun (n, (d, _)) -> (n, d)) sigma

let combine_user sigma = 
  combine (List.filter (fun (_, (_, origin)) -> origin = User) sigma)

let mem = List.mem_assoc

let sig_fold = List.fold_left

let is_elim sigma x =
  match fst(List.assoc x sigma) with
  | Elim _ -> true
  | _ -> false

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
  | Empty, _ -> Error.violation ~loc "unknown index" (* Is this a violation? I think it is *)
  | Cons (gamma, e), 0 -> e
  | Cons (gamma, _), k -> lookup_idx ~loc (k - 1) gamma

type cctx = (Common.variable * Syntax.expr) ctx

(** [index ~loc x gamma] finds the location of x in the cctx gamma. *)
let index ~loc x =
  let rec index k = function
    | Empty                -> Error.scoping ~loc "unknown identifier %s" x
    | Cons (gamma, (y, _)) -> 
       match Common.get_name y with 
	 Some y when x = y -> k
       | _ -> index (k + 1) gamma
  in
  index 0

let rec lookup_idx_ty ~loc k gamma = Syntax.shift (k+1) (snd (lookup_idx ~loc k gamma))

let rec lookup_idx_name ~loc k gamma = (fst (lookup_idx ~loc k gamma))

(** Signature and context functions *)

let names (sigma, gamma : signature * cctx) : string list = 
  (List.map fst sigma) @ 
       (ctx_fold 
	  (fun ns (n,_) -> match Common.get_name n with | Some n -> n::ns |_ -> ns)
	  [] gamma)

let refresh_context (sigma, gamma) =
  let rec refresh_context = function 
    | Empty -> Empty, []
    | Cons(gamma, (x, t)) -> 
       let gamma', sofar = refresh_context gamma in
       let x' = Beautify.refresh x sofar in
       Cons(gamma' , (Common.some x', t)), x'::sofar
  in
  (sigma, fst(refresh_context gamma))
