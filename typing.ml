(** Type inference. *)

open Syntax
open Ctx

(** [equal ctx e1 e2] determines whether [e1] and [e2] are equal expressions. *)
let rec equal ctx e1' e2' =
  let (e1, l1) = Norm.whnf ctx e1' in
  let (e2, l2) = Norm.whnf ctx e2' in
  Print.debug "equal e1=@[%t@]@ e2=@[%t@]" (Print.expr ctx (e1,l1)) (Print.expr ctx (e2,l2)) ;
    match e1, e2 with
      | Ann(e1', _), _ -> equal ctx e1' (e2, l2)
      | _, Ann(e2', _) -> equal ctx (e1,l1) e2'
      | Var k1, Var k2 -> k1 = k2
      | Free v1, Free v2 -> Common.eq v2 v2
      | Const x1, Const x2 -> x1 = x2
      | Type, Type -> true
      | Pi a1, Pi a2 -> equal_abstraction ctx a1 a2
      | Lambda a1, Lambda a2 -> equal_abstraction ctx a1 a2
      | App (n1, e1), App (n2, e2) -> equal ctx n1 n2 && equal ctx e1 e2
      | HEq (t1, t1', e1, e1'), HEq (t2, t2', e2, e2') ->
	equal ctx t1 t2 && equal ctx t1' t2' &&
	equal ctx e1 e2 && equal ctx e1' e2' 
      | HRefl, HRefl | HSubst, HSubst -> true
      | (Var _ | Free _ | Const _ | HEq _ | HRefl | HSubst
	    | Type | Pi _ | Lambda _ | App _ | Subst _), _ -> false

and equal_abstraction (sigma, gamma as ctx) (x, e1, e2) (_, e1', e2') =
  equal ctx e1 e1' && equal (sigma, Ctx.extend gamma (x, e1)) e2 e2'

(* TODO equality is only supported for Type 0, extend to other levels *)
let hrefl_tp = mk_pi (Common.none_with "T", mk_universe (), 
		       mk_pi (Common.none_with "t", mk_var 0,
			      mk_heq (mk_var 1) (mk_var 1) (mk_var 0) (mk_var 0)))

let hsubst_tp = mk_pi (Common.none_with "R", mk_universe (),
		       mk_pi (Common.none_with "s", mk_var 0,
			      mk_pi (Common.none_with "r", mk_var 1,
				     mk_pi (Common.none_with "eq", 
					    mk_heq (mk_var 2) (mk_var 2) (mk_var 1) (mk_var 0),
					    mk_pi (Common.none_with "P", mk_arrow (mk_var 3) (mk_universe ()),
						   mk_pi (Common.none_with "p", mk_app (mk_var 0) (mk_var 3),
							  mk_app (mk_var 1) (mk_var 3)))))))

(** [infer ctx e] infers the type of expression [e] in context [ctx]. *)
let rec infer (sigma, gamma as ctx) (e, loc) =
  match e with
    | Var k -> Ctx.lookup_idx_ty ~loc k gamma
    | Free v -> Error.violation ~loc "Cannot infer the type of a free variable"
    | Const x -> lookup_ty x sigma
    | Type -> Type, loc
    | Pi (x, e1, e2) ->
      is_universe ctx e1 ;
      is_universe (sigma, Ctx.extend gamma (x, e1)) e2 ;
      mk_universe ()
    | Subst (s, e) -> infer ctx (Syntax.subst s e)
    | Lambda (x, e1, e2) ->
      is_universe ctx e1 ;
      let t2 = infer (sigma, Ctx.extend gamma (x, e1)) e2 in
        mk_pi (x, e1, t2)
    | App (e1, e2) ->
      let (x, s, t) = infer_pi ctx e1 in
      let t2 = infer ctx e2 in
        if not (equal ctx s t2)
        then
          Error.typing ~loc:(snd e2) 
            "expresion %t has type@ %t@ but@ %t@ was expected"
            (Print.expr ctx e2) (Print.expr ctx t2) (Print.expr ctx s)
        else
          mk_subst (Dot (e2, idsubst)) t
    | Ann (e1, e2) -> 
       let t = infer ctx e1 in 
       if equal ctx t e2 then t else
	 Error.typing ~loc
           "this expresion has type@ %t@ but@ %t@ was expected(annotation)"
           (Print.expr ctx t) (Print.expr ctx e2)
    | HEq (t1, t2, e1, e2) -> 
      is_universe ctx t1 ;
      is_universe ctx t2 ;
      let t1' = infer ctx e1 in
      let t2' = infer ctx e2 in
	if (equal ctx t1 t1') && (equal ctx t2 t2') 
	then mk_universe ()
	else Error.typing ~loc "Types in equality don't match"
    | HRefl -> hrefl_tp
    | HSubst -> hsubst_tp


(** [infer_universe ctx t] infers the universe level of type [t] in context [ctx]. *)
and is_universe (sigma, gamma as ctx) t =
  let u = infer ctx t in
      match fst (Norm.whnf ctx u) with
      | Type -> ()
      | Subst _ | App _ | Var _ | Free _ | HEq _ | HRefl | HSubst
      | Pi _ | Const _ | Lambda _ | Ann _ ->
        Error.typing ~loc:(snd t) "this expression has type@ %t@ but it should be a universe" (Print.expr ctx u)

and infer_pi (sigma, gamma as ctx) e =
  let t = infer ctx e in
    match fst (Norm.whnf ctx t) with
      | Pi a -> a
      | Subst _ | App _ | Var _ | Free _ | HEq _ | HRefl | HSubst
      | Type | Const _| Lambda _ | Ann _->
        Error.typing ~loc:(snd e) "this expression has type@ %t@ but it should be a function" (Print.expr ctx t)
