(** Type inference. *)

open Syntax
open Context

(** [equal ctx e1 e2] determines whether [e1] and [e2] are equal expressions. *)
let rec equal ctx e1 e2 =
  let (e1, _) = Norm.whnf ctx e1 in
  let (e2, _) = Norm.whnf ctx e2 in
    match e1, e2 with
      | Var k1, Var k2 -> k1 = k2
      | Const x1, Const x2 -> x1 = x2
      | Universe u1, Universe u2 -> u1 = u2
      | Pi a1, Pi a2 -> equal_abstraction ctx a1 a2
      | Lambda a1, Lambda a2 -> equal_abstraction ctx a1 a2
      | App (n1, e1), App (n2, e2) -> equal ctx n1 n2 && equal ctx e1 e2
      | (Var _ | Const _ | Universe _ | Pi _ | Lambda _ | App _ | Subst _), _ -> false

and equal_abstraction (sigma, gamma as ctx) (x, e1, e2) (_, e1', e2') =
  equal ctx e1 e1' && equal (sigma, Context.extend gamma (x, e1)) e2 e2'

(** [infer ctx e] infers the type of expression [e] in context [ctx]. *)
let rec infer (sigma, gamma as ctx) (e, loc) =
  match e with
    | Var k -> Context.lookup_idx_ty ~loc k gamma
    | Const x -> lookup_ty x sigma
    | Universe u -> mk_universe (u + 1)
    | Pi (x, e1, e2) ->
      let u1 = infer_universe ctx e1 in
      let u2 = infer_universe (sigma, Context.extend gamma (x, e1)) e2 in
        mk_universe (max u1 u2)
    | Subst (s, e) -> infer ctx (Syntax.subst s e)
    | Lambda (x, e1, e2) ->
      let _ = infer_universe ctx e1 in
      let t2 = infer (sigma, Context.extend gamma (x, e1)) e2 in
        mk_pi (x, e1, t2)
    | App (e1, e2) ->
      let (x, s, t) = infer_pi ctx e1 in
      let t2 = infer ctx e2 in
        if not (equal ctx s t2)
        then
          Error.typing ~loc:(snd e2) 
            "this expresion has type@ %t@ but@ %t@ was expected"
            (Print.expr ctx t2) (Print.expr ctx s)
        else
          mk_subst (Dot (e2, idsubst)) t

(** [infer_universe ctx t] infers the universe level of type [t] in context [ctx]. *)
and infer_universe (sigma, gamma as ctx) t =
  let u = infer ctx t in
    match fst (Norm.whnf ctx u) with
      | Universe u -> u
      | Subst _ | App _ | Var _ | Pi _ |Const _ | Lambda _ ->
        Error.typing ~loc:(snd t) "this expression has type@ %t@ but it should be a universe" (Print.expr ctx u)

and infer_pi (sigma, gamma as ctx) e =
  let t = infer ctx e in
    match fst (Norm.whnf ctx t) with
      | Pi a -> a
      | Subst _ | Var _ | Const _ | App _ | Universe _ | Lambda _ ->
        Error.typing ~loc:(snd e) "this expression has type@ %t@ but it should be a function" (Print.expr ctx t)
