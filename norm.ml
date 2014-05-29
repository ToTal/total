(** Normalization of expressions. *)

open Syntax
open Ctx

(** [norm env e] evaluates expression [e] in environment [env] to a weak head normal form,
    while [norm weak:false env e] evaluates to normal form. *)
let norm ?(weak=false) =
  let rec norm (sigma, gamma as ctx) ((e', loc) as e) =
    match e' with
      | Var k -> e
      | Free _ -> e		
      | Const x ->
        (match lookup_definition x sigma with
          | None -> e
          | Some e -> norm ctx e)
      | Universe _ -> e
      | Pi a -> 
        Pi (norm_abstraction ctx a), loc
      | Lambda a -> Lambda (norm_abstraction ctx a), loc
      | Subst (s, e) -> norm ctx (subst s e)
      | Ann (e1, e2) -> 
	 Ann (norm ctx e1, norm ctx e2), loc
      | App (e1, e2) ->
        let (e1', _) as e1 = norm ctx e1 in
          (match e1' with
            | Lambda (x, t, e) -> norm ctx (mk_subst (Dot (e2, idsubst)) e) (* Beta - reduction *)
	    | Const x when is_elim sigma x -> iota ctx (App (e1, e2), loc)  (* Iota - reduction *)
            | Var _ | Free _ | Const _ | App _ -> 
              let e2 = (if weak then e2 else norm ctx e2) in 
                App (e1, e2), loc
	    (* This removes the annotation, is this correct? *)
	    | Ann (e1, t) -> norm ctx (App (e1, e2), loc) 
            | Subst _ | Universe _ | Pi _ -> Error.runtime ~loc:(snd e1) "Function expected")

  and norm_abstraction (sigma, gamma as ctx) ((x, t, e) as a) =
    if weak
    then a
    else (x, norm ctx t, norm (sigma, extend gamma (x, t)) e)
  
  and iota (sigma, gamma as ctx) e =
    assert false
    (* let h, sp = split_head_spine e in *)
    (* let x = (match norm ctx h with *)
    (* | Const x, _ -> (if (is_elim sigma x) then x else Error.violation "iota reduction called in something that is not an eliminator")  *)
    (* | _ -> Error.violation "iota reudction does not contain an eliminator in its head")  *)
    (* in *)
    (* let t, n = match (lookup_elim x sigma) with Some (t, n) -> t, n | None -> Error.violation "this cannot happen" in *)
    (* if n = List.length sp then *)
    (*   (\* continue checking if it reduces *\) *)
    (*   assert false *)
    (* else *)
    (*   Print.debug "n= %d | len(sp) = %d[[%t]]" n (List.length sp) (Print.expr ctx e) ; *)
    (*   e *)

  in
    norm

(** [nf ctx e] computes the normal form of expression [e]. *)
let nf ctx e = norm ~weak:false ctx e

(** [whnf ctx e] computes the weak head normal form of expression [e]. *)
let whnf ctx e = norm ~weak:true ctx e

