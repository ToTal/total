(** Normalization of expressions. *)

open Syntax
open Su
open Ctx

(** [norm env e] evaluates expression [e] in environment [env] to a weak head normal form,
    while [norm weak:false env e] evaluates to normal form. *)
let norm ?(weak=false) sigma =
  let faux_ctx = (sigma, empty_context) in
  let rec norm ((e', loc) as e) =
    let h_e, sp_e = split_head_spine e in
    let empty = function | [] -> true | _ -> false in
    let empty_sp = empty sp_e in
    match h_e with
    | LambdaAnn a, loc when empty_sp ->
       LambdaAnn (norm_ann_abstraction a), loc
    | Lambda a, loc when empty_sp ->
       Lambda (norm_abstraction a), loc
    | LambdaAnn (_, _, body), loc -> (* lambda with a spine triggers a beta reduction *)
       norm (join_head_spine (norm (mk_subst (Dot (List.hd sp_e, idsubst)) body)) (List.tl sp_e))
    | Lambda (_, body), loc -> (* lambda with a spine triggers a beta reduction *)
       norm (join_head_spine (norm (mk_subst (Dot (List.hd sp_e, idsubst)) body)) (List.tl sp_e))
    | Const x, loc when is_elim sigma x && not empty_sp ->
       (* may trigger an iota reduction *)
       Print.debug "Try ι-red on: %t" (Print.expr faux_ctx e) ;
       let el = Util.this(lookup_elim x sigma) in
       Util.maybe (iota x sp_e el) (fun _ -> e)
    | Const x, loc ->
       begin match lookup_definition x sigma with
    	     | None -> if weak then e else 
    			 join_head_spine h_e (List.map norm sp_e)
    	     | Some e' -> norm (join_head_spine e' sp_e)
       end
    | Subst (s, e'), loc ->
       norm (join_head_spine (subst s e') sp_e)
    | Var _, loc -> 
       let sp_e = if weak then sp_e else List.map norm sp_e in
       join_head_spine h_e sp_e
    | Free _, loc ->
       (* Error.violation "Cannont normalize terms with free variables" *) (* TODO THIS SHOULD BE A VIOLATION *)
       let sp_e = if weak then sp_e else List.map norm sp_e in
       join_head_spine h_e sp_e
    | Type, loc when empty_sp ->  e
    | Type, loc -> Error.violation "Unexpected non-empty spine with a Universe head."
    | Pi a, loc when empty_sp -> Pi (norm_ann_abstraction a), loc
    | Pi a, loc -> Error.violation "Unexpected non-empty spine with a Pi head."
    | App _, _ -> Error.violation "Unexpected, head cannot be an application."
    | Ann (e, t), loc when empty_sp -> Ann(norm e, norm t), loc
    | Ann (e, t), loc -> norm (join_head_spine e sp_e) (* removes annotation *)
    | HEq (t1, t2, e1, e2), loc -> 
      HEq (norm t1, norm t2, norm e1, norm e2), loc
    | HRefl, loc -> HRefl, loc
    | HSubst, loc -> Util.maybe (hsubst sp_e) (fun _ -> e)
  
  and norm_ann_abstraction ((x, t, e) as a) =
    if weak then a
    else (x, norm t, norm e)

  and norm_abstraction ((x, e) as a) =
    if weak then a
    else (x, norm e)

  and hsubst sp =
    let hsubst_arity = 6 in
    let sp_len = List.length sp in
    if sp_len = hsubst_arity then
      let q = norm (List.nth sp 3) in 		(* get the proof of equality *)
      match split_head_spine q with
	| (HRefl, _), _ -> Some(List.nth sp 5)          (* perform the reduction *)
	| _, _ -> None
    else None
  and iota const sp el =
    Print.debug "ι-red on constructor %s and spine [%t]" const (Print.sequence ~sep:" ;" (fun e -> Print.expr faux_ctx e) sp);
    let elim = Util.this (lookup_elim_for_ty el.t_name sigma) in (* TODO MOVE TO THE THE ELIM RECORD *)
    let rec recs p mvec di =
      Print.debug "Called recs";
      Print.debug "Called recs on di:[%t](1)" (Print.sequence ~sep:" ;" (fun (e,_) -> Print.expr faux_ctx e) di);
      Print.debug "Called recs on di:[%t](2)" (Print.sequence ~sep:" ;" (fun (_,e) -> Print.expr faux_ctx e) di);
      match di with
      | [] -> []
      | (r, t)::es when produces_constr el.t_name t ->
    	 Print.debug "Recs: recursive %t with type %t" (Print.expr faux_ctx r) (Print.expr faux_ctx t);
	 let t_tel, e_body = get_telescope t in
	 let r' = join_head_spine r (List.map (fun (x, _) -> var x) t_tel) in
    	 (set_telescope 
			t_tel 
			(join_head_spine (mk_const elim) (r'::p::mvec)) 
			(fun x t e -> mk_lambda(x,t,e))):: recs p mvec es
      | _::es -> recs p mvec es
    in
    let sp_len = List.length sp in
    let delim = Util.this (lookup_elim const sigma) in
    if delim.arity = sp_len then
      let x = norm (List.nth sp 0) in	(* HACK ALERT *)
      let x_hd, x_sp = split_head_spine x in	(* c, Δᵢ *)
      Print.debug "Scrutinee x = %t @ head x_hd = %t@ x_sp = @[[%t]@]"
    		  (Print.expr faux_ctx x) (Print.expr faux_ctx x_hd) (Print.sequence ~sep:" ;" (Print.expr faux_ctx) x_sp) ;
      begin match x_hd with
    	    | Const x_name, _ ->
    	       let sp_types = let tel,_ = get_telescope (lookup_ty x_name sigma) in List.map snd tel in
    	       Print.debug "sp_types = @[[%t]@]" (Print.sequence ~sep:" ;" (Print.expr faux_ctx) sp_types) ;
    	       let p = List.nth sp 1 in
    	       Print.debug "p = %t" (Print.expr faux_ctx p) ;
    	       let mvec = List.tl (List. tl sp) in
    	       Print.debug "mvec = %t" (Print.sequence ~sep:" ;" (fun e -> Print.expr faux_ctx e) mvec) ;
    	       let rs = recs p mvec (Util.zip x_sp sp_types) in
    	       let i = Util.this (lookup_constr_number x_name sigma) in
    	       let mi = List.nth mvec i in
    	       Print.debug "mi = %t" (Print.expr faux_ctx mi) ;
    	       let res = norm (join_head_spine mi (x_sp @ rs)) in
	       Print.debug "ι-red : * ~~> %t" (Print.expr faux_ctx res) ;
	       Some res
    	    | _ -> Print.debug "ι-red stuck on %t (scrutinee does not reduce)" (Print.expr faux_ctx x) ;
    		   None
      end
    else
      (Print.debug "ι-red is stuck (more parameters required)" ;
       None)
  in
  norm


(** [nf ctx e] computes the normal form of expression [e]. *)
let nf ctx e = norm ~weak:false ctx e

(** [whnf ctx e] computes the weak head normal form of expression [e]. *)
let whnf ctx e = norm ~weak:true ctx e

