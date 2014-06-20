(** Normalization of expressions. *)

open Syntax
open Su
open Ctx

(** [norm env e] evaluates expression [e] in environment [env] to a weak head normal form,
    while [norm weak:false env e] evaluates to normal form. *)
let norm ?(weak=false) =
  let rec norm (sigma, gamma as ctx) ((e', loc) as e) =
    let h_e, sp_e = split_head_spine e in
    let empty = function | [] -> true | _ -> false in
    let empty_sp = empty sp_e in
    match h_e with
    | Lambda a, loc when empty_sp ->
       Lambda (norm_abstraction ctx a), loc
    | Lambda (_, _, body), loc -> (* lambda with a spine triggers a beta reduction *)
       norm ctx (join_head_spine (norm ctx (mk_subst (Dot (List.hd sp_e, idsubst)) body)) (List.tl sp_e))
    | Const x, loc when is_elim sigma x && not empty_sp ->
       (* may trigger an iota reduction *)
       Print.debug "Try ι-red on: %t" (Print.expr ctx e) ;
       let el = Util.this(lookup_elim x sigma) in
       Util.maybe (iota ctx x sp_e el) (fun _ -> e)
    | Const x, loc ->
       begin match lookup_definition x sigma with
    	     | None -> if weak then e else 
    			 join_head_spine h_e (List.map (norm ctx) sp_e)
    	     | Some e' -> norm ctx (join_head_spine e' sp_e)
       end
    | Subst (s, e'), loc ->
       norm ctx (join_head_spine (subst s e') sp_e)
    | Var _, loc
    | Free _, loc ->
       (* Error.violation "Cannont normalize terms with free variables" *) (* TODO THIS SHOULD BE A VIOLATION *)
       let sp_e = if weak then sp_e else List.map (norm ctx) sp_e in
       join_head_spine h_e sp_e
    | Universe _, loc when empty_sp ->  e
    | Universe _, loc -> Error.violation "Unexpected non-empty spine with a Universe head."
    | Pi a, loc when empty_sp -> Pi (norm_abstraction ctx a), loc
    | Pi a, loc -> Error.violation "Unexpected non-empty spine with a Pi head."
    | App _, _ -> Error.violation "Unexpected, head cannot be an application."
    | Ann (e, t), loc when empty_sp -> Ann(norm ctx e, norm ctx t), loc
    | Ann (e, t), loc -> norm ctx (join_head_spine e sp_e) (* removes annotation *)
			      
  and norm_abstraction (sigma, gamma as ctx) ((x, t, e) as a) =
    if weak then a
    else (x, norm ctx t, norm (sigma, extend gamma (x, t)) e)

  and iota (sigma, gamma as ctx) const sp el =
    Print.debug "ι-red on constructor %s and spine [%t]" const (Print.sequence ~sep:" ;" (fun e -> Print.expr ctx e) sp);
    let elim = Util.this (lookup_elim_for_ty el.t_name sigma) in (* TODO MOVE TO THE THE ELIM RECORD *)
    let rec recs p mvec di =
      Print.debug "Called recs";
      Print.debug "Called recs on di:[%t](1)" (Print.sequence ~sep:" ;" (fun (e,_) -> Print.expr ctx e) di);
      Print.debug "Called recs on di:[%t](2)" (Print.sequence ~sep:" ;" (fun (_,e) -> Print.expr ctx e) di);
      match di with
      | [] -> []
      | (r, t)::es when produces_constr ctx el.t_name t ->
    	 Print.debug "Recs: recursive %t with type %t" (Print.expr ctx r) (Print.expr ctx t);
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
      let x = norm ctx (List.nth sp 0) in	(* HACK ALERT *)
      let x_hd, x_sp = split_head_spine x in	(* c, Δᵢ *)
      Print.debug "Scrutinee x = %t @ head x_hd = %t@ x_sp = @[[%t]@]"
    		  (Print.expr ctx x) (Print.expr ctx x_hd) (Print.sequence ~sep:" ;" (Print.expr ctx) x_sp) ;
      begin match x_hd with
    	    | Const x_name, _ ->
    	       let sp_types = let tel,_ = get_telescope (lookup_ty x_name sigma) in List.map snd tel in
    	       Print.debug "sp_types = @[[%t]@]" (Print.sequence ~sep:" ;" (Print.expr ctx) sp_types) ;
    	       let p = List.nth sp 1 in
    	       Print.debug "p = %t" (Print.expr ctx p) ;
    	       let mvec = List.tl (List. tl sp) in
    	       Print.debug "mvec = %t" (Print.sequence ~sep:" ;" (fun e -> Print.expr ctx e) mvec) ;
    	       let rs = recs p mvec (Util.zip x_sp sp_types) in
    	       let i = Util.this (lookup_constr_number x_name sigma) in
    	       let mi = List.nth mvec i in
    	       Print.debug "mi = %t" (Print.expr ctx mi) ;
    	       let res = norm ctx (join_head_spine mi (x_sp @ rs)) in
	       Print.debug "ι-red : * ~~> %t" (Print.expr ctx res) ;
	       Some res
    	    | _ -> Print.debug "ι-red stuck on %t (scrutinee does not reduce)" (Print.expr ctx x) ;
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

