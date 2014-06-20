open Syntax
open Su

let rec is_kind c = function 
  | Universe _, _ -> true
  | Pi (_,_, e),_ -> is_kind c e
  | Subst _ as e, l -> is_kind c (Norm.whnf c (e, l))
  | _, _ -> false

let ctx_from sigma = (sigma, Ctx.empty_context)

let elab_type_constr sigma x t =
  let ctx = ctx_from sigma in
  let _ = Typing.infer ctx t in
  if not (is_kind ctx (Norm.whnf ctx t)) then
    Error.typing ~loc:(snd t) "expresion @ %t@ is not a kind" (Print.expr ctx t) ;
  Ctx.add_constr x t 0 sigma 	(* type constructor is number 0 *)

let constructs_type x t = 
  let _tel, body = get_telescope t in
  let h, _sp = split_head_spine body in
  match h with 
  | Const x',_  -> x = x'
  | _ -> false

let positive ctx t x = 
  let rec appears = function
    | Const x', _ -> x = x'
    | Var _, _ | Free _, _ | Universe _, _ -> false
    | Subst (s, e), _ -> appears e || sapp s
    | Pi (_, t, e), _ | Lambda (_, t, e), _ -> appears t || appears e
    | App (e1, e2),_ | Ann (e1,e2),_ -> appears e1 || appears e2
  and sapp = function
    | Shift _ -> false
    | Dot (e, s) -> appears e || sapp s
  in
  let appears_on_the_left = function
    | Pi(_,t,_), _ -> appears t
    | _ -> false
  in
  let t_tel, _ = get_telescope t in
  List.fold_left 
    (fun res (_, t) -> res && not (appears_on_the_left t)) 
    true t_tel

let validate_constrs (sigma : Ctx.signature) 
		 (x : Common.name) 
		 (t : Syntax.expr) 
		 (cs : (Common.name * Syntax.expr) list) =
  let elab (c, t) (sigma, n) = 
    let ctx = ctx_from sigma in
    if not (constructs_type x t) then
      Error.typing ~loc:(snd t) 
		   "constructor %s does not construct type %s (%t )" 
		   c x (Print.expr ctx t) ;
    let k = Typing.infer ctx t in
    if not (is_kind ctx k)then
      Error.typing ~loc:(snd t)
    		   "constructor %s does not construct a type (%t has type %t)"
    		   c (Print.expr ctx t) (Print.expr ctx k) ;
    if !Config.check_positivity then
      if not (positive ctx t x) then
	Error.typing ~loc:(snd t) "constructor %s is not strictly positive." c;
    (Ctx.add_constr c t n sigma, n+1)
  in
  fst(List.fold_right elab (List.rev cs) (sigma, 0))

(** Computes the induction hypothesis *)
let motive_ty sigma d t = 
  let ctx = ctx_from sigma in
  Print.debug "Building motive for type %s : %t" d (Print.expr ctx t);
  let params,_ = get_telescope t in
  let d' = join_head_spine (mk_const d) (List.map (fun (x, _) -> var x) params) in

  let p_tel = params @ [(Common.none_with "D", d')] in
  Print.debug "Telescope for P : [%t]" (Print.tele ctx p_tel) ;

  let p = set_telescope p_tel (mk_universe 0) (fun x t e -> mk_pi(x, t, e)) in
  Print.debug "Motive for %s is P : %t" d (Print.expr ctx p) ;
  p

let method_ty sigma d t c ct p_nm = 
  let rec constructor_params_for p = function
    | App (e1, e2), l -> App(constructor_params_for p e1, e2), l
    | Const _, l -> (fst p), l
    | _ -> Error.violation "This is not happening"
  in
  let ctx = ctx_from sigma in
  Print.debug "Computing method : %s" c ;
  (* The term that contains P *)
  let p = var p_nm in
  
  (* All the constructor's parameters *)
  let constr_tel, constr = get_telescope ct in
  Print.debug "constr_tel = %t" (Print.tele ctx constr_tel) ;

  (* The parameters that represent a recursive call *)
  let recs = List.filter (fun (x, t) -> produces_constr ctx d t) constr_tel in

  Print.debug "t = %t" (Print.expr ctx t) ;
  Print.debug "d = %s" d ;
  Print.debug "recs = %t" (Print.tele ctx recs) ;

  let hyps = List.map 
	       (fun (x, t) -> 
		Common.none_with "r", 
		let t_tel, t_body = get_telescope t in
		Print.debug "t_tel = %t" (Print.tele ctx t_tel) ;
		Print.debug "t_body = %t" (Print.expr ctx t_body) ;
		let r_app = join_head_spine (var x) (List.map (fun (y,_) -> var y) t_tel) in
		set_telescope 
			      t_tel
			      (mk_app (constructor_params_for p t_body) r_app)
			      (fun x t e -> mk_pi(x,t,e)))
	       recs
  in
  let final_tel = constr_tel @ hyps (* @ constr_tel *) in 

  (* p with argments up to D applied *) 
  let p' = constructor_params_for p constr in
  Print.debug "What I want to know is: %t" (Print.expr ctx constr) ;

  let result = mk_app p' (join_head_spine (mk_const c) (List.map (fun (x, _) -> var x) constr_tel)) in

  let m = set_telescope final_tel result (fun v t e -> mk_pi (v, t, e)) in
  Print.debug "For %s method: %t" c (Print.expr ctx m) ;
  m

let elim sigma d t cs = 
  let ctx = ctx_from sigma in
  Print.debug "Computing eliminator for %s" d ;
  let targets, _ = get_telescope t in
  Print.debug "targets = %t" (Print.tele ctx targets) ;
  let x = Common.none_with "x" in

  let p_nm = Common.none_with "P" in
  let p = motive_ty sigma d t in

  let ms = List.map
  	     (fun (c, ct) ->
  	      Common.none_with "m", method_ty sigma d t c ct p_nm) 
	     cs

  in

  let x_dest = join_head_spine (mk_const d) (List.map (fun (x, _) -> var x) targets) in

  let final_tel = targets @ [(x, x_dest) ; (p_nm, p)] @ ms in

  let result = join_head_spine (var p_nm) (List.map (fun (x,_) -> var x) (targets @ [(x, x_dest)])) in

  Print.debug "Eliminator telescope length: %d" (List.length final_tel) ;
  Print.debug "Eliminator telescope: %t" (Print.tele ctx final_tel) ;
  Print.debug "result = %t" (Print.expr ctx result) ;

  let elim_ty = set_telescope final_tel result (fun v t e -> mk_pi (v, t, e)) in

  Print.debug "Final eliminator = %t" (Print.expr ctx elim_ty) ;

  let kind = Typing.infer ctx elim_ty in
  if not (is_kind ctx (Norm.whnf ctx kind)) then
    Error.violation 
      "expresion @ %t@  in eliminator is not a kind @ %t@ (inductive.ml)"
      (Print.expr ctx elim_ty) (Print.expr ctx kind);
  
  Ctx.add_elim (d^"-elim") elim_ty d sigma


