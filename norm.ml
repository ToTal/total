(** Normalization of expressions. *)

open Syntax
open Su
open Ctx

let rec head_is_elim sigma = function
  | App (e1, e2),l -> head_is_elim sigma e1
  | Const x, l -> is_elim sigma x
  | _ -> false

let empty = function | [] -> true | _ -> false

(** [norm env e] evaluates expression [e] in environment [env] to a weak head normal form,
    while [norm weak:false env e] evaluates to normal form. *)
let norm ?(weak=false) =
  let rec norm (sigma, gamma as ctx) ((e', loc) as e) =
    let h_e, sp_e = split_head_spine ctx e in
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

  and iota (sigma, gamma as _ctx) const sp el = None
    (* Print.debug "ι-red on constructor %s and spine [%t]" const (Print.sequence ~sep:" ;" (fun e -> Print.expr ctx e) sp); *)
    (* let elim = Util.this (lookup_elim_for_ty el.t_name sigma) in (\* TODO MOVE THE THE ELIM RECORD *\) *)
    (* let rec recs p mvec di = *)
    (*   Print.debug "Called recs"; *)
    (*   Print.debug "Called recs on di:[%t](1)" (Print.sequence ~sep:" ;" (fun (e,_) -> Print.expr ctx e) di); *)
    (*   Print.debug "Called recs on di:[%t](2)" (Print.sequence ~sep:" ;" (fun (_,e) -> Print.expr ctx e) di); *)
    (*   match di with *)
    (*   | [] -> [] *)
    (*   | (e, t)::es when is_constr ctx el.t_name t -> *)
    (* 	 Print.debug "Recs: recursive %t" (Print.expr ctx e) ; *)
    (* 	 (join_head_spine (mk_const elim) (e::p::mvec)):: recs p mvec es *)
    (*   | _::es -> recs p mvec es *)
    (* in *)
    (* let sp_len = List.length sp in *)
    (* let delim = Util.this (lookup_elim const sigma) in *)
    (* if delim.arity == sp_len then *)
    (*   let x = List.nth sp 0 in	(\* HACK ALERT *\) *)
    (*   begin match norm ctx x with *)
    (* 	    | Const x_name, _ -> *)
    (* 	       let sp_types = let tel,_ = get_telescope ctx (lookup_ty x_name sigma) in List.map snd tel in *)
    (* 	       let p = List.nth sp 1 in *)
    (* 	       let mvec = List.tl (List. tl sp) in *)
    (* 	       let rs = recs p mvec (Util.zip sp sp_types) in *)
    (* 	       let i = Util.this (lookup_constr_number x_name sigma) in *)
    (* 	       let mi = List.nth mvec i in *)
    (* 	       Some (join_head_spine mi (sp @ rs)) *)
    (* 	    | _ -> Print.debug "ι reduction stuck on %t (scrutinee does not reduce)" (Print.expr ctx x) ; *)
    (* 		   None *)
    (*   end  *)
    (* else *)
    (*   (Print.debug "ι reduction is stuck (more parameters required)" ; *)
    (*    None) *)
  in
  (fun ctx e -> 
   let e' = norm ctx e in
   Print.debug "Normalization @[%t@]@ ~~> @[%t@]" (Print.expr ctx e)  (Print.expr ctx e') ;
   e')

(* The old normal *)
(*
let old_norm ?(weak=false) =
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
	   (* Beta - reduction *)
            | Lambda (x, t, e) -> norm ctx (mk_subst (Dot (e2, idsubst)) e)
	    (* Iota - reduction *)
	    | App _ when head_is_elim sigma e1 -> 
	       let (Const x, _), _ = split_head_spine ctx e1 in
	       let delim = Util.this (lookup_elim x sigma) in
	       iota ctx (App (e1, e2), loc) delim.t delim.t_name
	    | Const x when is_elim sigma x -> 
	       let delim = Util.this(lookup_elim x sigma) in
	       iota ctx (App (e1, e2), loc) delim.t delim.t_name

            | Var _ | Free _ | Const _ | App _ -> 
              let e2 = (if weak then e2 else norm ctx e2) in 
                App (e1, e2), loc
	    (* This removes the annotation, is this correct? *)
	    | Ann (e1, t) -> norm ctx (App (e1, e2), loc) 
            | Subst _ | Universe _ | Pi _ -> 
	      Error.runtime ~loc:(snd e1) 
			    "Function expected got a %t"
			    (Print.expr ctx e1))

  and norm_abstraction (sigma, gamma as ctx) ((x, t, e) as a) =
    if weak
    then a
    else (x, norm ctx t, norm (sigma, extend gamma (x, t)) e)
  
  and iota (sigma, gamma as ctx) e delim d = 
    Print.debug "ι-reduction of : %t" (Print.expr ctx e) ;
    let rec recs p mvec =
      Print.debug "Called recs";
      function
      | [] -> []
      | (e, t)::es when is_constr ctx d t ->
    	 Print.debug "Recs: recursive %t" (Print.expr ctx e) ;
    	 let elim = Util.this (lookup_elim_for_ty d sigma) in
    	 (join_head_spine (mk_const elim) (e::p::mvec)):: recs p mvec es
      | _::es -> recs p mvec es
    in
    let h, sp = split_head_spine ctx e in
    Print.debug "sp = [%t]" (Print.sequence ~sep:" ;" (fun e -> Print.expr ctx e)  sp  ) ;
    let x = (match norm ctx h with
    | Const x, _ ->
       (if (is_elim sigma x) then x
    	else Error.violation "iota reduction called in something that is not an eliminator")
    | _ -> Error.violation "iota reudction does not contain an eliminator in its head")
    in
    let t, n = match (lookup_elim x sigma) with
      |	Some el -> el.t, el.arity
      | None -> Error.violation "this cannot happen" in
    if n = List.length sp then
      (* the head of the spine contains the target *)
      (Print.debug "Target: %t" (Print.expr ctx (List.hd (List.rev sp))) ;
      let t_hd, t_sp = split_head_spine ctx (norm ctx (List.hd (List.rev sp))) in
      Print.debug "Target head: %t" (Print.expr ctx t_hd) ;
      Print.debug "Target spine: [%t]" (Print.sequence ~sep:" ;" (fun e -> Print.expr ctx e) t_sp);
      (match t_hd with
       (* the normal form of the target is constructor *)
       | Const x, l ->
    	  let t_sp_types = let tel,_ = get_telescope ctx (lookup_ty x sigma) in List.map snd tel in
    	  let p, mvec =
    	    (match List.rev sp  with
    	     | _::p:: mvec -> p, mvec | _ -> Error.violation "The spine is too short!")
    	  in
    	  Print.debug "The motive P = %t" (Print.expr ctx p) ;
    	  Print.debug "mvec = %t" (Print.sequence ~sep:" ;" (fun e -> Print.expr ctx e) mvec) ;
    	  let i = (match lookup_constr_number x sigma with
    		   | Some i -> i | _ -> Error.violation "Constructor has to exist")
    	  in
    	  let mi = List.nth mvec i in
    	  Print.debug "The %d th method is: %t" i (Print.expr ctx mi) ;
    	  let rs = recs p mvec (Util.zip t_sp t_sp_types) in
    	  Print.debug "The recursive calls are: [%t]"
    		      (Print.sequence ~sep:" ;" (fun e -> Print.expr ctx e) rs) ;
    	  let res = join_head_spine mi (t_sp @ rs) in
    	  Print.debug "@[%t@]@ reduces to @[%t@]" (Print.expr ctx e) (Print.expr ctx res) ;
    	  res
	  
       (* the eliminator is stuck *)
       | _ ->
    	  Print.debug
    	    "Stuck eliminator: @[ %t @]@ with head: @[ %t @]"
    	    (Print.expr ctx e) (Print.expr ctx t_hd);
    	  e))
    else
      (Print.debug "n= %d | len(sp) = %d[[%t]]" n (List.length sp) (Print.expr ctx e) ;
      e)
  in
  (fun ctx e -> 
   let e' = norm ctx e in
   Print.debug "Normalization @[%t@]@ ~~> @[%t@]" (Print.expr ctx e)  (Print.expr ctx e') ;
   e')
 *)


(** [nf ctx e] computes the normal form of expression [e]. *)
let nf ctx e = norm ~weak:false ctx e

(** [whnf ctx e] computes the weak head normal form of expression [e]. *)
let whnf ctx e = norm ~weak:true ctx e

