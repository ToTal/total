(** Normalization of expressions. *)

open Syntax
open Ctx

let rec head_is_elim sigma = function
  | App (e1, e2),l -> head_is_elim sigma e1
  | Const x, l -> is_elim sigma x
  | _ -> false

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
	   (* Beta - reduction *)
            | Lambda (x, t, e) -> norm ctx (mk_subst (Dot (e2, idsubst)) e)
	    (* Iota - reduction *)
	    | App _ when head_is_elim sigma e1 -> 
	       let (Const x, _), _ = split_head_spine e1 in
	       let Some (delim, _, d) = lookup_elim x sigma in
	       iota ctx (App (e1, e2), loc) delim d
	    | Const x when is_elim sigma x -> 
	       let Some (delim, _, d) = lookup_elim x sigma in
	       iota ctx (App (e1, e2), loc) delim d

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
      | (e, t)::es when is_constr d t ->
	 Print.debug "Recs: recursive %t" (Print.expr ctx e) ;
	 let Some elim = lookup_elim_for_ty d sigma in
    	 (join_head_spine (mk_const elim) (e::p::mvec)):: recs p mvec es
      | _::es -> recs p mvec es
    in
    let h, sp = split_head_spine e in
    Print.debug "sp = [%t]" (Print.sequence ~sep:" ;" (fun e -> Print.expr ctx e)  sp  ) ;
    let x = (match norm ctx h with
    | Const x, _ ->
       (if (is_elim sigma x) then x
    	else Error.violation "iota reduction called in something that is not an eliminator")
    | _ -> Error.violation "iota reudction does not contain an eliminator in its head")
    in
    let t, n = match (lookup_elim x sigma) with
      |	Some (t, n, _) -> t, n
      | None -> Error.violation "this cannot happen" in
    if n = List.length sp then
      (* the head of the spine contains the target *)
      (Print.debug "Target: %t" (Print.expr ctx (List.hd (List.rev sp))) ;
      let t_hd, t_sp = split_head_spine (norm ctx (List.hd (List.rev sp))) in
      Print.debug "Target head: %t" (Print.expr ctx t_hd) ;
      Print.debug "Target spine: [%t]" (Print.sequence ~sep:" ;" (fun e -> Print.expr ctx e) t_sp);
      (match t_hd with
       (* the normal form of the target is constructor *)
       | Const x, l ->
	  let t_sp_types = let tel,_ = get_telescope (lookup_ty x sigma) in List.map snd tel in
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
    norm

(** [nf ctx e] computes the normal form of expression [e]. *)
let nf ctx e = norm ~weak:false ctx e

(** [whnf ctx e] computes the weak head normal form of expression [e]. *)
let whnf ctx e = norm ~weak:true ctx e

