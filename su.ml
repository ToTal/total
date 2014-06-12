(** Syntax Utilities. *)

open Syntax
open Ctx

(** A telescope returns all the binders (pi abstractions) at the beginning of a type.
    It returns a function that given a expression, returns a value  *)
type telescope = (Common.variable * expr) list

let get_telescope ctx e = 
  let rec get_telescope : expr -> telescope * expr = function
    | Pi (x, t, e),loc -> 
       let tel, rest = get_telescope (top_db_to_var x e) in
       (x, t)::tel, rest
    | Lambda _,_ -> Error.violation "Lambda found in a telescope, add?"
    | rest -> [], rest
  in
  let tel, e' = get_telescope e in
  Print.debug "******* Get telescope of e: @[%t@]@ into@ tel:@[[%t]@]@ rest:@[%t@]"
  	      (Print.expr ctx e)
  	      (Print.sequence ~sep:" ;" (fun (_, e) -> Print.expr ctx e) tel)
  	      (Print.expr ctx e') ;
  tel, e'

let set_telescope ctx t e f =
  let rec set_telescope (t : telescope) (e : expr) (f : Common.variable -> expr -> expr -> expr ) : expr =
    List.fold_left (fun e (x, t) -> f x t (var_to_db x e)) e t
  in
  let e' = set_telescope t e f in
  (* Print.debug "####### Set telescope tel: @[[%t@]]@ and@ rest:@[%t@]@ into:@[%t@]" *)
  (* 	      (Print.sequence ~sep:" ;" (fun (_, e) -> Print.expr ctx e) t) *)
  (* 	      (Print.expr ctx e) *)
  (* 	      (Print.expr ctx e') ; *)
  e'

(* Note: split and join head spine destroy location information, it'd
   be better to have a native spine based representation (TODO) *)

let split_head_spine ctx e = 
  let rec split_head_spine (e, l) = match e with
    | App(e1, e2) -> let h, sp = split_head_spine e1 in h, (sp @ [e2])
    | _ -> (e, l), []
  in
  let h, sp = split_head_spine e in
  (* Print.debug "@@@@@@@@@@@@@@@@ Splitting e: @[%t@]@ into@ Head:@[%t@]@ Spine:@[[%t]@]" *)
  (* 	      (Print.expr ctx e) *)
  (* 	      (Print.expr ctx h) *)
  (* 	      (Print.sequence ~sep:" ;" (Print.expr ctx) sp) ; *)
  h, sp
	       

let join_head_spine ctx h sp =
  let res = List.fold_left (fun h sp -> Common.nowhere(App(h, sp))) h sp in
  (* Print.debug "XXXXXXXXXXXXXXXXXX JOIN Head:@[%t@]@ Spine:@[[%t]@]@ into e: @[%t@]" *)
  (* 	      (Print.expr ctx h) *)
  (* 	      (Print.sequence ~sep:" ;" (Print.expr ctx) sp)  *)
  (* 	      (Print.expr ctx res) *)
  (* ; *)
  res

(* Returns wether e constains the constant d in its head *)
let is_constr ctx d e = 
    let h, _ = split_head_spine ctx e in
    match h with
    | Const c, l -> d = c
    | _ -> false

