(** Syntax Utilities. *)

open Syntax
open Ctx

(** A telescope returns all the binders (pi abstractions) at the beginning of a type.
    It returns a function that given a expression, returns a value  *)
type telescope = (Common.variable * expr) list

let get_telescope e = 
  let rec get_telescope : expr -> telescope * expr = function
    | Pi (x, t, e),loc -> 
       let tel, rest = get_telescope (top_db_to_var x e) in
       (x, t)::tel, rest
    | LambdaAnn _,_ -> Error.violation "Lambda found in a telescope, add?"
    | rest -> [], rest
  in
  get_telescope e 

let set_telescope t e f =
  let rec set_telescope (t : telescope) (e : expr) (f : Common.variable -> expr -> expr -> expr ) : expr =
    List.fold_right (fun (x, t) e -> f x t (var_to_db x e)) t e
  in
  set_telescope t e f

(* Note: split and join head spine destroy location information, it'd
   be better to have a native spine based representation (TODO) *)

let split_head_spine e = 
  let rec split_head_spine (e, l) = match e with
    | App(e1, e2) -> let h, sp = split_head_spine e1 in h, (sp @ [e2])
    | _ -> (e, l), []
  in
  split_head_spine e
	       
let join_head_spine h sp =
  List.fold_left (fun h sp -> Common.nowhere(App(h, sp))) h sp 

(* Returns wether e constains the constant d in its head *)
let is_constr d e = 
    let h, _ = split_head_spine e in
    match h with
    | Const c, l -> d = c
    | _ -> false

(* Returns wether e contains the constant d after some Pi abstractions *)
let rec produces_constr c = function
  | Pi (_,_, e),_ -> produces_constr c e
  | e -> is_constr c e

