(** Definitions in common use *)

type name = string

(** Variable names *)

type variable = name option * unit ref

let none () = None, ref ()
let none_with x = Some x, ref ()
let some x = Some x, ref ()

let get_name = fst

let eq (_, r1) (_,r2) = r1 == r2

(** Position in source code. For each type in the abstract syntax we define two versions
    [t] and [t']. The former is the latter with a position tag. For example, [expr = expr'
    * position] and [expr'] is the type of expressions (without positions). 
*)
type position =
  | Position of Lexing.position * Lexing.position (** delimited position *)
  | Nowhere (** unknown position *)

(** [nowhere e] is the expression [e] without a source position. It is used when
    an expression is generated and there is not reasonable position that could be
    assigned to it. *)
let nowhere x = (x, Nowhere)
