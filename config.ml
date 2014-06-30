(* Should the pretty printing print non dependent function types as -> ? *)
let pretty_print_arrow = ref true 

(* Should the pretty printer use De Bruijn indices? *)
let pretty_print_db = ref false 

(* Is debug on? *)
let debug = ref false

(* Positivity check on *)

let check_positivity = ref true

(* Are programs total? *)

let totality_is_tainted = ref false


(* Option handling *)

(* type cmd_line_option =  *)
(*     { key : Arg.key  *)
(*     ; spec : Arg. spec *)
(*     ; doc : Arg.doc  *)
(*     } *)


type opt_tp =
  | Opt_float of float
  | Opt_int of int
  | Opt_string of string
  | Opt_bool of bool
