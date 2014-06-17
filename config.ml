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
