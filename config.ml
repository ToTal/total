(* Should the pretty printing print non dependent function types as -> ? *)
let pretty_print_arrow = ref true 

(* Should the pretty printer use De Bruijn indices? *)
let pretty_print_db = ref false 

(* Is debug on? *)
let debug = ref false

(* Positivity check on *)

let check_positivity = ref true

(* Do we need to print synthesized signature components *)

let show_synthesized = ref false

(* Are programs total? *)

let totality_is_tainted = ref false

let check_tainted_totality () = 
  if not !check_positivity then
    totality_is_tainted := true

(* Setting handling *)

exception SettingExc of string

type setting_value = 
    | BoolSetting of bool 
    | StringSetting of string

type setting =
    { set : (setting_value -> unit)
    ; get : (unit -> setting_value)
    ; help : string
    }

(* convert a  setting_value to boolean if possible *)
let to_boolean = function
  | BoolSetting b -> b
  | _ -> raise (SettingExc "Expected a setting of type boolean (On|Off)")

(* convert a  setting_value to string if possible *)
let to_string = function
  | StringSetting s -> s
  | _ -> raise (SettingExc "Expected a setting of type string")


let settings = 
  [ ("Positivity", { set = (fun v -> check_positivity := (to_boolean v))
		   ; get = (fun () -> BoolSetting !check_positivity)
		   ; help = "Boolean setting to enable/disable the positivity checker"})

  ; ("Debug", { set = (fun v -> debug := (to_boolean v))
	      ; get = (fun () -> BoolSetting !debug)
	      ; help = "Boolean setting to enable/disable debug mode"})

  ; ("Synthesized", { set = (fun v -> show_synthesized := (to_boolean v))
		    ; get = (fun () -> BoolSetting !show_synthesized)
		    ; help = "Boolean setting to enable/disable printed sinthesized declarations"})
  ]

let set_option n v =
  try 
    let entry = List.assoc n settings in
    entry.set v ;
    check_tainted_totality()
  with Not_found -> raise (SettingExc "Setting not found")
			  
let get_option n = 
  try 
    let entry = List.assoc n settings in
    entry.get ()
  with Not_found -> raise (SettingExc "Setting not found")

let str_of_setting = function
  | BoolSetting true -> "On"
  | BoolSetting false -> "Off"
  | StringSetting s -> "\"" ^ s ^ "\""

let get_settings_doc () = 
  List.fold_left (fun r (s, e) -> 
		  s ^ " := " ^ (str_of_setting (e.get ())) ^ "\t(" ^ e.help ^ ")\n" ^ r)
		 (if !totality_is_tainted then "Totality may be tainted." else "")
		 settings
