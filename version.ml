let logo = " _______________    _ \n" ^
	   "|__   _____   __|  | |\n" ^
	   "   | | ___ | | __ _| |\n" ^
	   "   | |/ _ \\| |/ _` | |\n" ^
	   "   | | (_) | | (_| | |\n" ^
	   "   |_|\\___/|_|\\__,_|_| (ish - this version contains Type:Type)\n"

let logo_alt = "  ____________________ _       _ \n" ^
	       " |  __   ________   __(_)     | |\n" ^
	       " | |__) |_ _ _ __| |   _  __ _| |\n" ^
	       " |  ___/ _` | '__| |  | |/ _` | |\n" ^
	       " | |  | (_| | |  | |  | | (_| | |\n" ^
	       " |_|   \\__,_|_|  |_|  |_|\\__,_|_|\n"
                                 
let version = "0.00 transitional" ;;

let print_version () =
    print_endline (if !Config.totality_is_tainted then logo_alt else logo);
    print_endline ("Version: " ^ version);
