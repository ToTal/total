{
  (** The lexer. *)

  open Parser

  let reserved = [
    ("forall", FORALL);
    ("fun", FUN);
    ("Type", TYPE);
    ("impossible", IMPOSSIBLE);
    ("on", ON) ;
    ("yields", YIELDS) ;
    ("On", SET_ON) ;
    ("Off", SET_OFF)
  ]

  let directives = [
    ("Check", CHECK) ;
    ("Definition", DEFINITION) ;
    ("Recursive", RECURSIVE) ;
    ("Split", SPLIT) ;
    ("Eval", EVAL) ;
    ("Whnf", WHNF) ;
    ("Help", HELP) ;
    ("Version", VERSION) ;

    ("Axiom", AXIOM);
    ("Context", CONTEXT) ;
    ("Inductive", INDUCTIVE) ;
    ("Load", LOAD) ;
    ("Reset", RESET) ;
    ("Set", SET) ;
    ("Get", GET) ;
    ("Quit",  QUIT)
  ]

  let position_of_lex lex =
    Common.Position (Lexing.lexeme_start_p lex, Lexing.lexeme_end_p lex)

(* To buffer string literals *)

let string_buffer = ref ""
let reset_string_buffer () =
  string_buffer := ""

let store_string_char c =
  string_buffer := !string_buffer ^ Char.escaped c

let store_string s =
  string_buffer := !string_buffer ^ s

let store_lexeme lexbuf =
  store_string (Lexing.lexeme lexbuf)

let get_stored_string () = !string_buffer

}

let name = ['a'-'z' 'A'-'Z'] ['_' '-' 'a'-'z' 'A'-'Z' '0'-'9' '\'']*

let numeral = ['0'-'9']+

rule token = parse
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\r' '\t']     { token lexbuf }
  | numeral             { NUMERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | "\""
                        { reset_string_buffer();
			  string lexbuf;
			  STRING (get_stored_string()) 
			}
  | "InternalOption"    { option lexbuf}
  | name                { let s = Lexing.lexeme lexbuf in
                            try
                              List.assoc s reserved
                            with Not_found ->
                              (try
                                 List.assoc s directives
                               with Not_found -> NAME s)
                        }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | "."                 { PERIOD }
  | ':'                 { COLON }
  | ','                 { COMMA }
  | "->"                { ARROW }
  | "=>"                { DARROW }
  | ":="                { COLONEQUAL }
  | "|"                 { BAR }
  | "(*"                { comments 0 lexbuf }
  | eof                 { EOF }

and comments level = parse
  | "*)"                { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | "(*"                { comments (level+1) lexbuf }
  | '\n'                { Lexing.new_line lexbuf; comments level lexbuf }
  | _                   { comments level lexbuf }
  | eof                 { print_endline "comments are not closed";
			  raise End_of_file }
and string = parse
  |  '"'                { () }
  | "\\\""              { store_string_char '"' ; string lexbuf }
  | eof                 { Error.violation "Unterminated String" }
  | _                   { store_string_char(Lexing.lexeme_char lexbuf 0); string lexbuf }

and option = parse
  | [^'\n''.']*         { OPTION (Lexing.lexeme lexbuf) }
  | '.'                 { PERIOD }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }

{
  let read_file parser fn =
  try
    let fh = open_in fn in
    let lex = Lexing.from_channel fh in
    lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};
    try
      let terms = parser lex in
      close_in fh;
      terms
    with
      (* Close the file in case of any parsing errors. *)
      Error.Error err -> close_in fh; raise (Error.Error err)
  with
    (* Any errors when opening or closing a file are fatal. *)
    Sys_error msg -> Error.fatal ~loc:Common.Nowhere "%s" msg


  let read_toplevel parser () =
    let ends_with_period str =
      let i = ref (String.length str - 1) in
        while !i >= 0 && List.mem str.[!i] [' '; '\n'; '\t'; '\r'] do decr i done ;
        !i >= 0 && str.[!i] = '.' 
    in

    let rec read_more prompt acc =
      if ends_with_period acc
      then acc
      else begin
        print_string prompt ;
        let str = read_line () in
          read_more "  " (acc ^ "\n" ^ str)
      end
    in

    let str = read_more "# " "" in
    let lex = Lexing.from_string (str ^ "\n") in
      parser lex
}
