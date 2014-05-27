type name = string

type variable 

val none : unit -> variable

val none_with : name -> variable

val some : name -> variable

val get_name : variable -> name option

val eq : variable -> variable -> bool

type position = Position of Lexing.position * Lexing.position | Nowhere

val nowhere : 'a -> 'a * position
