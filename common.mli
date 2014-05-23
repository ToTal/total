type name = string

type variable 

val none : variable

val none_with : name -> variable

val some : name -> variable

val get_name : variable -> name option

type position = Position of Lexing.position * Lexing.position | Nowhere

val nowhere : 'a -> 'a * position
