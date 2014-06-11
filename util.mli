
val zip : 'a list -> 'b list -> ('a * 'b) list

val maybe : 'a option -> (unit -> 'a) -> 'a

val impossible : unit -> 'a

val this : 'a option -> 'a
