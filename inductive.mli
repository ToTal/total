val elab_type_constr : Context.signature 
		       -> Common.name
		       -> Syntax.expr 
		       (* -> (Common.variable * Syntax.expr) list  *)
		       -> Context.signature

val validate_constrs : Context.signature 
		       -> Common.name
		       -> Syntax.expr 
		       -> (Common.name * Syntax.expr) list
		       -> Context.signature

val elim : Context.signature 
	   -> Common.name
	   -> Syntax.expr 
	   -> (Common.name * Syntax.expr) list
	   -> Context.signature



			    
