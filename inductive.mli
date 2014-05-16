val elab_type_constr : Context.signature 
		       -> Common.variable 
		       -> Syntax.expr 
		       (* -> (Common.variable * Syntax.expr) list  *)
		       -> Context.signature

val validate_constrs : Context.signature 
		       -> Common.variable 
		       -> Syntax.expr 
		       -> (Common.variable * Syntax.expr) list
		       -> Context.signature

val elim : Context.signature 
	   -> Common.variable 
	   -> Syntax.expr 
	   -> (Common.variable * Syntax.expr) list
	   -> Context.signature



			    
