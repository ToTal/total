val elab_type_constr : Context.signature 
		       -> Common.variable 
		       -> Syntax.expr 
		       (* -> (Common.variable * Syntax.expr) list  *)
		       -> Context.signature

val elab_constrs : Context.signature 
		   -> Common.variable 
		   -> Syntax.expr 
		   -> (Common.variable * Syntax.expr) list
		   -> Context.signature




			    
