val elab_type_constr : Ctx.signature 
		       -> Common.name
		       -> Syntax.expr 
		       -> Ctx.signature

val validate_constrs : Ctx.signature 
		       -> Common.name
		       -> Syntax.expr 
		       -> (Common.name * Syntax.expr) list
		       -> Ctx.signature

val elim : Ctx.signature 
	   -> Common.name
	   -> Syntax.expr 
	   -> (Common.name * Syntax.expr) list
	   -> Ctx.signature



			    
