signature DIRECT_EXP =
  sig
     include ATOMS

     structure Type: HASH_TYPE
     structure Func: ID	
     structure Exp:
	sig
	   type t

	   val const: Const.t -> t
	   val var: Var.t * Type.t -> t
	   val tuple: {exps: t list, ty: Type.t} -> t
	   val select: {tuple: t,
			offset: int,
			ty: Type.t} -> t
	   val conApp: {con: Con.t,
			args: t list,
			ty: Type.t} -> t
	   val primApp: {prim: Prim.t,
			 info: PrimInfo.t,
			 targs: Type.t list,
			 args: t list,
			 ty: Type.t} -> t
	   val call: {func: Func.t,
		      args: t list,
		      ty: Type.t} -> t
	   val raisee: t -> t
	   val handlee: {try: t,
			 ty: Type.t,
			 catch: Var.t * Type.t,
			 handler: t} -> t
	   val casee: {
		       cause: Cause.t,
		       test: t,
		       cases: {con: Con.t,
			       args: (Var.t * Type.t) list,
			       body: t} list,
		       default: t option,
		       ty: Type.t (* type of entire case expression *)
		       } -> t
	   val lett: {decs: {
			     var: Var.t,
			     ty: Type.t,
			     exp: t
			     } list,
		      body: t} -> t

	   val detuple: {tuple: t,
			 body: Var.t list -> t} -> t

	   val detupleBind: {tuple: t,
			     components: Var.t list,
			     body: t} -> t
	end

     structure Program:
	sig
	   type t
	end
     
     structure Accum: 
	sig
	   type t
	      
	   type global = {var: Var.t,
			  ty: Type.t,
			  exp: Exp.t}

	   type function = {name: Func.t,
			    args: (Var.t * Type.t) list,
			    body: Exp.t,
			    returns: Type.t list}

	   type data = {cons:{args:Type.t list,con:Con.t} list,
			tycon:Tycon.t}

	   val empty      : t
	   val addGlobal  : (t * global) -> t
	   val addGlobals : (t * global list) -> t
	   val addFunc    : (t * function) -> t
	   val addDatas   : (t * data list) -> t
	   val done       : t -> Func.t -> Program.t
	end
  end
