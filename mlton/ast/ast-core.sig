(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature AST_CORE_STRUCTS = 
   sig
      include AST_ATOMS
   end

signature AST_CORE = 
   sig
      include AST_CORE_STRUCTS
      
      (* controls layout of applications of Vid's *)
      val isInfix: (Vid.t -> bool) ref
      
      structure Fixity:
	 sig
	    datatype t =
	       Infix of int option
	     | Infixr of int option
	     | Nonfix

	    val bogus: t
	    val isInfix: t -> bool
	    val layout: t -> Layout.t
	 end
	 
      structure Fixop:
	 sig
	    datatype t = Op | None
	 end

      structure Pat:
	 sig
	    type t
	       
	    structure Item:
	       sig
		  type pat
		  datatype t =
		     Field of Record.Field.t * pat
		   | Vid of Vid.t * Type.t option * pat option 
		     (* vid <:ty> <as pat> *)
	       end
	    sharing type Item.pat = t

	    datatype node =
	       Wild
	     | Var of {fixop: Fixop.t, name: Longvid.t}
	     | Const of Const.t
	     | Tuple of t vector
	     | Record of {items: Item.t vector,
			  flexible: bool}
	     | List of t list
	     | FlatApp of t vector
	     | App of Longcon.t * t
	     | Constraint of t * Type.t
	     | Layered of {fixop: Fixop.t,
			   var: Var.t,
			   constraint: Type.t option,
			   pat: t}
	       
	    include WRAPPED sharing type node' = node
			    sharing type obj = t

	    val app: Con.t * t -> t
	    val con: Con.t -> t
	    val const: Const.t -> t
	    val constraint: t * Type.t -> t
	    val layered: {fixop: Fixop.t,
			  var: Var.t,
			  constraint: Type.t option,
			  pat: t} -> t
	    val layout: t -> Layout.t
	    val longvid: Longvid.t -> t
	    val tuple: t vector -> t
	    val var: Var.t -> t
	    val wild: t
	 end
      
      structure Exp:
	 sig
	    type dec
	    type t
	    datatype node =
	       Const of Const.t
	     | Var of {name: Longvid.t, fixop: Fixop.t}
	     | Fn of (Pat.t * t) vector
	     | FlatApp of t vector
	     | App of t * t
	     | Case of t * (Pat.t * t) vector
	     | Let of dec * t
	     | Seq of t vector
	     | Record of t Record.t
	     | List of t list
	     | Selector of Record.Field.t
	     | Constraint of t * Type.t
	     | Handle of t * (Pat.t * t) vector
	     | Raise of t
	     | If of t * t * t
	     | Andalso of t * t
	     | Orelse of t * t
	     | While of {test: t,
			 expr: t}
	     | Prim of {name: string,
			ty: Type.t}
	     | FFI of {name: string,
		       ty: Type.t}

	    include WRAPPED sharing type node' = node
			    sharing type obj = t
	       
	    val app: t * t -> t
	    val casee: t * (Pat.t * t) vector -> t
	    val con: Con.t -> t
	    val const: Const.t -> t
	    val constraint: t * Type.t -> t
	    val fnn: (Pat.t * t) vector -> t
	    val handlee: t * (Pat.t * t) vector -> t
	    val iff: t * t * t -> t
	    val layout: t -> Layout.t
	    val lett: dec vector * t -> t
	    val longvid: Longvid.t -> t
	    val raisee: t -> t
	    val record: t Record.t -> t
	    val select: {tuple: t, offset: int} -> t
	    val seq: t vector -> t
	    val tuple: t vector -> t
	    val unit: t
	    val var: Var.t -> t
	 end

      structure EbRhs:
	 sig
	    type t
	    datatype node =
	       Gen of Type.t option
	     | Def of Longcon.t
	    include WRAPPED sharing type node' = node
			    sharing type obj = t
	 end

      structure Dec:
	 sig
	    type t
	    datatype node =
	       Val of {tyvars: Tyvar.t vector,
		       vbs: (Pat.t * Exp.t) vector,
		       rvbs: {var: Var.t,
			      ty: Type.t option,
			      fixity: Vid.t option,
			      rules: (Pat.t * Exp.t) vector} vector}
	     | Fun of Tyvar.t vector * {pats: Pat.t vector,
					resultType: Type.t option,
					body: Exp.t} vector vector
	     | Type of TypBind.t
	     | Datatype of DatatypeRhs.t
	     | Abstype of {datBind: DatBind.t,
			   body: t}
	     | Exception of (Con.t * EbRhs.t) vector
	     | SeqDec of t vector
	     | Local of t * t
	     | Open of Longstrid.t vector
	     | Overload of Var.t * Type.t * Longvar.t vector
	     | Fix of {fixity: Fixity.t,
		       ops: Vid.t vector}
	    include WRAPPED sharing type node' = node
			    sharing type obj = t

            val datatypee: {tyvars: Tyvar.t vector,
			    tycon: Tycon.t,
			    cons: (Con.t * Type.t option) vector} vector -> t
            val empty: t
	    val exceptionn: Con.t * Type.t option -> t
	    val fromExp: Exp.t -> t
	    val funn:
	       Tyvar.t vector
	       * (Var.t * (Pat.t * Exp.t) vector * Type.t option) vector
	       -> t
	    val layout: t -> Layout.t
	    val openn: Longstrid.t vector -> t
	    val vall: Tyvar.t vector * Var.t * Exp.t -> t
	 end
      sharing type Dec.t = Exp.dec

      val layoutLet: Layout.t * Layout.t -> Layout.t
      val layoutLocal: Layout.t * Layout.t -> Layout.t
   end
