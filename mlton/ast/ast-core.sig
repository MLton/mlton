(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature AST_CORE_STRUCTS = 
   sig
      include AST_ATOMS
   end

signature AST_CORE = 
   sig
      include AST_CORE_STRUCTS
      
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
	       App of Longcon.t * t
	     | Const of Const.t
	     | Constraint of t * Type.t
	     | FlatApp of t vector
	     | Layered of {fixop: Fixop.t,
			   var: Var.t,
			   constraint: Type.t option,
			   pat: t}
	     | List of t list
	     | Record of {items: Item.t vector,
			  flexible: bool}
	     | Tuple of t vector
	     | Var of {fixop: Fixop.t, name: Longvid.t}
	     | Wild
	       
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

      structure PrimKind:
	 sig
	    structure Attribute:
	       sig
		  datatype t = Cdecl | Stdcall
		     
		  val layout: t -> Layout.t
	       end
	    
	    datatype t =
	       BuildConst
	     | Const
	     | Export of Attribute.t list
	     | Import of Attribute.t list
	     | Prim
	 end
      
      structure Exp:
	 sig
	    type dec
	    type match
	    type t
	    datatype node =
	       Andalso of t * t
	     | App of t * t
	     | Case of t * match
	     | Const of Const.t
	     | Constraint of t * Type.t
	     | FlatApp of t vector
	     | Fn of match
	     | Handle of t * match
	     | If of t * t * t
	     | Let of dec * t
	     | List of t list
	     | Orelse of t * t
	     | Prim of {kind: PrimKind.t,
			name: string,
			ty: Type.t}
	     | Raise of {exn: t,
			 filePos: string}
	     | Record of t Record.t
	     | Selector of Record.Field.t
	     | Seq of t vector
	     | Var of {fixop: Fixop.t,
		       name: Longvid.t}
	     | While of {expr: t,
			 test: t}

	    include WRAPPED sharing type node' = node
			    sharing type obj = t
	       
	    val app: t * t -> t
	    val casee: t * match -> t
	    val con: Con.t -> t
	    val const: Const.t -> t
	    val constraint: t * Type.t -> t
	    val fnn: match -> t
	    val handlee: t * match -> t
	    val iff: t * t * t -> t
	    val layout: t -> Layout.t
	    val lett: dec vector * t -> t
	    val longvid: Longvid.t -> t
	    val raisee: {exn: t, filePos: string} -> t
	    val record: t Record.t -> t
	    val select: {tuple: t, offset: int} -> t
	    val seq: t vector -> t
	    val tuple: t vector -> t
	    val unit: t
	    val var: Var.t -> t
	 end

      structure Match:
	 sig
	    datatype t = T of {filePos: string,
			       rules: (Pat.t * Exp.t) vector}
	 end where type t = Exp.match
      
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
		       vbs: {pat: Pat.t,
			     exp: Exp.t,
			     filePos: string} vector,
		       rvbs: {pat: Pat.t,
			      match: Match.t} vector}
	     | Fun of Tyvar.t vector * {clauses: {pats: Pat.t vector,
						  resultType: Type.t option,
						  body: Exp.t} vector,
					filePos: string} vector
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
	    val funn: Tyvar.t vector * {var: Var.t,
					match: Match.t,
					resultTy: Type.t option} vector -> t
	    val layout: t -> Layout.t
	    val openn: Longstrid.t vector -> t
	    val vall: Tyvar.t vector * Var.t * Exp.t -> t
	 end
      sharing type Dec.t = Exp.dec

      val layoutLet: Layout.t * Layout.t -> Layout.t
      val layoutLocal: Layout.t * Layout.t -> Layout.t
   end
