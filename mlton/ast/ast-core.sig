(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
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
		     Field of pat
		   | Vid of Vid.t * Type.t option * pat option 
		     (* vid <:ty> <as pat> *)
	       end
	    sharing type Item.pat = t

	    datatype node =
	       App of Longcon.t * t
	     | Const of Const.t
	     | Constraint of t * Type.t
	     | FlatApp of t vector
	     | Layered of {constraint: Type.t option,
			   fixop: Fixop.t,
			   pat: t,
			   var: Var.t}
	     | List of t vector
	     | Record of {flexible: bool,
			  items: (Record.Field.t * Item.t) vector}
	     | Tuple of t vector
	     | Var of {fixop: Fixop.t,
		       name: Longvid.t}
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
	    val layoutDelimit: t -> Layout.t
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
	     | CommandLineConst of {value: Const.t}
	     | Const
	     | Export of Attribute.t list
	     | Import of Attribute.t list
	     | Prim
	 end

      structure Priority:
	 sig
	    datatype t = T of int option
	    val <= : t * t -> bool
	    val default: t
	    val layout: t -> Layout.t
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
	     | List of t vector
	     | Orelse of t * t
	     | Prim of {kind: PrimKind.t,
			name: string,
			ty: Type.t}
	     | Raise of t
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
	    val const: Const.t -> t
	    val constraint: t * Type.t -> t
	    val fnn: (Pat.t * t) vector -> t
            val layout: t -> Layout.t
	    val lett: dec vector * t * Region.t -> t
	    val longvid: Longvid.t -> t
	    val tuple: t vector -> t
	    val unit: t
	    val var: Var.t -> t
	 end

      structure Match:
	 sig
	    type t
	    datatype node = T of (Pat.t * Exp.t) vector
	    include WRAPPED
	    sharing type node' = node
            sharing type obj = t

            val layout: t -> Layout.t
	 end where type t = Exp.match
      
      structure EbRhs:
	 sig
	    type t
	    datatype node =
	       Def of Longcon.t
	     | Gen of Type.t option
	    include WRAPPED sharing type node' = node
			    sharing type obj = t
	 end

      structure Dec:
	 sig
	    type t
	    datatype node =
	       Abstype of {datBind: DatBind.t,
			   body: t}
	     | Datatype of DatatypeRhs.t
	     | Exception of (Con.t * EbRhs.t) vector
	     | Fix of {fixity: Fixity.t,
		       ops: Vid.t vector}
	     | Fun of Tyvar.t vector * {body: Exp.t,
					pats: Pat.t vector,
					resultType: Type.t option} vector vector
	     | Local of t * t
	     | Open of Longstrid.t vector
	     | Overload of Priority.t *
	                   Var.t * 
			   Tyvar.t vector * Type.t * 
			   Longvar.t vector
	     | SeqDec of t vector
	     | Type of TypBind.t
	     | Val of {rvbs: {match: Match.t,
			      pat: Pat.t} vector,
		       tyvars: Tyvar.t vector,
		       vbs: {exp: Exp.t,
			     pat: Pat.t} vector}
	    include WRAPPED sharing type node' = node
			    sharing type obj = t

            val checkSyntax: t -> unit
            val datatypee: {tyvars: Tyvar.t vector,
			    tycon: Tycon.t,
			    cons: (Con.t * Type.t option) vector} vector -> t
            val empty: t
	    val exceptionn: Con.t * Type.t option -> t
	    val fromExp: Exp.t -> t
	    val layout: t -> Layout.t
	    val openn: Longstrid.t vector -> t
	    val vall: Tyvar.t vector * Var.t * Exp.t -> t
	 end
      sharing type Dec.t = Exp.dec
   end
