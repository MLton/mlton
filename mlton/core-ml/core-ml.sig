(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

type int = Int.t
   
signature CORE_ML_STRUCTS = 
   sig
      include ATOMS
      structure Type: TYPE
      sharing Type = Prim.Type
   end

signature CORE_ML = 
   sig
      include CORE_ML_STRUCTS

      structure Pat:
	 sig
	    type t
	    datatype node =
	       Con of {
		       con: Con.t,
		       arg: t option
		      }
	     | Const of Ast.Const.t
	     | Constraint of t * Type.t
	     | Layered of Var.t * t
	     | Record of {
			  flexible: bool,
			  record: t Record.t
			 }
	     | Var of Var.t
	     | Wild
	    include WRAPPED sharing type node' = node
		            sharing type obj = t

	    val foreachVar: t * (Var.t -> unit) -> unit
	    (* true if pattern contains a constant, constructor or variable *)
	    val isRefutable: t -> bool
	    val isWild: t -> bool
	    val layout: t -> Layout.t
	    val list: t list * Region.t -> t
	    val record: {flexible: bool,
			 record: t Record.t,
			 region: Region.t} -> t
	    (* removeOthersReplace(pat, old,new) replaces all variables in pat
	     * with Wild, except for old, which it replaces with new
	     *)
	    val removeOthersReplace: t * Var.t * Var.t -> t
	    val removeVars: t -> t 	    (* replace all variables with Wild *)
	    val toAst: t -> Ast.Pat.t	    (* conversion to Ast *)
	    val tuple: t vector * Region.t -> t
	    val unit: Region.t -> t
	    (* a list (without duplicates) of variables occurring in a pattern *)
	    val vars: t -> Var.t list 
	 end

      structure Exp:
	 sig
	    type dec
	    type match
	    type t
	    datatype node =
	       App of t * t
	     | Con of Con.t
	     | Const of Ast.Const.t
	     | Constraint of t * Type.t
	     | Fn of match
	     | Handle of t * match
	     | Let of dec vector * t
	     | Prim of Prim.t
	     | Raise of {exn: t,
			 filePos: string}
	     | Record of t Record.t
	     | Var of Var.t
	    include WRAPPED sharing type node' = node
		            sharing type obj = t

	    val andAlso: t * t * Region.t -> t
	    val casee: t * match * Region.t -> t
	    val delay: t * Region.t -> t
	    val force: t * Region.t -> t
	    val foreachVar: t * (Var.t -> unit) -> unit
	    val iff: t * t * t * Region.t -> t
	    (* true if the expression may side-effect. See p 19 of Definition *)
	    val isExpansive: t -> bool
	    val lambda: Var.t * t * Region.t -> t
	    val layout: t -> Layout.t
	    val list: t list * Region.t -> t
	    val orElse: t * t * Region.t -> t
	    val selector: Record.Field.t * Region.t -> t
	    val seq: t vector * Region.t -> t
	    val tuple: t vector * Region.t -> t
	    val unit: Region.t -> t
	    val whilee: {test: t, expr: t, region: Region.t} -> t
	 end

      structure Match:
	 sig
	    type t

	    val filePos: t -> string
	    val new: {rules: (Pat.t * Exp.t) vector,
		      filePos: string} -> t
	    val region: t -> Region.t
	    val rules: t -> (Pat.t * Exp.t) vector
	 end
      where type t = Exp.match

      structure Dec:
	 sig
	    datatype t =
	       Datatype of {
			    tyvars: Tyvar.t vector,
			    tycon: Tycon.t,
			    cons: {
				   con: Con.t,
				   arg: Type.t option
				  } vector
			   } vector
	     | Exception of {
			     con: Con.t,
			     arg: Type.t option
			    }
	     | Fun of {
		       tyvars: Tyvar.t vector,
		       decs: {
			      var: Var.t,
			      types: Type.t vector, (* multiple constraints *)
			      match: Match.t
			     } vector
		      }
	     | Overload of {
			    var: Var.t,
			    scheme: Scheme.t,
			    ovlds: Var.t vector
			   }
	     | Val of {exp: Exp.t,
		       filePos: string,
		       pat: Pat.t,
		       tyvars: Tyvar.t vector}

	    val isExpansive: t -> bool
	    val layout: t -> Layout.t
	    val toAst: t -> Ast.Dec.t
	 end
      where type t = Exp.dec

      structure Program:
	 sig
	    datatype t = T of {
			       decs: Dec.t vector
			      }

	    val layout: t -> Layout.t
	    val layoutStats: t -> Layout.t
	 end
   end
