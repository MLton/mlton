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
	    datatype t =
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

	    val foreachVar: t * (Var.t -> unit) -> unit
	    (* true if pattern contains a constant, constructor or variable *)
	    val isRefutable: t -> bool 
	    val layout: t -> Layout.t
	    val list: t list -> t
	    val record: { flexible: bool, record: t Record.t } -> t
	    (* removeOthersReplace(pat, old,new) replaces all variables in pat
	     * with Wild, except for old, which it replaces with new
	     *)
	    val removeOthersReplace: t * Var.t * Var.t -> t
	    val removeVars: t -> t 	    (* replace all variables with Wild *)
	    val toAst: t -> Ast.Pat.t	    (* conversion to Ast *)
	    val tuple: t vector -> t
	    val unit: t
	    (* a list (without duplicates) of variables occurring in a pattern *)
	    val vars: t -> Var.t list 
	 end

      structure Exp:
	 sig
	    type dec
	    type match
	    datatype t =
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

	    val andAlso: t * t -> t
	    val casee: t * match -> t
	    val compose: unit -> t
	    val delay: t -> t
	    val force: t -> t
	    val foreachVar: t * (Var.t -> unit) -> unit
	    val iff: t * t * t -> t
	    (* true if the expression may side-effect. See p 19 of Definition *)
	    val isExpansive: t -> bool
	    val lambda: Var.t * t -> t
	    val layout: t -> Layout.t
	    val list: t list -> t
	    val orElse: t * t -> t
	    val selector: Record.Field.t -> t
	    val seq: t vector -> t
	    val tuple: t vector -> t
	    val unit: t
	    val whilee: {test: t, expr: t} -> t
	 end

      structure Match:
	 sig
	    datatype t = T of {rules: (Pat.t * Exp.t) vector,
			       filePos: string}

	    val filePos: t -> string
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
