(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature TYPE_STRUCTS =
   sig
      structure Ast: AST
      structure Record: RECORD
      structure Tycon: TYCON
      structure Tyvar: TYVAR
      sharing Tyvar = Ast.Tyvar
      sharing Ast.Tycon = Tycon.AstId
      sharing Record = Ast.SortedRecord
   end

signature TYPE = 
   sig
      include TYPE_STRUCTS
      include TYPE_OPS where type tycon = Tycon.t
	    
      datatype t' =
	 Var of Tyvar.t
       | Con of Tycon.t * t' vector
       | Record of t' Record.t
      sharing type t = t'
	 
      val hom: {ty: t,
		var: Tyvar.t -> 'a,
		con: Tycon.t * 'a vector -> 'a} -> 'a
      val layout: t -> Layout.t
      val optionToAst: t option -> Ast.Type.t option
      val record: t Record.t -> t
      (* substitute(t, [(a1, t1), ..., (an, tn)]) performs simultaneous
       * substitution of the ti for ai in t.
       *)
      val substitute: t * (Tyvar.t * t) vector -> t
      val toAst: t -> Ast.Type.t
      (* tyvars returns a list (without duplicates) of all the type variables
       * in a type.
       *)
      val tyvars: t -> Tyvar.t list
      val var: Tyvar.t -> t
   end

