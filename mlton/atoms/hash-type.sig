(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature HASH_TYPE_STRUCTS = 
   sig
      include ATOMS
   end

signature HASH_TYPE = 
   sig
      include HASH_TYPE_STRUCTS
      include TYPE_OPS sharing type tycon = Tycon.t

      structure Dest:
	 sig
	    datatype dest =
	       Var of Tyvar.t
	     | Con of Tycon.t * t vector
	    val dest: t -> dest
	 end

      val containsTycon: t * Tycon.t -> bool
      (* O(1) time *)
      val equals: t * t -> bool
      (* for reporting type errors *)
      val error: string * Layout.t -> 'a
      val fromPrim: Prim.Type.t -> t
      val hash: t -> Word.t
      val hom: {ty: t,
		var: Tyvar.t -> 'a,
		con: Tycon.t * 'a vector -> 'a} -> 'a
      val isUnit: t -> bool
      val layout: t -> Layout.t
      val makeHom:
	 {var: t * Tyvar.t -> 'a,
	  con: t * Tycon.t * 'a vector -> 'a}
	 -> {hom: t -> 'a,
	     destroy: unit -> unit}
      val makeMonoHom:
	 {con: t * Tycon.t * 'a vector -> 'a}
	 -> {hom: t -> 'a,
	     destroy: unit -> unit}
      val ofConst: Const.t -> t
      val optionToAst: t option -> Ast.Type.t option
      val plist: t -> PropertyList.t
      val stats: unit -> Layout.t
      (* substitute (t, [(a1, t1), ..., (an, tn)]) performs simultaneous
       * substitution of the ti for ai in t.
       * The ai's are not required to contain every free variable in t
       *)
      val substitute: t * (Tyvar.t * t) vector -> t
      (* conversion to Ast *)
      val toAst: t -> Ast.Type.t
      val toPrim: t -> Prim.Type.t
      val tycon: t -> Tycon.t
      val var: Tyvar.t -> t
   end
