(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
   
signature MATCH_COMPILE_STRUCTS =
   sig
      include ATOMS
      structure Type:
	 sig
	    type t

	    val deTuple: t -> t vector
	    val equals: t * t -> bool
	    val int: IntSize.t -> t
	    val layout: t -> Layout.t
	    val unit: t
	    val word: WordSize.t -> t
	 end
      structure Cases:
	 sig
	    type exp
	    type t

	    val con: {con: Con.t,
		      targs: Type.t vector,
		      arg: (Var.t * Type.t) option,
		      rhs: exp} vector -> t
	    val int: IntSize.t * (IntX.t * exp) vector -> t
	    val word: WordSize.t * (WordX.t * exp) vector -> t
	 end
      structure Exp:
	 sig
	    type t
	       
	    val const: Const.t -> t
	    val var: Var.t * Type.t -> t
	    val detuple: {tuple: t,
			  body: (Var.t * Type.t) vector -> t} -> t
	    val casee:
	       {cases: Cases.t,
		default: (t * Region.t) option,
		test: t,
		ty: Type.t}  (* type of entire case expression *)
	       -> t
	    val lett: {var: Var.t, exp: t, body: t} -> t
	    val iff: {test: t, thenn: t, elsee: t, ty: Type.t} -> t
	    val equal: t * t -> t
	    val deref: t -> t
	    val layout: t -> Layout.t
	 end
      sharing type Cases.exp = Exp.t
      structure NestedPat: NESTED_PAT
      sharing Atoms = NestedPat.Atoms
      sharing Type = NestedPat.Type
   end

signature MATCH_COMPILE =
   sig
      include MATCH_COMPILE_STRUCTS

      val matchCompile:
	 {caseType: Type.t, (* type of entire expression *)
	  cases: (NestedPat.t * ((Var.t -> Var.t) -> Exp.t)) vector,
	  conTycon: Con.t -> Tycon.t,
	  region: Region.t,
	  test: Var.t,
	  testType: Type.t,
	  tyconCons: Tycon.t -> {con: Con.t, hasArg: bool} vector}
	 -> Exp.t * (unit -> Layout.t vector)
   end
