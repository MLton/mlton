(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature DIRECT_EXP_STRUCTS =
  sig
     include SSA_TREE
  end

signature DIRECT_EXP =
  sig
     include DIRECT_EXP_STRUCTS

     structure DirectExp:
	sig
	   type t

	   datatype cases =
	      Char of (char * t) vector
	    | Con of {con: Con.t,
		      args: (Var.t * Type.t) vector,
		      body: t} vector
	    | Int of (int * t) vector
	    | Word of (word * t) vector
	    | Word8 of (Word8.t * t) vector

	   val arith: {prim: Prim.t,
		       args: t vector,
		       overflow: t,
		       ty: Type.t} -> t
	   (* For now, call always uses Handler.None.  This means it should only
	    * be used for functions that cannot raise.
	    *)
	   val call: {func: Func.t, args: t vector, ty: Type.t} -> t
	   val casee: {test: t, 
		       cases: cases,
		       default: t option,
		       ty: Type.t} -> t
	   val conApp: {con: Con.t, 
			args: t vector,
			ty: Type.t} -> t
	   val const: Const.t -> t
	   val detuple: {body: Var.t vector -> t,
			 length: int,
			 tuple: t} -> t
	   val detupleBind: {body: t,
			     components: Var.t vector,
			     tuple: Var.t,
			     tupleTy: Type.t} -> t
	   val eq: t * t * Type.t -> t
	   val falsee: t
	   val handlee: {try: t,
			 ty: Type.t,
			 catch: Var.t * Type.t,
			 handler: t} -> t
	   val int: int -> t
	   val layout: t -> Layout.t
	   val lett: {decs: {var: Var.t, exp: t} list,
		      body: t} -> t
	   val linearize: t * Handler.t -> Label.t * Block.t list
	   val linearizeGoto: t * Handler.t * Label.t -> Label.t * Block.t list
	   val name: t * (Var.t -> t) -> t
	   val primApp: {args: t vector,
			 prim: Prim.t,
			 targs: Type.t vector, 
			 ty: Type.t} -> t 
	   val raisee: t -> t
	   val select: {tuple: t, 
			offset: int, 
			ty: Type.t} -> t
	   val seq: t * t -> t
	   val truee: t
	   val tuple: {exps: t vector, ty: Type.t} -> t
	   val var: Var.t * Type.t -> t
	end
  end
