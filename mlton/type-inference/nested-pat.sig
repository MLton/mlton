(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature NESTED_PAT_STRUCTS = 
   sig
      include ATOMS
      structure Type:
	 sig
	    type t
	    val tuple: t vector -> t
	 end
   end

signature NESTED_PAT = 
   sig
      include NESTED_PAT_STRUCTS
      
      datatype t = T of {pat: node, ty: Type.t}
      and node =
	 Wild
       | Var of Var.t
       | Const of Const.t
       | Con of {con: Con.t,
		 targs: Type.t vector,
		 arg: t option}
       | Tuple of t vector
       | Layered of Var.t * t

      (* isRefutable p iff p contains a constant, constructor or variable. *)
      val isRefutable: t -> bool
      val isVar: t -> bool
      val layout: t -> Layout.t
      val new: node * Type.t -> t
      val node: t -> node
      val tuple: t vector -> t
      val ty: t -> Type.t
      val unit: t
      (* varsAndTypes returns a list of the variables in the pattern, along with
       * their types.  It is used for match compilation in order to build a
       * function that abstracts over the expression of a case rule p => e.
       * See infer.fun.
       *)
      val varsAndTypes: t -> (Var.t * Type.t) list
      val wild: Type.t -> t
   end
