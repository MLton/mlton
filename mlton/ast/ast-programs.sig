(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature AST_PROGRAMS_STRUCTS =
   sig
      include AST_ATOMS_STRUCTS
   end

signature AST_PROGRAMS =
   sig
      include AST_MODULES

      structure Program:
	 sig
	    datatype t = T of Topdec.t list list

	    val append: t * t -> t
	    val coalesce: t -> t
	    val empty: t
	    val size: t -> int
	    val layout: t -> Layout.t
	 end
   end
