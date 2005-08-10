(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature XML_TYPE =
   sig
      include HASH_TYPE
	 
      datatype dest =
	 Var of Tyvar.t
       | Con of Tycon.t * t vector
	 
      val dest: t -> dest
   end
