(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature CONST_TYPE_STRUCTS =
   sig
   end

signature CONST_TYPE =
   sig
      include CONST_TYPE_STRUCTS
	 
      datatype t = Bool | Real | String | Word

      val toString: t -> string
   end
