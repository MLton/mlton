(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature SMALL_INT_INF_STRUCTS = 
   sig
   end

signature SMALL_INT_INF = 
   sig
      include SMALL_INT_INF_STRUCTS
      
      type t

      val toCstring: t -> string
      val toMLstring: t -> string
      val toWord: t -> Word.t
   end
