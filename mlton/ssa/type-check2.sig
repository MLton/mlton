(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature TYPE_CHECK2_STRUCTS = 
   sig
      include ANALYZE2
   end

signature TYPE_CHECK2 = 
   sig
      include TYPE_CHECK2_STRUCTS
      
      val typeCheck: Program.t -> unit
   end
