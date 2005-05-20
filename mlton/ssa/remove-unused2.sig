(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature REMOVE_UNUSED2_STRUCTS = 
   sig
      include SHRINK2
   end

signature REMOVE_UNUSED2 = 
   sig
      include REMOVE_UNUSED2_STRUCTS
      
      val remove: Program.t -> Program.t
   end
