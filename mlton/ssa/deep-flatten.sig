(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature DEEP_FLATTEN_STRUCTS = 
   sig
      include SHRINK2
   end

signature DEEP_FLATTEN = 
   sig
      include DEEP_FLATTEN_STRUCTS
      
      val flatten: Program.t -> Program.t
   end
