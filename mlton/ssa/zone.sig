(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature ZONE_STRUCTS = 
   sig
      include TYPE_CHECK2
   end

signature ZONE = 
   sig
      include ZONE_STRUCTS
      
      val zone: Program.t -> Program.t
   end
