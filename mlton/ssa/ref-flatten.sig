(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature REF_FLATTEN_STRUCTS = 
   sig
      include TYPE_CHECK2
   end

signature REF_FLATTEN = 
   sig
      include REF_FLATTEN_STRUCTS
      
      val flatten: Program.t -> Program.t
   end
