(* Copyright (C) 2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
type word = Word.t
   
signature PROFILE_ALLOC_STRUCTS = 
   sig
      structure Rssa: RSSA
   end

signature PROFILE_ALLOC = 
   sig
      include PROFILE_ALLOC_STRUCTS
      
      val doit: Rssa.Program.t -> Rssa.Program.t
   end
