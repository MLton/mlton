(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
type word = Word.t
   
signature PROFILE_STRUCTS = 
   sig
      structure Machine: MACHINE
      structure Rssa: RSSA
      sharing Machine.ProfileLabel = Rssa.ProfileLabel
   end

signature PROFILE = 
   sig
      include PROFILE_STRUCTS
      
      val profile:
         Rssa.Program.t
         -> Rssa.Program.t * ({frames: Rssa.Label.t vector}
                              -> Machine.ProfileInfo.t option)
   end
