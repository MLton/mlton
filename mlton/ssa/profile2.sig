(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)


signature PROFILE2_STRUCTS = 
   sig
      include SHRINK2
   end

signature PROFILE2 = 
   sig
      include PROFILE2_STRUCTS

      val addProfile: Program.t -> Program.t
      val dropProfile: Program.t -> Program.t
   end
