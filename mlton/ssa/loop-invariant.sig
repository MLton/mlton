(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LOOP_INVARIANT_STRUCTS = 
   sig
      include SHRINK
   end

signature LOOP_INVARIANT = 
   sig
      include LOOP_INVARIANT_STRUCTS

      val loopInvariant: Program.t -> Program.t
   end
