(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RSSA_TRANSFORM_STRUCTS = 
   sig
      include RSSA_RESTORE
   end

signature RSSA_TRANSFORM = 
   sig
      include RSSA_TRANSFORM_STRUCTS

      val transform: Program.t -> Program.t
   end
