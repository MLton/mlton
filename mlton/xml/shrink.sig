(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SHRINK_STRUCTS = 
   sig
     include TYPE_CHECK
   end

signature SHRINK = 
   sig
      include SHRINK_STRUCTS

      val shrink: Program.t -> Program.t
   end
