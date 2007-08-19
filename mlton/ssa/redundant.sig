(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REDUNDANT_STRUCTS = 
   sig
      include SHRINK
   end

signature REDUNDANT = 
   sig
      include REDUNDANT_STRUCTS

      val redundant: Program.t -> Program.t
   end
