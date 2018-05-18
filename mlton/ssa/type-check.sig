(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TYPE_CHECK_STRUCTS = 
   sig
      include ANALYZE
   end

signature TYPE_CHECK = 
   sig
      include TYPE_CHECK_STRUCTS

      val typeCheck: Program.t -> unit
   end
