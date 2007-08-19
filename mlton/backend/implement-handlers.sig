(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature IMPLEMENT_HANDLERS_STRUCTS = 
   sig
      structure Rssa: RSSA
   end

signature IMPLEMENT_HANDLERS = 
   sig
      include IMPLEMENT_HANDLERS_STRUCTS

      val doit: Rssa.Program.t -> Rssa.Program.t
   end
