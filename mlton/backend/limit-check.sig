(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t

signature LIMIT_CHECK_STRUCTS = 
   sig
      structure Rssa: RSSA
   end

signature LIMIT_CHECK = 
   sig
      include LIMIT_CHECK_STRUCTS

      val insert: Rssa.Program.t -> Rssa.Program.t
   end
