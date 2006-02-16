(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
   
signature SIGNAL_CHECK_STRUCTS = 
   sig
      structure Rssa: RSSA
   end

signature SIGNAL_CHECK = 
   sig
      include SIGNAL_CHECK_STRUCTS

      val insert: Rssa.Program.t -> Rssa.Program.t
   end
