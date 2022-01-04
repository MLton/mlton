(* Copyright (C) 2009,2022 Matthew Fluet.
 * Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_EXN =
   sig
      val history: exn -> string list
   end
