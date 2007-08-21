(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Main = Main ()

val _ =
   let
      open Trace.Immediate
   in
      debug := Out Out.error
      ; flagged ()
      ; on []
   end
