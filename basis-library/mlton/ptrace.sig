(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_PTRACE =
   sig
      type pid

      val attach: pid -> unit
      val cont: pid -> unit
      val detach: pid -> unit
      val kill: pid -> unit
      val peekText: pid * Word.word -> Word.word
      val singleStep: pid -> unit
      val sysCall: pid -> unit
   end
