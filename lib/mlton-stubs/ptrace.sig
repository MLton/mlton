(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
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
