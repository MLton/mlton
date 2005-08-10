(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
