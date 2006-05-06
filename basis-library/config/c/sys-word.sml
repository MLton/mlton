(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure SysWord = C_UIntmax

functor SysWord_ChooseWordN (A: CHOOSE_WORDN_ARG) :
   sig val f : SysWord.word A.t end =
   C_UIntmax_ChooseWordN (A)
