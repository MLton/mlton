(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Word = Word32
type word = Word.word

functor Word_ChooseWordN (A: CHOOSE_WORDN_ARG) :
   sig val f : Word.word A.t end =
   ChooseWordN_Word32 (A)
