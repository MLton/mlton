(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure LargeInt = IntInf

functor LargeInt_ChooseInt (A: CHOOSE_INT_ARG) :
   sig val f : LargeInt.int A.t end =
   ChooseInt_IntInf (A)
