(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Int = Int32
type int = Int.int

functor Int_ChooseInt (A: CHOOSE_INT_ARG) :
   sig val f : Int.int A.t end =
   ChooseInt_Int32 (A)
