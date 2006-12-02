(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure FixedInt = Int64

functor FixedInt_ChooseIntN (A: CHOOSE_INTN_ARG) :
   sig val f : FixedInt.int A.t end =
   ChooseIntN_Int64 (A)
