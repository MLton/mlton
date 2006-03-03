(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Position = C_Off

functor Position_ChooseIntN (A: CHOOSE_INT_ARG) :
   sig val f : Position.int A.t end =
   C_Off_ChooseIntN (A)
