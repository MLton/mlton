(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHOOSE_REALN_ARG =
   sig
      type 'a t
      val fReal32: Real32.real t
      val fReal64: Real64.real t
   end

functor ChooseRealN_Real32 (A : CHOOSE_REALN_ARG) : 
   sig val f : Real32.real A.t end = 
   struct val f = A.fReal32 end
functor ChooseRealN_Real64 (A : CHOOSE_REALN_ARG) : 
   sig val f : Real64.real A.t end = 
   struct val f = A.fReal64 end
