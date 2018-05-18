(* Copyright (C) 2010 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHOOSE_CHARN_ARG =
   sig
      type 'a t
      val fChar8: Char8.char t
      val fChar16: Char16.char t
      val fChar32: Char32.char t
   end

functor ChooseCharN_Char8 (A : CHOOSE_CHARN_ARG) : 
   sig val f : Char8.char A.t end = 
   struct val f = A.fChar8 val _ = A.fChar16 val _ = A.fChar32 end
functor ChooseCharN_Char16 (A : CHOOSE_CHARN_ARG) : 
   sig val f : Char16.char A.t end = 
   struct val _ = A.fChar8 val f = A.fChar16 val _ = A.fChar32 end
functor ChooseCharN_Char32 (A : CHOOSE_CHARN_ARG) : 
   sig val f : Char32.char A.t end = 
   struct val _ = A.fChar8 val _ = A.fChar16 val f = A.fChar32 end
