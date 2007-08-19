(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure WideChar = Char32
structure WideString = String32

functor WideChar_ChooseChar (A: CHOOSE_CHARN_ARG) :
   sig val f : WideChar.char A.t end =
   ChooseCharN_Char32 (A)

functor WideString_ChooseString (A: CHOOSE_STRINGN_ARG) :
   sig val f : WideString.string A.t end =
   ChooseStringN_String32 (A)
