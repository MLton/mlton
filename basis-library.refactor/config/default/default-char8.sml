(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Char = Char8
type char = Char.char
structure String = String8
type string = String.string

functor Char_ChooseChar (A: CHOOSE_CHARN_ARG) :
   sig val f : Char.char A.t end =
   ChooseCharN_Char8 (A)

functor String_ChooseString (A: CHOOSE_STRINGN_ARG) :
   sig val f : String.string A.t end =
   ChooseStringN_String8 (A)
