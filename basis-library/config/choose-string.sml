(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CHOOSE_STRINGN_ARG =
   sig
      type 'a t
      val fString8: String8.string t
      val fString16: String16.string t
      val fString32: String32.string t
   end

functor ChooseStringN_String8 (A : CHOOSE_STRINGN_ARG) : 
   sig val f : String8.string A.t end = 
   struct val f = A.fString8 end
functor ChooseStringN_String16 (A : CHOOSE_STRINGN_ARG) : 
   sig val f : String16.string A.t end = 
   struct val f = A.fString16 end
functor ChooseStringN_String32 (A : CHOOSE_STRINGN_ARG) : 
   sig val f : String32.string A.t end = 
   struct val f = A.fString32 end
