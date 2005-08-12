(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type word = Word.word
   
signature MLTON_WORD =
   sig
      type t
	 
      val rol: t * word -> t
      val ror: t * word -> t
   end
