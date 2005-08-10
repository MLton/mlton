(* Copyright (C) 2002-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type word = Word.word
   
signature MLTON_WORD =
   sig
      type t
	 
      val rol: t * word -> t
      val ror: t * word -> t
   end
