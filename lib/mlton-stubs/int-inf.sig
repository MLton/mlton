(* Copyright (C) 2002-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.int
type word = Word.word
   
signature MLTON_INT_INF =
   sig
      type t

      val areSmall: t * t -> bool
      val gcd: t * t -> t
      val isSmall: t -> bool
      datatype rep =
	 Big of word vector
       | Small of int
      val rep: t -> rep
   end
