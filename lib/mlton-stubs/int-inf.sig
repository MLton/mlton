(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
