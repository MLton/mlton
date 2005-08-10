(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
type word = Word.t
   
signature INT_INF =
   sig
      include INTEGER

      val andb: t * t -> t
      val hash: t -> word
      val log2: t -> int
      val maxPow2ThatDivides: t -> word
      val notb: t -> t
      val orb: t * t -> t
      val xorb: t * t -> t
      val << : t * Pervasive.Word.word -> t
      val ~>> : t * Pervasive.Word.word -> t
   end
