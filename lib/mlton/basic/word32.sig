(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Pervasive.Int.int
type word = Pervasive.Word.word

signature WORD32 =
   sig
      include WORD

      val addCheck: t * t -> t (* may raise Overflow *)
      val fromWord8: Word8.t -> t
      (* fromWord8s f.  f 0 should return the least significant byte
       * and f 3 should return the most significant.
       *)
      val fromWord8s: (int -> Word8.t) -> t
      val log2: t -> t (* 2 ^ (log2 w) <= w < 2 ^ (1 + log2 w) *)
      val maxPow2ThatDivides: t -> word
      val toWord8: t -> Word8.t
      val rotateLeft: t * t -> t
      val roundDownToPowerOfTwo: t -> t
      val roundUpToPowerOfTwo: t -> t
   end

functor TestWord32 (S: WORD32 where type t = Word32.word): sig end =
struct

open S

val _ =
   Assert.assert
   ("TestWord32", fn () =>
    List.forall
    ([(4, 0wxabcdabcd, 0wxbcdabcda),
      (8, 0wxabcdabcd, 0wxcdabcdab),
      (12, 0wxabcdabcd, 0wxdabcdabc),
      (16, 0wxabcdabcd, 0wxabcdabcd),
      (20, 0wxabcdabcd, 0wxbcdabcda),
      (24, 0wxabcdabcd, 0wxcdabcdab),
      (28, 0wxabcdabcd, 0wxdabcdabc)],
     fn (s, w, w') =>
     equals(w', rotateLeft(w, fromInt s))))

end
