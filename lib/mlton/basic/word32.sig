type int = Pervasive.Int.int


signature WORD32 =
   sig
      include WORD

      val fromWord8: Word8.t -> t
      (* fromWord8s f.  f 0 should return the least significant byte
       * and f 3 should return the most significant.
       *)
      val fromWord8s: (int -> Word8.t) -> t
      val toWord8: t -> Word8.t
      val rotateLeft: t * t -> t
   end

functor TestWord32 (S: WORD32 where type t = Word32.word): sig end =
struct

open S

val _ =
   Assert.assert
   ("Word", fn () =>
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
