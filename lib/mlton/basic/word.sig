type int = Pervasive.Int.int
   
signature WORD =
   sig
      type t

      val << : t * t -> t 
      val >> : t * t -> t 
      val ~>> : t * t -> t 
      val + : t * t -> t 
      val - : t * t -> t 
      val * : t * t -> t 
      val > : t * t -> bool 
      val < : t * t -> bool 
      val >= : t * t -> bool 
      val <= : t * t -> bool 
      val andb: t * t -> t 
      val compare: t * t -> order 
      val div: t * t -> t
      val equals: t * t -> bool
      val fromChar: char -> t
      val fromInt: int -> t
      val fromString: string -> t option
      val layout: t -> Layout.t
      val max: t * t -> t
      val min: t * t -> t 
      val mod: t * t -> t
      val notb: t -> t
      val nthBitIsSet: t * int -> bool
      val orb: t * t -> t
      val rol: t * Pervasive.Word.word -> t
      val ror: t * Pervasive.Word.word -> t
      val toChar: t -> char
      val toInt: t -> int
      val toIntX: t -> int
      val toString: t -> string
      val wordSize: int
      val xorb: t * t -> t 
   end

signature WORD32 =
   sig
      include WORD

      val fromWord8: Word8.t -> t
      (* fromWord8s f.  f 0 should return the least significant byte
       * and f 3 should return the most significant.
       *)
      val fromWord8s: (int -> Word8.t) -> t
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
