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
