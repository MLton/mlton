type word = Word.word
   
signature MLTON_INT_INF =
   sig
      include INT_INF

       val areSmall: int * int -> bool
       val gcd: int * int -> int 
       val isSmall: int -> bool
       datatype rep =
	  Big of word vector
	| Small of Int.int
       val rep: int -> rep
       val size: int -> Int.int
   end
