type word = Word.word
   
signature MLTON_INT_INF =
   sig
      include INT_INF

       val gcd: int * int -> int 
       datatype rep =
 	  Small of word
 	| Big of word vector
       val rep: int -> rep
       val size: int -> Int.int
   end
