signature MLTON_INT_INF =
   sig
      include INT_INF

       val gcd: int * int -> int 
       datatype rep =
 	  Small of Word.word
 	| Big of Word.word Vector.vector
       val rep: int -> rep
       val size: int -> Int.int
   end
