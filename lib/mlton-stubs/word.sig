type word = Word.word
   
signature MLTON_WORD =
   sig
      type t
	 
      val rol: t * word -> t
      val ror: t * word -> t
   end
