signature MLTON_WORD =
   sig
      type word
	 
      val ~ : word -> word
      val rol: word * Word.word -> word
      val ror: word * Word.word -> word
   end
