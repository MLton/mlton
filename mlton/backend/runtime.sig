type int = Int.t
type word = Word.t
   
signature RUNTIME =
   sig
      val isValidObjectHeader: {numPointers: int,
				numWordsNonPointers: int} -> bool
      val isValidObjectSize: int -> bool
      val isValidArrayHeader: {numBytesNonPointers: int,
			       numPointers: int} -> bool
   end
