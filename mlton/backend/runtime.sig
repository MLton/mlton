type int = Int.t
type word = Word.t
   
signature RUNTIME =
   sig
      (* All sizes are in bytes, unless they explicitly say "pointers". *)

      val arrayHeaderSize: int
      val isValidObjectHeader: {numPointers: int,
				numWordsNonPointers: int} -> bool
      (* objectSize does not include the header. *)
      val objectSize: {numPointers: int,
		       numWordsNonPointers: int} -> int
      val isValidArrayHeader: {numBytesNonPointers: int,
			       numPointers: int} -> bool
      val labelSize: int
      val maxFrameSize: int
      val objectHeaderSize: int
      val pointerSize: int
      val wordSize: int
   end
