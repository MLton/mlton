type int = Int.int
type word = Word.word
   
signature MLTON_RANDOM =
   sig
      (* Return a string of random alphanumeric characters of specified
       * length.
       *)
      val alphaNumString: int -> string
	 
      (* Get the next pseudrandom. *)
      val rand: unit -> word
	 
      (* Use /dev/random to get a word.  Useful as an arg to srand.
       * Return NONE if /dev/random can't be read.
       *)
      val seed: unit -> word option
	 
      (* Set the seed used by rand. *)
      val srand: word -> unit

      (* Use /dev/urandom to get a word.  Useful as an arg to srand.
       * Return NONE if /dev/urandom can't be read.
       *)
      val useed: unit -> word option
   end
