type int = Int.t
type word = Word.t
   
signature WORD_X_STRUCTS = 
   sig
      structure WordSize: WORD_SIZE
   end

signature WORD_X = 
   sig
      include WORD_X_STRUCTS
	 
      (* Words of all WordSize.t sizes. *)
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
      val div: t * t -> t
      val equals: t * t -> bool
      val fromChar: char -> t (* returns a word of size 8 *)
      val fromLargeInt: IntInf.t * WordSize.t -> t
      val fromWord8: Word8.t -> t
      val isAllOnes: t -> bool
      val isOne: t -> bool
      val isMax: t -> bool
      val isZero: t -> bool
      val layout: t -> Layout.t
      val make: LargeWord.t * WordSize.t -> t
      val max: WordSize.t -> t
      val mod: t * t -> t
      val notb: t -> t
      val one: WordSize.t -> t
      val orb: t * t -> t
      val resize: t * WordSize.t -> t
      val resizeX: t * WordSize.t -> t
      val rol: t * t -> t
      val ror: t * t -> t
      val size: t -> WordSize.t
      val toChar: t -> char
      val toIntInf: t -> IntInf.t
      val toIntInfX: t -> IntInf.t
      val toLargeWord: t -> LargeWord.t
      val toString: t -> string
      val xorb: t * t -> t
      val zero: WordSize.t -> t
   end

