type int = Pervasive.Int.int
   
signature WORD =
   sig
      type t

      val << : t * Pervasive.Word.word -> t
      val >> : t * Pervasive.Word.word -> t
      val ~>> : t * Pervasive.Word.word -> t
      val + : t * t -> t 
      val - : t * t -> t 
      val * : t * t -> t 
      val > : t * t -> bool 
      val < : t * t -> bool 
      val >= : t * t -> bool 
      val <= : t * t -> bool 
      val andb: t * t -> t 
      val compare: t * t -> order 
      val div: t * t -> t
      val equals: t * t -> bool
      val fromChar: char -> t
      val fromInt: int -> t
      val fromWord: Pervasive.Word.word -> t
      val fromString: string -> t option
      val layout: t -> Layout.t
      val max: t * t -> t
      val min: t * t -> t 
      val mod: t * t -> t
      val notb: t -> t
      val nthBitIsSet: t * int -> bool
      val orb: t * t -> t
      val rol: t * Pervasive.Word.word -> t
      val ror: t * Pervasive.Word.word -> t
      val toChar: t -> char
      val toInt: t -> int
      val toIntX: t -> int
      val toWord: t -> Pervasive.Word.word
      val toWordX: t -> Pervasive.Word.word
      val toString: t -> string
      val wordSize: int
      val xorb: t * t -> t 
   end
