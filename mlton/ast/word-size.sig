type int = Int.t
type word = Word.t
   
signature WORD_SIZE_STRUCTS =
   sig
   end

signature WORD_SIZE =
   sig
      include WORD_SIZE_STRUCTS
	 
      datatype t = W8 | W16 | W32 | W64

      val all: t list
      val allOnes: t -> LargeWord.t
      val bytes: t -> int
      val cardinality: t -> IntInf.t
      val default: t
      val equals: t * t -> bool
      val max: t -> LargeWord.t
      val memoize: (t -> 'a) -> t -> 'a
      val size: t -> int
      val toString: t -> string
   end
