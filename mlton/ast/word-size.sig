type int = Int.t
type word = Word.t
   
signature WORD_SIZE_STRUCTS =
   sig
   end

signature WORD_SIZE =
   sig
      include WORD_SIZE_STRUCTS
	 
      datatype t = W8 | W16 | W32

      val all: t list
      val allOnes: t -> word
      val bytes: t -> int
      val default: t
      val equals: t * t -> bool
      val max: t -> word
      val memoize: (t -> 'a) -> t -> 'a
      val size: t -> int
      val toString: t -> string
   end
