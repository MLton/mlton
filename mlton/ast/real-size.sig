type int = Int.t
type word = Word.t
   
signature REAL_SIZE_STRUCTS = 
   sig
   end

signature REAL_SIZE = 
   sig
      include REAL_SIZE_STRUCTS
      
      datatype t = R32 | R64

      val all: t list
      val bytes: t -> int
      val default: t
      val equals: t * t -> bool
      val memoize: (t -> 'a) -> t -> 'a
      val toString: t -> string
   end
