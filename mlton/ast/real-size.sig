type int = Int.t
   
signature REAL_SIZE_STRUCTS = 
   sig
   end

signature REAL_SIZE = 
   sig
      include REAL_SIZE_STRUCTS
      
      datatype t = R32 | R64

      val all: t list
      val bits: t -> int
      val bytes: t -> int
      val default: t
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val memoize: (t -> 'a) -> t -> 'a
      val toString: t -> string
   end
