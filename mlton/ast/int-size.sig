type int = Int.t
   
signature INT_SIZE_STRUCTS =
   sig
   end

signature INT_SIZE =
   sig
      include INT_SIZE_STRUCTS
	 
      datatype t = I8 | I16 | I32 | I64

      val all: t list
      val bytes: t -> int
      val default: t
      val equals: t * t -> bool
      val isInRange: t * IntInf.t -> bool
      val layout: t -> Layout.t
      val max: t -> IntInf.t
      val memoize: (t -> 'a) -> t -> 'a
      val min: t -> IntInf.t
      val range: t -> IntInf.t * IntInf.t
      val size: t -> int
      val toString: t -> string
   end
