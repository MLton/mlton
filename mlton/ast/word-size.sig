type int = Int.t
   
signature WORD_SIZE_STRUCTS =
   sig
   end

signature WORD_SIZE =
   sig
      include WORD_SIZE_STRUCTS

      eqtype t
	 
      val all: t list
      val bits: t -> int
      val bytes: t -> int
      val cardinality: t -> IntInf.t
      val default: t
      val equals: t * t -> bool 
      val layout: t -> Layout.t
      val max: t -> IntInf.t
      val memoize: (t -> 'a) -> t -> 'a
      val pointer: unit -> t
      datatype prim = W8 | W16 | W32 | W64
      val prim: t -> prim
      val prims: t list
      val roundUpToPrim: t -> t
      val toString: t -> string
      val W: int -> t
   end
