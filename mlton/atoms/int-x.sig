type int = Int.t
type word = Word.t
   
signature INT_X_STRUCTS = 
   sig
      structure IntSize: INT_SIZE
   end

signature INT_X = 
   sig
      include INT_X_STRUCTS
	 
      (* Ints of all IntSize.t sizes. *)
      type t

      val + : t * t -> t 
      val - : t * t -> t 
      val * : t * t -> t
      val ~ : t -> t
      val > : t * t -> bool 
      val < : t * t -> bool 
      val >= : t * t -> bool 
      val <= : t * t -> bool
      val compare: t * t -> Relation.t
      val defaultInt: int -> t
      val equals: t * t -> bool
      val format: t * StringCvt.radix -> string
      val hash: t -> word
      val isMax: t -> bool
      val isMin: t -> bool
      val isNegOne: t -> bool
      val isOne: t -> bool
      val isZero: t -> bool
      val layout: t -> Layout.t
      val make: IntInf.t * IntSize.t -> t
      val max: IntSize.t -> t
      val min: IntSize.t -> t
      val one: IntSize.t -> t
      val quot: t * t -> t
      val rem: t * t -> t
      val size: t -> IntSize.t
      val toChar: t -> char
      val toInt: t -> int
      val toIntInf: t -> IntInf.t
      val toString: t -> string
      val zero: IntSize.t -> t
   end

