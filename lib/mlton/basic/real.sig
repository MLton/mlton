signature REAL =
   sig
      type int = Int.t
	 
      include ORDERED_FIELD

      structure Format:
	 sig
	    type t

	    val sci: int option -> t
	    val fix: int option -> t
	    val gen: int option -> t
	 end

      val acos: t -> t
      val asin: t -> t
      val atan: t -> t
      val atan2: t * t -> t
      val ceiling: t -> int
      val choose: t * t -> t
      val cos: real -> real
      val exp: t -> t
      val floor: t -> int
      val format: t * Format.t -> string
      val fromString: string -> t option
      exception Input
      val input: In0.t -> t
      val isFinite: t -> bool
      val ln: t -> t
      val log: t * t -> t
      val log2: t -> t
      val maxFinite: t
      val pi: t
      val realMod: t -> t
      val realPower: t * t -> t
      val round: t -> int
      val sin: real -> real
      val sqrt: t -> t
      val tan: t -> t
      val toString: t -> string
      val trunc: t -> int
   end
