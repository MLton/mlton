type int = Int.t

signature COUNTER =
   sig
      type t
	 
      val new: int -> t
      val next: t -> int
      val tick: t -> unit
      val reset: t * int -> unit
      val value: t -> int
      val equals: t * t -> bool
   end
