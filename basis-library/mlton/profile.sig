type int = Int.int
type word = Word.word

signature MLTON_PROFILE =
   sig
      structure Data:
	 sig
	    type t

	    val equals: t * t -> bool
	    val free: t -> unit
	    val malloc: unit -> t
	    val write: t * string -> unit
	 end

      val current: unit -> Data.t
      val isOn: bool (* a compile-time constant *)
      val setCurrent: Data.t -> unit
   end
