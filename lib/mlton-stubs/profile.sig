type int = Int.int
type word = Word.word

signature MLTON_PROFILE =
   sig
      (* a compile-time constant *)
      val profile: bool

      structure Data:
         sig
            type t

            val equals: t * t -> bool
            val free: t -> unit
            val malloc: unit -> t
            val reset: t -> unit
            val write: t * string -> unit
         end
      
      val current: unit -> Data.t
      val setCurrent: Data.t -> unit
   end
