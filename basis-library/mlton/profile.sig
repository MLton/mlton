type int = Int.int
type word = Word.word

signature MLTON_PROFILE =
   sig
      structure Data: MLTON_PROFILE_DATA

      val current: unit -> Data.t
      val isOn: bool (* a compile-time constant *)
      val setCurrent: Data.t -> unit
   end
