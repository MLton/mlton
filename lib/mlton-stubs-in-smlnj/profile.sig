type int = Int.int
type word = Word.word

signature MLTON_PROFILE =
   sig
      (* a compile-time constant, like MLton.debug *)
      val profile: bool

      (* reset all the bin counters to 0 *)
      val reset: unit -> unit
      (* write out all of the bins to a mlmon.out format file,
       * with the given file name
       *)
      val write: string -> unit
   end
