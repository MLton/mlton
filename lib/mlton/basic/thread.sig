signature THREAD =
   sig
      include MLTON_THREAD

      (* generate f returns a function g, that when called runs f until f
       * either completes, in which case g returns NONE, or f calls its argument,
       * in which case g returns what f passes it, and then pauses f.
       *)
      val generate: (('a -> unit) -> unit) -> unit -> 'a option
   end
