signature MLTON_FINALIZE =
   sig
      val finalize: 'a * (unit -> unit) -> unit
   end
