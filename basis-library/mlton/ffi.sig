signature MLTON_FFI =
   sig
      val handleCallFromC: (unit -> unit) -> unit
   end
