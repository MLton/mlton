signature MLTON_ITIMER =
   sig
      datatype t =
	 Prof
       | Real
       | Virtual

      val set: t * {value: Time.time,
		    interval: Time.time} -> unit
      val signal: t -> Posix.Signal.signal
   end
