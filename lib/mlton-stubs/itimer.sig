signature MLTON_ITIMER =
   sig
      datatype t =
	 Prof
       | Real
       | Virtual

      val set: t * {interval: Time.time,
		    value: Time.time} -> unit
      val signal: t -> Posix.Signal.signal
   end
