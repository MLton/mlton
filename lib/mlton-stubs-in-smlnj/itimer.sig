signature MLTON_ITIMER =
   sig
      datatype which =
	 Prof
       | Real
       | Virtual

      val set: which * {value: Time.time,
		        interval: Time.time} -> unit
      val whichSignal: which -> Posix.Signal.signal
   end
