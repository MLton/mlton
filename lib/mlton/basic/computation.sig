signature COMPUTATION = 
   sig
      structure Time: TIME
	 
      type t

      val keepAll: t * (string -> bool) -> t
      val inspect: t -> unit
      val output: t * Out.t -> unit
      val outputCalls: t * Out.t -> unit
      val outputTimes: t * Out.t -> unit
      val time: t -> Time.t
   end
