signature TIMER =
   sig
      type cpu_timer
      type real_timer

      val checkCPUTimer: cpu_timer -> {gc: Time.time,
				       sys: Time.time,
				       usr: Time.time}
      val checkRealTimer: real_timer -> Time.time 
      val startCPUTimer: unit -> cpu_timer 
      val startRealTimer: unit -> real_timer 
      val totalCPUTimer: unit -> cpu_timer 
      val totalRealTimer: unit -> real_timer 
  end
