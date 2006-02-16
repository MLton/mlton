signature TIMER =
   sig
      type cpu_timer
      type real_timer

      val checkCPUTimer: cpu_timer -> {sys: Time.time, usr: Time.time}
      val checkCPUTimes: cpu_timer -> {gc: {sys: Time.time,
                                             usr: Time.time},
                                       nongc: {sys: Time.time,
                                               usr: Time.time}}
      val checkGCTime: cpu_timer -> Time.time
      val checkRealTimer: real_timer -> Time.time
      val startCPUTimer: unit -> cpu_timer
      val startRealTimer: unit -> real_timer
      val totalCPUTimer: unit -> cpu_timer
      val totalRealTimer: unit -> real_timer
  end
