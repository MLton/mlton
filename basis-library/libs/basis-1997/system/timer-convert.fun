functor TimerConvert
        (structure Timer: TIMER) :
        TIMER_1997 =
  struct
     open Timer

     val checkCPUTimer = fn cput =>
       let
	 val {usr, sys} = checkCPUTimer cput
	 val gc = checkGCTime cput
       in
	 {usr = usr, sys = sys, gc = gc}
       end
  end
