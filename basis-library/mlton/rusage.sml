structure Rusage =
   struct
      open Primitive.MLton.Rusage

      type t = {utime: Time.time, stime: Time.time}

      fun toTime {sec, usec} =
	 let
	   val time_sec = Time.fromSeconds (LargeInt.fromInt sec)
	   val time_usec = Time.fromMicroseconds (LargeInt.fromInt usec)
	 in
	   Time.+ (time_sec, time_usec)
	 end

      fun rusage () =
         let val _ = ru ()
	 in {self = {utime = toTime {sec = self_utime_sec (),
				     usec = self_utime_usec ()},
		     stime = toTime {sec = self_stime_sec (),
				     usec = self_stime_usec ()}},
	     children = {utime = toTime {sec = children_utime_sec (),
					 usec = children_utime_usec ()},
			 stime = toTime {sec = children_stime_sec (),
					 usec = children_stime_usec ()}},
	     gc = {utime = toTime {sec = gc_utime_sec (),
				   usec = gc_utime_usec ()},
		   stime = toTime {sec = gc_stime_sec (),
				   usec = gc_stime_usec ()}}}
	 end
   end
