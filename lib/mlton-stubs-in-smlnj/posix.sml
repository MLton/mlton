structure Posix =
   struct
      open Posix

      structure ProcEnv =
	 struct
	    open ProcEnv

	    (* SML/NJ times is broken.  So it's probably best to ignore what
	     * it says and return zero.
	     *)
	    fun times () =
	       {cstime = Time.zeroTime,
		cutime = Time.zeroTime,
		elapsed = Time.zeroTime,
		stime = Time.zeroTime,
		utime = Time.zeroTime}
	 end
   end
