(* Written by Stephen Weeks (sweeks@sweeks.com). *)

structure Main =
   struct
      fun doit () =
	 let
	    val len = 20000
	    val sum = len * (len - 1)
	    val v = Vector.tabulate (len, fn i => i)
	    fun loop n =
	       if n < 0
		  then ()
	       else
		  if sum = Vector.foldl (op +) 0 (Vector.concat [v, v])
		     then loop (n - 1)
		  else raise Fail "bug"
	    val numTrials = 100000
	 in loop numTrials
	 end
   end
