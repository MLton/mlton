open MLton.Signal Posix.Process

val _ =
   case fork () of
      NONE =>
	 (handleWith (int, fn () => print "child got an int\n")
 	  ; print "child suspending\n"
 	  ; suspend Mask.none
 	  ; print "child done\n")
    | SOME pid =>
	 (print "parent sleeping\n"
	  ; sleep (Time.fromSeconds 1)
	  ; print "parent sending int\n"
	  ; kill (K_PROC pid, int)
	  ; print "parent done\n")
