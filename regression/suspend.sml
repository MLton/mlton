open Posix.Signal MLton.Signal Posix.Process

val _ =
   case fork () of
      NONE =>
         (setHandler (int, Handler.simple (fn () => print "child got an int\n"))
          ; print "child suspending\n"
          ; suspend Mask.none
          ; print "done\n")
    | SOME pid =>
         (sleep (Time.fromSeconds 1)
          ; print "parent sending int\n"
          ; kill (K_PROC pid, int)
          ; wait ()
          ; print "done\n")
