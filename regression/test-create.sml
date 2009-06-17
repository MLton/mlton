fun statusToString status =
   case status of
      Posix.Process.W_EXITED => "W_EXITED"
    | Posix.Process.W_EXITSTATUS w => concat ["W_EXITSTATUS ", Word8.toString w]
    | Posix.Process.W_SIGNALED s => 
         concat ["W_SIGNALED ", SysWord.toString (Posix.Signal.toWord s)]
    | Posix.Process.W_STOPPED s => 
         concat ["W_STOPPED ", SysWord.toString (Posix.Signal.toWord s)]

val cmd = CommandLine.name ()

fun stdout () =
   TextIO.output (TextIO.stdOut, "Hello world! [stdout]\n")
fun exit () = Posix.Process.exit 0wx7
fun diverge () = diverge ()

fun test () =
   let
      fun create arg =
         let
            val _ = TextIO.flushOut (TextIO.stdOut)
            val _ = TextIO.flushOut (TextIO.stdErr)
         in
            MLton.Process.create
            {path = cmd, 
             args = [arg],
             env = NONE,
             stdin = MLton.Process.Param.self,
             stdout = MLton.Process.Param.self,
             stderr = MLton.Process.Param.self}
         end
      fun reap pid =
         MLton.Process.reap pid
      fun kill (pid, signal) =
         MLton.Process.kill (pid, signal)
      fun doTest (arg, withPid) =
         let
            val _ = print (concat ["testing ", arg, "...\n"])
            val pid = create arg
            val () = withPid pid
            val status = reap pid
            val _ = print (concat ["exit_status: ", statusToString status, "\n"])
         in
            ()
         end
      fun doSimpleTest arg = doTest (arg, fn _ => ())
   in
      print "create test:\n"
      ; doSimpleTest "stdout"
      ; doSimpleTest "exit"
      ; doTest ("diverge", fn pid => kill (pid, Posix.Signal.kill))
   end

val _ =
   case CommandLine.arguments () of
      [] => test ()
    | ["stdout"] => stdout ()
    | ["exit"] => exit ()
    | ["diverge"] => diverge ()
    | _ => raise Match
