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
      fun spawn arg =
         let
            val _ = TextIO.flushOut (TextIO.stdOut)
            val _ = TextIO.flushOut (TextIO.stdErr)
         in
            MLton.Process.spawn
            {path = cmd, args = [cmd, arg]}
         end
      fun waitpid pid =
         let
            val (pid', status) =
               Posix.Process.waitpid (Posix.Process.W_CHILD pid, [])
            val () =
               if pid <> pid'
                  then raise Fail "reap: pid <> pid'"
               else ()
         in
            status
         end
      fun kill (pid, signal) =
         Posix.Process.kill (Posix.Process.K_PROC pid, signal)
      fun doTest (arg, withPid) =
         let
            val _ = print (concat ["testing ", arg, "...\n"])
            val pid = spawn arg
            val () = withPid pid
            val status = waitpid pid
            val _ = print (concat ["exit_status: ", statusToString status, "\n"])
         in
            ()
         end
      fun doSimpleTest arg = doTest (arg, fn _ => ())
   in
      print "spawn test:\n"
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
