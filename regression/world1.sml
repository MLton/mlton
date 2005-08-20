fun run (f: unit -> unit) =
   case Posix.Process.fork () of
      SOME pid =>
         let
            open Posix.Process
            val (pid', status) = waitpid (W_CHILD pid, [])
         in if pid = pid' andalso status = W_EXITED
               then ()
            else raise Fail "child failed"
         end
    | NONE => let open OS.Process
              in exit ((f (); success) handle _ => failure)
              end

fun succeed () =
   let open OS.Process
   in exit success
   end

open MLton.World

val (w, out) = MLton.TextIO.mkstemp "/tmp/world"
val _ = TextIO.closeOut out

val _ =
   case save w of
      Clone => (print "I am the clone\n"; succeed ())
    | Original => print "I am the original\n"

val _ = run (fn () => load w)
   
val _ = OS.FileSys.remove w

