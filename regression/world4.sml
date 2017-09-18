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

val (w1, out) = MLton.TextIO.mkstemp "/tmp/world"
val _ = TextIO.closeOut out
val (w2, out) = MLton.TextIO.mkstemp "/tmp/world"
val _ = TextIO.closeOut out

val _ = print "before saves\n"

val original = ref true
   
val _ = (case save w1 of
            Clone => original := false
          | Original => ())

val _ = print "between saves\n"

val _ = (case save w2 of
            Clone => original := false
          | Original => ())

val _ = print "after saves\n"

val _ = if !original
           then (run (fn () => load w1)
                 ; run (fn () => load w2)
                 ; OS.FileSys.remove w1
                 ; OS.FileSys.remove w2)
        else ()
