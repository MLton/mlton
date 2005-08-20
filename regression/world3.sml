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
   
exception Foo

fun f n =
   if n = 0
      then (case save w of
               Original => 0
             | Clone => raise Foo)
   else f (n - 1) + 1

val _ = (f 13; ()) handle Foo => (print "caught foo\n"; succeed ())

val _ = run (fn () => load w)
   
val _ = OS.FileSys.remove w
