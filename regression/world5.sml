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

open MLton.World MLton.Signal Posix.Process Posix.ProcEnv

val (w, out) = MLton.TextIO.mkstemp "/tmp/world"
val _ = TextIO.closeOut out

val childReady = ref false

fun print s = TextIO.output (TextIO.stdErr, s)

val _ = Handler.set (usr1, Handler.simple (fn () => childReady := true))
   
val parent = getpid ()

val _ =
   case fork () of
      NONE => 
	 (Handler.set (usr1, Handler.simple (fn () => save w))
	  ; kill (K_PROC parent, usr1)
	  ; let
	       val rec loop =
		  fn 0 => print "success\n"
		   | n => loop (n - 1)
	    in loop 100000000
	    end
	  ; let open OS.Process
	    in exit success
	    end)
    | SOME child =>
	 let
	    fun loop () = if !childReady then () else loop ()
	 in loop ()
	    ; kill (K_PROC child, usr1)
	    ; wait ()
	    ; run (fn () => load w)
	    ; OS.FileSys.remove w
	 end
