(* Modified from SML/NJ sources by sweeks@research.nj.nec.com on 1998-6-25.
 * Further modified by sweeks@acm.org 1999-12-10, 2000-1-18
 *)

(* os-process.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The Posix-based implementation of the generic process control
 * interface (OS.Process).
 *
 *)

structure OS_Process: OS_PROCESS_EXTRA =
   struct
      open Posix.Process

      structure Signal = MLton.Signal
      structure Handler = Signal.Handler
      type status = int

      val success: status = 0
      val failure: status = 1

      fun wait pid =
	 case #2 (waitpid (W_CHILD pid, [])) of
	    W_EXITED => success
	  | W_EXITSTATUS w => Word8.toInt w
	  | W_SIGNALED s => failure (* ?? *)
	  | W_STOPPED s => failure (* this shouldn't happen *)
	       
      fun system cmd =
	 case fork () of
	    NONE => exec ("/bin/sh", ["sh", "-c", cmd])
	  | SOME pid =>
	       let
		  val old =
		     List.map (fn s => 
			       let val old = Handler.get s
				  val _ = Handler.set (s, Handler.Ignore)
			       in (s, old)
			       end)
		     [Signal.int, Signal.quit]
	       in DynamicWind.wind (fn () => wait pid,
				   fn () => List.app Handler.set old)
	       end

      fun atExit f = Cleaner.addNew (Cleaner.atExit, f)

      fun terminate x = exit (Word8.fromInt x)

      fun exit sts =
	 if 0 <= sts andalso sts < 256
	    then (let open Cleaner in clean atExit end
		     ; Primitive.halt sts
		     ; raise Fail "exit")
	 else raise Fail (concat ["exit must have 0 <= status < 256: saw ",
				  Int.toString sts])

      val getEnv = Posix.ProcEnv.getenv
   end
