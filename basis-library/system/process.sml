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

      structure Signal = MLtonSignal
      type status = PreOS.Process.status

      val success: status = 0
      val failure: status = 1

      fun isSuccess st = st = success

      fun wait pid =
	 case #2 (waitpid (W_CHILD pid, [])) of
	    W_EXITED => success
	  | W_EXITSTATUS w => Word8.toInt w
	  | W_SIGNALED s => failure (* ?? *)
	  | W_STOPPED s => failure (* this shouldn't happen *)
	       
      fun system cmd =
	 let
	    val pid =
	       MLtonProcess.spawn {path = "/bin/sh",
				   args = ["sh", "-c", cmd]}
	    val old =
	       List.map (fn s => 
			 let
			    val old = Signal.getHandler s
			    val _ = Signal.ignore s
			 in (s, old)
			 end)
	       [Signal.int, Signal.quit]
	 in
	    DynamicWind.wind (fn () => wait pid,
			      fn () => List.app Signal.setHandler old)
	 end

      fun atExit f = Cleaner.addNew (Cleaner.atExit, f)
 
      val exit = MLtonProcess.exit

      fun terminate x = Posix.Process.exit (Word8.fromInt x)

      val getEnv = Posix.ProcEnv.getenv

      fun sleep t = if Time.<=(t, Time.zeroTime)
		       then ()
		    else (Posix.Process.sleep t; ())
   end
