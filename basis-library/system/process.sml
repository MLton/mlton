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
	 
      structure Status =
	 struct
	    open Primitive.Status

	    val fromPosix =
	       fn es =>
	       let
		  datatype z = datatype Posix.Process.exit_status
	       in
		  case es of
		     W_EXITED => success
		   | W_EXITSTATUS w => fromInt (Word8.toInt w)
		   | W_SIGNALED _ => failure
		   | W_STOPPED _ => failure
	       end
	 end

      type status = Status.t

      val failure = Status.failure
      val success = Status.success
      fun isSuccess st = st = success
	 
      fun wait (pid: Pid.t): Status.t =
	 Status.fromPosix (#2 (waitpid (W_CHILD pid, [])))
	       
      fun system cmd =
	 let
	    val pid =
	       MLtonProcess.spawn {args = ["sh", "-c", cmd],
				   path = "/bin/sh"}
	    val old =
	       List.map (fn s => 
			 let
			    open Signal
			    val old = getHandler s
			    val _ = setHandler (s, Handler.ignore)
			 in
			    (s, old)
			 end)
	       [Posix.Signal.int, Posix.Signal.quit]
	 in
	    DynamicWind.wind (fn () => wait pid,
			      fn () => List.app Signal.setHandler old)
	 end

      val atExit = MLtonProcess.atExit
	 
      val exit = MLtonProcess.exit

      fun terminate x = Posix.Process.exit (Word8.fromInt (Status.toInt x))

      val getEnv = Posix.ProcEnv.getenv

      fun sleep t = if Time.<= (t, Time.zeroTime)
		       then ()
		    else (ignore (Posix.Process.sleep t); ())
   end
