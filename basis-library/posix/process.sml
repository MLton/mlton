(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure PosixProcess: POSIX_PROCESS_EXTRA =
   struct
      structure Prim = PosixPrimitive.Process
      open Prim
      structure Error = PosixError
      structure SysCall = Error.SysCall

      type signal = PosixSignal.signal
      type pid = Pid.t

      val wordToPid = Pid.fromInt o SysWord.toInt
      val pidToWord = SysWord.fromInt o Pid.toInt

      fun fork () =
	 SysCall.syscall
	 (fn () =>
	  let 
	     val p = Prim.fork ()
	     val p' = Pid.toInt p
	  in (p', fn () => if p' = 0 then NONE else SOME p)
	  end)

      val fork =
	 if Primitive.MLton.Platform.OS.forkIsEnabled
	    then fork
	 else fn () => Error.raiseSys Error.nosys

      val conv = NullString.nullTerm
      val convs = C.CSS.fromList

      fun exece (path, args, env): 'a =
	 let
	    val path = conv path
	    val args = convs args
	    val env = convs env
	 in
	    (SysCall.simple
	     (fn () => Prim.exece (path, args, env))
	     ; raise Fail "Posix.Process.exece")
	 end
	 
      fun exec (path, args): 'a =
	 exece (path, args, PosixProcEnv.environ ())

      fun execp (file, args): 'a =
	 let
	    val file = conv file
	    val args = convs args
	 in
	    (SysCall.simple 
	     (fn () => Prim.execp (file, args))
	     ; raise Fail "Posix.Process.execp")
	 end

      datatype waitpid_arg =
	 W_ANY_CHILD
       | W_CHILD of pid
       | W_SAME_GROUP
       | W_GROUP of pid 

      datatype exit_status =
	 W_EXITED
       | W_EXITSTATUS of Word8.word
       | W_SIGNALED of signal
       | W_STOPPED of signal 

      fun fromStatus status =
	 if Prim.ifExited status
	    then (case Prim.exitStatus status of
		     0 => W_EXITED
		   | n => W_EXITSTATUS (Word8.fromInt n))
	 else if Prim.ifSignaled status
	    then W_SIGNALED (Prim.termSig status)
	 else if Prim.ifStopped status
	    then W_STOPPED (Prim.stopSig status)
	 else raise Fail "Posix.Process.fromStatus"

      structure W =
	 struct
	    open W BitFlags
	 end

      local
	 val status: Status.t ref = ref (Status.fromInt 0)
	 fun wait (wa, status, flags) =
	    let
	       val p =
		  case wa of
		     W_ANY_CHILD => ~1
		   | W_CHILD pid => Pid.toInt pid
		   | W_SAME_GROUP => 0
		   | W_GROUP pid => ~ (Pid.toInt pid)
	       val flags = W.flags flags
	    in
	       SysCall.syscallRestart
	       (fn () =>
		let
		   val pid = Prim.waitpid (Pid.fromInt p, status,
					   SysWord.toInt flags)
		in
		   (Pid.toInt pid, fn () => pid)
		end)
	    end
	 fun getStatus () = fromStatus (!status)
      in
	 fun waitpid (wa, flags) =
	    let
	       val pid = wait (wa, status, flags)
	    in 
	       (pid, getStatus ())
	    end

	 fun waitpid_nh (wa, flags) =
	    let
	       val pid = wait (wa, status, wnohang :: flags)
	    in
	       if 0 = Pid.toInt pid
		  then NONE
	       else SOME (pid, getStatus ())
	    end
      end

      fun wait () = waitpid (W_ANY_CHILD, [])

      fun exit (w: Word8.word): 'a  =
	 (* Posix.Process.exit does not call atExit cleaners, as per the basis
	  * library spec.
	  *)
	 (Prim.exit (Word8.toInt w)
	  ; raise Fail "Posix.Process.exit")

      datatype killpid_arg  =
	 K_PROC of pid
       | K_SAME_GROUP
       | K_GROUP of pid 

      fun kill (ka: killpid_arg, s: signal): unit =
	 let
	    val pid =
	       case ka of
		  K_PROC pid => Pid.toInt pid
		| K_SAME_GROUP => ~1
		| K_GROUP pid => ~ (Pid.toInt pid)
	 in
	    SysCall.simple (fn () => Prim.kill (Pid.fromInt pid, s))
	 end

      local
	 fun wrap prim (t: Time.time): Time.time =
	    Time.fromSeconds
	    (LargeInt.fromInt 
	     (prim 
	      (LargeInt.toInt (Time.toSeconds t)
	       handle Overflow => Error.raiseSys Error.inval)))
      in
	 val alarm = wrap Prim.alarm
	 val sleep = wrap Prim.sleep
      end
	 
      (* FIXME: pause *)
      fun pause () =
	 SysCall.syscallErr
	 ({clear = false, restart = false},
	  fn () =>
	  {return = Prim.pause (),
	   post = fn () => (),
	   handlers = [(Error.intr, fn () => ())]})
   end
