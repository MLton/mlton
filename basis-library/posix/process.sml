(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
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

      structure MLton = Primitive.MLton
	 
      fun fork () =
	 SysCall.syscall
	 (fn () =>
	  let 
	     val p = Prim.fork ()
	     val p' = Pid.toInt p
	  in (p', fn () => if p' = 0 then NONE else SOME p)
	  end)

      val fork =
	 (* Enable fork everywhere, and in particular, on Cygwin. *)
	 if true then fork
	 else 
	 if let open MLton.Platform.OS in host <> Cygwin end
	    then fork
 	 else
	    fn () =>
	    if true
	       then Error.raiseSys Error.nosys
	    else
	    (* On Cygwin, need to have the parent wait around until the child
	     * starts to work around a Cygwin fork/mmap bug.
	     * We accomplish this by creating a pipe that the child writes a
	     * single byte to and the parent reads from.
	     *)
	    let
	       val {infd, outfd} = PosixIO.pipe ()
	       fun close () = (PosixIO.close infd
			       ; PosixIO.close outfd)
	       fun doit () =
		  case fork () of
		     NONE => 
			(ignore (PosixIO.writeVec (outfd,
						   Word8VectorSlice.full
						   (Word8Vector.tabulate
						    (1, fn _ => 0w0))))
			 ; NONE)
		   | SOME n =>
			let
			   (* Wait in the parent until the child writes the
			    * byte to the pipe.  Need to restart the read system
			    * call in case the child also sends a signal.
			    *)
			   fun loop () =
			      (if 1 = Word8Vector.length (PosixIO.readVec
							  (infd, 1))
				  then ()
			       else raise Fail "bug in fork")
				  handle
				  e as (PosixError.SysErr (_, SOME err)) =>
				     if err = PosixError.intr
					then loop ()
				     else raise e
			   val _ = loop ()
			in
			   SOME n
			end
	    in
	       DynamicWind.wind (doit, close)
	    end

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
	    SysCall.simple
	    (fn () => Prim.kill (Pid.fromInt pid, s))
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
