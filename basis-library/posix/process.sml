(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure PosixProcess: POSIX_PROCESS =
   struct
      structure Prim = PosixPrimitive.Process
      open Prim
      structure Error = PosixError

      type signal = PosixSignal.signal
      type pid = Prim.pid

      val wordToPid = SysWord.toInt
      val pidToWord = SysWord.fromInt
	 
      fun fork() =
	 case Prim.fork() of
	    ~1 => Error.error()
	  | 0 => NONE
	  | n => SOME n

      val conv = String.nullTerm
      val convs = C.CSS.fromList

      fun exec(path, args): 'a =
	 (Error.checkResult(Prim.exec(conv path, convs args))
	  ; raise Fail "Posix.Process.exec")

      fun exece(path, args, env): 'a =
	 (Error.checkResult(Prim.exece(conv path, convs args, convs env))
	  ; raise Fail "Posix.Process.exece")
	 
      fun execp(file, args): 'a =
	 (Error.checkResult(Prim.execp(conv file, convs args))
	  ; raise Fail "Posix.Process.execp")

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

      structure W =
	 struct
	    open W PosixFlags
	 end

      local
	 val convertwa =
	    fn W_ANY_CHILD => ~1
	     | W_CHILD pid => pid
	     | W_SAME_GROUP => 0
	     | W_GROUP pid => ~ pid
	    
	 val status: status ref = ref 0

	 fun getStatus() =
	    let val status = !status
	    in if Prim.ifExited status
		  then (case Prim.exitStatus status of
			   0 => W_EXITED
			 | n => W_EXITSTATUS(Word8.fromInt n))
	       else if Prim.ifSignaled status
		       then W_SIGNALED(Prim.termSig status)
	       else if Prim.ifStopped status
		       then W_STOPPED(Prim.stopSig status)
		    else raise Fail "Posix.Process.waitpid"
	    end
      in
	 fun waitpid(wa, flags) =
	    let val pid = Prim.waitpid(convertwa wa, status,
				       SysWord.toInt(W.flags flags))
	    in Error.checkResult pid
	       ; (pid, getStatus())
	    end

	 fun waitpid_nh(wa, flags) =
	    let
	       val pid =
		  Prim.waitpid(convertwa wa, status,
			       SysWord.toInt(W.flags(wnohang :: flags)))
	    in Error.checkResult pid
	       ; if pid = 0
		    then NONE
		 else SOME(pid, getStatus())
	    end
      end

      fun wait() = waitpid(W_ANY_CHILD, [])

      fun exit (w: Word8.word): 'a  =
	 ((* A call to AtExit.clean is done before calling exit in
	   * system/process.sml, if necessary
	   *)
	  Prim.exit (Word8.toInt w)
	  ; raise Fail "Posix.Process.exit")

      datatype killpid_arg  =
	 K_PROC of pid
       | K_SAME_GROUP
       | K_GROUP of pid 

      fun kill(ka: killpid_arg, s: signal): unit =
	 let val pid = (case ka of
			   K_PROC pid => pid
			 | K_SAME_GROUP => ~1
			 | K_GROUP pid => ~ pid)
	 in Error.checkResult(Prim.kill(pid, s))
	 end

      local
	 fun wrap prim (t: Time.time): Time.time =
	    Time.fromSeconds
	    (LargeInt.fromInt(prim(LargeInt.toInt(Time.toSeconds t))))
      in
	 val alarm = wrap Prim.alarm
	 val sleep = wrap Prim.sleep
      end
	 
      fun pause() = Error.checkResult(Prim.pause())
   end
