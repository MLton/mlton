(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure PosixProcEnv: POSIX_PROC_ENV =
   struct
      structure Prim = PosixPrimitive.ProcEnv
      structure Error = PosixError
      structure CS = C.CS

      local open Prim
      in
	 type pid = pid
	 type uid = uid
	 type gid = gid
	 datatype file_desc = datatype file_desc
	 val getpgrp = getpgrp (* No error checking required *)
	 val getegid = getegid (* No error checking required *)
	 val geteuid = geteuid (* No error checking required *)
	 val getgid = getgid (* No error checking required *)
	 val getpid = getpid (* No error checking required *)
	 val getppid = getppid (* No error checking required *)
	 val getuid = getuid (* No error checking required *)
	 val setgid = Error.checkResult o setgid
	 val setsid = Error.checkReturnResult o setsid 
	 val setuid = Error.checkResult o setuid
      end

      local
	 val a: word array = Primitive.Array.array Prim.numgroups
      in
	 fun getgroups () =
	    let val n = Prim.getgroups a
	    in Error.checkResult n
	       ; Array.prefixToList (a, n)
	    end
      end

      fun id x = x
      val uidToWord = id 
      val wordToUid = id
      val gidToWord = id
      val wordToGid = id

      fun getlogin () =
	 let val cs = Prim.getlogin ()
	 in if Primitive.Cpointer.isNull cs
	       then raise (Error.SysErr ("no login name", NONE))
	    else CS.toString cs
	 end

      fun setpgid {pid, pgid} =
	 let
	    val f =
	       fn NONE => 0
		| SOME n => n
	 in Error.checkResult (Prim.setpgid (f pid, f pgid))
	 end

      local
	 structure Uname = Prim.Uname
      in
	 fun uname () =
	    (Error.checkResult (Uname.uname ());
	     [("sysname", CS.toString (Uname.sysname ())),
	      ("nodename", CS.toString (Uname.nodename ())),
	      ("release", CS.toString (Uname.release ())),
	      ("version", CS.toString (Uname.version ())),
	      ("machine", CS.toString (Uname.machine ()))])
      end

      val time = Time.now

      fun sysconf s =
	 case List.find (fn (_, s') => s = s') Prim.sysconfNames of
	    NONE => Error.raiseSys Error.inval
	  | SOME (n, _) =>
	       let val res = Prim.sysconf n
	       in Error.checkResult res;
		  SysWord.fromInt res
	       end
	       
      local
	 structure Tms = Prim.Tms

(*
	 val ticksPerSecond: LargeInt.int =
	    SysWord.toLargeInt (sysconf "CLK_TCK")

	 val millisecondsPerSecond: LargeInt.int = 1000
	    
	 fun cvt (ticks: int): Time.time =
	    Time.fromMilliseconds
	    (LargeInt.div
	     (LargeInt.fromInt ticks * millisecondsPerSecond,
	      ticksPerSecond))
*)
	 val ticksPerSec = Real.fromInt (SysWord.toIntX (sysconf "CLK_TCK"))
	 
	 fun cvt (ticks: word) =
	    Time.fromReal (Word.toReal ticks / ticksPerSec)
      in
	 fun times () =
	    let
	       val elapsed = Prim.times ()
	    in
	       {elapsed = cvt elapsed,
		utime = cvt (Tms.utime ()), 
		stime = cvt (Tms.stime ()), 
		cutime = cvt (Tms.cutime ()), 
		cstime = cvt (Tms.cstime ())}
	    end
      end

      fun environ () = C.CSS.toList Prim.environ

      fun getenv name =
	 let val cs = Prim.getenv (String.nullTerm name)
	 in if Primitive.Cpointer.isNull cs
	       then NONE
	    else SOME (CS.toString cs)
	 end

      fun ctermid () = CS.toString (Prim.ctermid ())

      fun isatty (FD n) = Prim.isatty n

      fun ttyname (FD n) =
	 let val cs = Prim.ttyname n
	 in if Primitive.Cpointer.isNull cs
	       then Error.error ()
	    else CS.toString cs
	 end
   end
