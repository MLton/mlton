(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure PosixProcEnv: POSIX_PROC_ENV =
   struct
      structure Prim = PosixPrimitive.ProcEnv
      structure Error = PosixError
      structure CS = C.CS

      type pid = Pid.t
	 
      local
	 open Prim
      in
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
	 val setuid = Error.checkResult o setuid
      end

      fun setsid () = Pid.fromInt (Error.checkReturnResult
				   (Pid.toInt (Prim.setsid ())))

      fun id x = x
      val uidToWord = id 
      val wordToUid = id
      val gidToWord = id
      val wordToGid = id

      local
	 val a: word array = Primitive.Array.array Prim.numgroups
      in
	 fun getgroups () =
	    let val n = Prim.getgroups a
	    in Error.checkResult n
	       ; ArraySlice.toList (ArraySlice.slice (a, 0, SOME n))
	    end
      end

      fun getlogin () =
	 let val cs = Prim.getlogin ()
	 in if Primitive.Pointer.isNull cs
	       then raise (Error.SysErr ("no login name", NONE))
	    else CS.toString cs
	 end

      fun setpgid {pid, pgid} =
	 let
	    val f =
	       fn NONE => Pid.fromInt 0
		| SOME pid => pid
	 in
	    Error.checkResult (Prim.setpgid (f pid, f pgid))
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

	 val ticksPerSec = Int.toLarge (SysWord.toIntX (sysconf "CLK_TCK"))

	 fun cvt (ticks: word) =
	    Time.fromTicks (LargeInt.quot
			    (LargeInt.* (Word.toLargeIntX ticks,
					 Time.ticksPerSecond),
			     ticksPerSec))
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
	 let
	    val cs = Prim.getenv (String.nullTerm name)
	 in
	    if Primitive.Pointer.isNull cs
	       then NONE
	    else SOME (CS.toString cs)
	 end

      fun ctermid () = CS.toString (Prim.ctermid ())

      fun isatty (FD n) = Prim.isatty n

      fun ttyname (FD n) =
	 let val cs = Prim.ttyname n
	 in if Primitive.Pointer.isNull cs
	       then Error.error ()
	    else CS.toString cs
	 end
   end
