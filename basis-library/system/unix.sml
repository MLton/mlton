(* Modified from SML/NJ sources by sweeks@research.nj.nec.com on 1998-9-4.
 * changed 1. signals
 *         2. IO
 * Further modified by sweeks@acm.org on 1999-12-10.
 *         1. Put back support for Signals
 *)

(* unix.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Unix: UNIX =
  struct

    structure PP = Posix.Process
    structure PE = Posix.ProcEnv
    structure PF = Posix.FileSys
    structure PIO = Posix.IO
    structure SS = Substring

    type signal = Posix.Signal.signal
    datatype exit_status = datatype Posix.Process.exit_status
    val fromStatus = Posix.Process.fromStatus

    structure Mask = MLton.Signal.Mask

    fun ('a, 'b) protect(f: 'a -> 'b) (x: 'a): 'b =
       let val _ = Mask.block Mask.all
       in DynamicWind.wind(fn () => f x, fn () => Mask.unblock Mask.all)
       end

    datatype ('a, 'b) proc = PROC of {pid: PP.pid,
				      ins: 'a,
				      outs: 'b}

    fun executeInEnv (cmd, argv, env) =
       if not(PF.access(cmd, [PF.A_EXEC]))
	  then PosixError.raiseSys PosixError.noent
       else
	  let
	     val p1 = PIO.pipe ()
	     val p2 = PIO.pipe ()
	     fun closep () = (PIO.close (#outfd p1); 
			      PIO.close (#infd p1);
			      PIO.close (#outfd p2); 
			      PIO.close (#infd p2))
	     val base = SS.string(SS.taker (fn c => c <> #"/") (SS.all cmd))
	     fun startChild () =
		case protect PP.fork () of
		   SOME pid => pid (* parent *)
		 | NONE => let
			      val oldin = #infd p1
			      val oldout = #outfd p2
			      val newin = PF.stdin
			      val newout = PF.stdout
			   in
			      PIO.close (#outfd p1);
			      PIO.close (#infd p2);
			      if (oldin = newin) then ()
			      else (PIO.dup2{old = oldin, new = newin};
				    PIO.close oldin);
			      if (oldout = newout) then ()
			      else (PIO.dup2{old = oldout, new = newout};
				    PIO.close oldout);
			      PP.exece (cmd, base :: argv, env)
			   end
	     (* end case *)
	     val _ = TextIO.flushOut TextIO.stdOut
	     val pid = (startChild ()) handle ex => (closep(); raise ex)
	     val ins = TextIO.newIn(#infd p2)
	     val outs = TextIO.newOut(#outfd p1)
          in
	     (* close the child-side fds *)
	     PIO.close (#outfd p2);
	     PIO.close (#infd p1);
	     (* set the fds close on exec *)
	     PIO.setfd (#infd p2, PIO.FD.flags [PIO.FD.cloexec]);
	     PIO.setfd (#outfd p1, PIO.FD.flags [PIO.FD.cloexec]);
	     PROC {
		   pid = pid,
		   ins = ins,
		   outs = outs
		   }
          end

    fun execute (cmd, argv) = executeInEnv (cmd, argv, PE.environ())

    fun textInstreamOf (PROC{ins, ...}) = ins
    fun binInstreamOf (PROC{ins, ...}) = ins
    fun textOutstreamOf (PROC{outs, ...}) = outs
    fun binOutstreamOf (PROC{outs, ...}) = outs
    fun streamsOf (PROC{ins, outs, ...}) = (ins, outs)

    fun reap (PROC{pid, ins, outs}) =
       (TextIO.closeIn ins
	; TextIO.closeOut outs
	; (* protect is probably too much; typically, one
	   * would only mask SIGINT, SIGQUIT and SIGHUP
	   *)
	protect OS_Process.wait pid)

    fun kill (PROC{pid, ...}, signal) = PP.kill (PP.K_PROC pid, signal)

    fun exit st = OS_Process.exit (Word8.toInt st)
  end (* structure Unix *)
