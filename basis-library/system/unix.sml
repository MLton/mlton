(* Modified from SML/NJ sources by sweeks@research.nj.nec.com on 1998-9-4.
 * changed 1. signals
 *         2. IO
 * Further modified by sweeks@acm.org on 1999-12-10.
 *         1. Put back support for Signals
 * Further modified by fluet@cs.cornell.edu on 2002-10-15.
 *         1. Adapted for new Basis Library specification.
 *)

(* unix.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Unix: UNIX =
   struct
      open Posix

      structure Status = Primitive.Status

      type signal = Signal.signal
      datatype exit_status = datatype Process.exit_status
      val fromStatus = Process.fromStatus

      structure Mask = MLtonSignal.Mask

      fun ('a, 'b) protect (f: 'a -> 'b) (x: 'a): 'b =
	 let
	    val _ = Mask.block Mask.all
	 in
	    DynamicWind.wind (fn () => f x, fn () => Mask.unblock Mask.all)
	 end

      datatype 'a str =
	 FD of FileSys.file_desc
	| STR of 'a * ('a -> unit)
	
      fun close str =
	 case str of
	    FD fd => IO.close fd
	  | STR (str, close) => close str
      
      datatype ('a, 'b) proc =
	 PROC of {ins: 'a str ref,
		  outs: 'b str ref,
		  pid: Process.pid,
		  status: OS.Process.status option ref}

      fun executeInEnv (cmd, argv, env) =
	 if not (FileSys.access (cmd, [FileSys.A_EXEC]))
	    then PosixError.raiseSys PosixError.noent
	 else
	    let
	       val p1 = IO.pipe ()
	       val p2 = IO.pipe ()
	       fun closep () =
		  (IO.close (#outfd p1)
		   ; IO.close (#infd p1)
		   ; IO.close (#outfd p2)
		   ; IO.close (#infd p2))
	       val base =
		  Substring.string (Substring.taker (fn c => c <> #"/")
				    (Substring.full cmd))
	       fun startChild () =
		  case protect Process.fork () of
		     SOME pid => pid (* parent *)
		   | NONE =>
			let
			   val oldin = #infd p1
			   val oldout = #outfd p2
			   val newin = FileSys.stdin
			   val newout = FileSys.stdout
			in
			   IO.close (#outfd p1)
			   ; IO.close (#infd p2)
			   ; if oldin = newin
				then ()
			     else (IO.dup2{old = oldin, new = newin}
				   ; IO.close oldin)
				; if oldout = newout
				     then ()
				  else (IO.dup2{old = oldout, new = newout}
					; IO.close oldout)
				     ; Process.exece (cmd, base :: argv, env)
			end
	       val _ = TextIO.flushOut TextIO.stdOut
	       val pid = (startChild ()) handle ex => (closep(); raise ex)
	       fun cloexec fd = IO.setfd (fd, IO.FD.flags [IO.FD.cloexec])
	    in
	       IO.close (#outfd p2)
	       ; IO.close (#infd p1)
	       ; cloexec (#infd p2)
	       ; cloexec (#outfd p1)
	       ; PROC {ins = ref (FD (#infd p2)),
		       outs = ref (FD (#outfd p1)),
		       pid = pid,
		       status = ref NONE}
	    end

      fun execute (cmd, argv) = executeInEnv (cmd, argv, ProcEnv.environ ())

      local
	 fun mkInstreamOf (newIn, closeIn) (PROC {ins, ...}) =
	    case !ins of
	       FD fd =>
		  let
		     val str = newIn (fd, "<process>")
		     val () = ins := STR (str, closeIn)
		  in
		     str
		  end
	     | STR (str, _) => str
	 fun mkOutstreamOf (newOut, closeOut) (PROC {outs, ...}) =
	    case !outs of
	       FD fd =>
		  let
		     val str = newOut (fd, "<process>")
		     val () = outs := STR (str, closeOut)
		  in
		     str
		  end
	     | STR (str, _) => str
      in
	 fun textInstreamOf proc =
	    mkInstreamOf (TextIO.newIn, TextIO.closeIn) proc
	 fun textOutstreamOf proc =
	    mkOutstreamOf (TextIO.newOut, TextIO.closeOut) proc
	 fun binInstreamOf proc =
	    mkInstreamOf (BinIO.newIn, BinIO.closeIn) proc
	 fun binOutstreamOf proc =
	    mkOutstreamOf (BinIO.newOut, BinIO.closeOut) proc
      end

      fun streamsOf pr = (textInstreamOf pr, textOutstreamOf pr)

      fun reap (PROC {pid, status, ins, outs}) =
	 case !status of
	    NONE =>
	       let
		  val _ = close (!ins)
		  val _ = close (!outs)
		  (* protect is probably too much; typically, one
		   * would only mask SIGINT, SIGQUIT and SIGHUP
		   *)
		  val st = protect OS.Process.wait pid
		  val _ = status := SOME st
	       in
		  st
	       end
	  | SOME status => status

      fun kill (PROC {pid, ...}, signal) =
	 Process.kill (Process.K_PROC pid, signal)

      fun exit (w: Word8.word): 'a =
	 OS.Process.exit (Status.fromInt (Word8.toInt w))
   end
