structure Process: PROCESS =
struct

local
   open Trace.Immediate
in
   val message = message
   val messageStr = messageStr
end

structure Command =
   struct
      type t = In.t * Out.t -> unit

      fun layout _ = Layout.str "<command>"
   end

type command = Command.t

structure Pid = Pid

structure Status =
   struct
      open Posix.Process

      type t = exit_status

      fun toString (s: t): string =
	 case s of
	    W_EXITED => "exited"
	  | W_EXITSTATUS w => concat ["exit status ", Word8.toString w]
	  | W_SIGNALED s => concat ["signal ",
				    SysWord.toString (Posix.Signal.toWord s)]
	  | W_STOPPED s => concat ["stop signal ",
				   SysWord.toString (Posix.Signal.toWord s)]

      val layout = Layout.str o toString

      val success = W_EXITED
      val failure = W_EXITSTATUS 0w1
   end

fun succeed (): 'a =
   let open OS.Process
   in exit success
   end

val succeed = Trace.trace ("Process.succeed", Unit.layout, Unit.layout) succeed

(* This song and dance is so that succeed can have the right type, unit -> 'a,
 * instead of unit -> unit.
 *)
val succeed: unit -> 'a = fn () => (succeed (); raise Fail "can't get here")

fun fork (c: unit -> unit): Pid.t =
   case Posix.Process.fork () of
      NONE => (Trace.Immediate.inChildProcess ()
	       ; let open OS.Process
		 in exit ((c (); success) handle _ => failure)
		 end)
    | SOME pid => pid

val fork = Trace.trace ("Process.fork", Command.layout, Pid.layout) fork

fun closes l = List.foreach (l, FileDesc.close)

fun forkIn (c: Out.t -> unit): Pid.t * In.t =
   let
      val {infd, outfd} = FileDesc.pipe ()
      val pid = fork (fn () =>
		     (FileDesc.close infd
		      ; c (MLton.TextIO.newOut outfd)))
      val _ = FileDesc.close outfd
   in (pid, MLton.TextIO.newIn infd)
   end

fun forkOut (c: In.t -> unit): Pid.t * Out.t =
   let
      val {infd, outfd} = FileDesc.pipe ()
      val pid = fork (fn () =>
		      (FileDesc.close outfd
		       ; c (MLton.TextIO.newIn infd)))
      val _ = FileDesc.close infd
   in (pid, MLton.TextIO.newOut outfd)
   end

fun forkInOut (c: In.t * Out.t -> unit): Pid.t * In.t * Out.t =
   let
      val {infd = in1, outfd = out1} = FileDesc.pipe ()
      val {infd = in2, outfd = out2} = FileDesc.pipe ()
      val pid = fork (fn () =>
		      (closes [in1, out2]
		       ; c (MLton.TextIO.newIn in2, MLton.TextIO.newOut out1)))
      val _ = closes [in2, out1]
   in (pid, MLton.TextIO.newIn in1, MLton.TextIO.newOut out2)
   end

fun wait (p: Pid.t): unit =
   let val (p', s) = Posix.Process.waitpid (Posix.Process.W_CHILD p, [])
   in if p <> p'
	 then raise Fail (concat ["wait expected pid ",
				  Pid.toString p,
				  " but got pid ",
				  Pid.toString p'])
      else ()
	 ; (case s of
	       Status.W_EXITED => ()
	     | _ => raise Fail (concat [Status.toString s]))
   end

val wait = Trace.trace ("Process.wait", Pid.layout, Unit.layout) wait

val run = wait o fork

(* doubleFork avoids creating zombies. *)
fun doubleFork (c: unit -> unit): unit =
   run (fn () => (fork c; ()))

structure Posix =
   struct
      open Posix
      structure Process =
	 struct
	    open Process

	    val wait =
	       Trace.trace ("Posix.Process.wait", Unit.layout,
			   Layout.tuple2 (Pid.layout, Status.layout))
	       wait
	 end
   end

fun waits (pids: Pid.t list): unit =
   case pids of
      [] => ()
    | _ =>
	 let
	    val (pid, status) = Posix.Process.wait ()
	    val pids =
	       case status of
		  Posix.Process.W_EXITED =>
		     List.keepAll (pids, fn p => p <> pid)
		| _ => raise Fail (concat ["child ",
					 Pid.toString pid,
					 " failed with ",
					 Status.toString status])
	 in waits pids
	 end

fun pipe (cs: command list, ins: In.t, out: Out.t): unit =
   let
      fun loop (cs: command list,
	       ins: In.t,
	       maybeClose,
	       pids: Pid.t list): unit =
	 case cs of
	    [] => ()
	  | [c] => let val pid = fork (fn () => c (ins, out))
		       val _ = maybeClose ()
		   in waits (pid :: pids)
		   end
	  | c :: cs =>
	       let val (pid, ins) = forkIn (fn out => c (ins, out))
		  val _ = maybeClose ()
	       in loop (cs, ins, fn _ => In.close ins, pid :: pids)
	       end
   in loop (cs, ins, fn _ => (), [])
   end

fun pipe' cs = pipe (cs, In.standard, Out.standard)

local
   open DynamicWind
in
   val windFail = windFail
   val wind = wind
end

fun exec (c: string, a: string list, ins: In.t, out: Out.t): unit =
   let open FileDesc
   in if MLton.isMLton
	 then (move {from = MLton.TextIO.inFd ins,
		     to = stdin}
	       ; move {from = MLton.TextIO.outFd out,
		       to = stdout}
	       ; move {from = MLton.TextIO.outFd Out.error,
		       to = stderr})
      else ()
      ; (Posix.Process.execp (c, c :: a)
	 handle e => (messageStr (concat ["unable to exec ", c, "\n"])
		      ; raise e))
   end

val exec =
   Trace.trace4 ("Process.exec", String.layout, List.layout String.layout,
		 In.layout, Out.layout, Unit.layout)
   exec

fun call (c, a) (ins, out) = run (fn () => exec (c, a, ins, out))

fun call' ca = call ca (In.standard, Out.standard)

fun collect (c: Command.t): string =
   let val (pid, ins) = forkIn (fn out => c (In.standard, out))
   in In.inputAll ins before (In.close ins; wait pid)
   end

fun doesSucceed c = (run c; true) handle Fail _ => false

val doesSucceed =
   Trace.trace ("Process.doesSucceed", Function.layout, Bool.layout)
   doesSucceed

fun makeMain main () =
   (main (CommandLine.arguments ())
    ; succeed ())
   handle e =>
      ((* You shouldn't use Trace.Immediate.message here, because that only
	* outputs if MLton.debug = true.
	*)
       Layout.output (Exn.layout e, Out.error)
       ; Out.newline Out.error
       ; let open OS.Process
	 in exit failure
	 end)

fun basename s = #file (OS.Path.splitDirFile s)
   
val commandName = Promise.lazy (fn () => basename (CommandLine.name ()))

local open Posix.SysDB Posix.ProcEnv
in
   fun su (name: string): unit =
      let val p = getpwnam name
      in setgid (Passwd.gid p)
	 ; setuid (Passwd.uid p)
      end
   val su = Trace.trace ("su", String.layout, Unit.layout) su
   fun userName () = Passwd.name (getpwuid (getuid ()))
end

val getEnv = Posix.ProcEnv.getenv

fun glob (s: string): string list =
   String.tokens (collect (call ("bash", ["-c", "ls " ^ s])),
		 fn c => c = #"\n")

fun fail x = raise Fail x

fun usage {usage: string, msg: string}: 'a =
   fail (concat [msg, "\n", "Usage: ", commandName (), " ", usage])

val sleep = Posix.Process.sleep

fun watch (f: unit -> unit) =
   let
      fun loop () =
	 wait (fork f)
	 handle _ => (messageStr "watcher noticed child failure"
		      ; loop ())
   in loop ()
   end

fun signal (p: Pid.t, s: Signal.t): unit =
   let open Posix.Process
   in kill (K_PROC p, s)
   end

fun signalGroup (p: Pid.t, s: Signal.t): unit =
   let open Posix.Process
   in kill (K_GROUP p, s)
   end

local
   val delay = Time.fromMilliseconds (IntInf.fromInt 250)
   val maxDelay = Time.minutes (IntInf.fromInt 1)
in
   fun try (f: unit -> 'a, msg: string): 'a =
      let
	 fun loop (delay: Time.t): 'a =
	    if Time.> (delay, maxDelay)
	       then fail msg
	    else (f () handle _ => (sleep delay
				   ; loop (Time.+ (delay, delay))))
      in loop delay
      end
end

structure State =
   struct
      datatype t = DiskSleep | Running | Sleeping | Traced | Zombie

      fun fromString s =
	 case s of
	    "D" => SOME DiskSleep
	  | "R" => SOME Running
	  | "S" => SOME Sleeping
	  | "T" => SOME Traced
	  | "Z" => SOME Zombie
	  | _ => NONE

      val toString =
	 fn DiskSleep => "DiskSleep"
	  | Running => "Running"
	  | Sleeping => "Sleeping"
	  | Traced => "Traced"
	  | Zombie => "Zombie"

      val layout = Layout.str o toString
   end

val op / = String./
   
fun ps () =
   Dir.inDir
   ("/proc", fn () =>
    List.fold
    (Dir.lsDirs ".", [], fn (d, ac) =>
     case Pid.fromString d of
	NONE => ac
      | SOME pid =>
	   case String.tokens (hd (File.lines ("/proc"/d/"stat")),
			       Char.isSpace) of
	      _ :: name :: state :: ppid :: pgrp :: _ =>
		 {(* drop the ( ) around the name *)
		  name = String.substring (name, 1, String.size name - 2),
		  pgrp = valOf (Pid.fromString pgrp),
		  pid = pid,
		  ppid = valOf (Pid.fromString ppid),
		  state = valOf (State.fromString state)
		  } :: ac
	    | _ => fail "ps"))

val ps =
   Trace.trace
   ("ps", Unit.layout,
    List.layout (fn {name, pid, state, ...} =>
		 Layout.record [("pid", Pid.layout pid),
				("name", String.layout name),
				("state", State.layout state)]))
   ps

fun callWithIn (name, args, f: In.t -> 'a) =
   let
      val (pid, ins) =
	 forkIn (fn out => In.withNull (fn ins => call (name, args) (ins, out)))
   in DynamicWind.wind
      (fn () => In.withClose (ins, f),
       fn () => wait pid)
   end

fun callWithOut (name, args, f: Out.t -> 'a) =
   let
      val (pid, out) =
	 forkOut
	 (fn ins => Out.withNull (fn out => call (name, args) (ins, out)))
   in DynamicWind.wind
      (fn () => Out.withClose (out, fn () => f out),
       fn () => wait pid)
   end

(*
 * text	   data	    bss	    dec	    hex	filename
 * 3272995	 818052	  24120	4115167	 3ecadf	mlton
 *)
fun size (f: File.t): {text: int, data: int, bss: int}  =
   let
      val fail = fn () => fail (concat ["size failed on ", f])
   in callWithIn
      ("size", [f], fn ins =>
       case In.lines ins of
	  [_, nums] =>
	     (case String.tokens (nums, Char.isSpace) of
		 text :: data :: bss :: _ =>
		    (case (Int.fromString text,
			   Int.fromString data,
			   Int.fromString bss) of
			(SOME text, SOME data, SOME bss) =>
			   {text = text, data = data, bss = bss}
		      | _ => fail ())
	       | _ => fail ())
	| _ => fail ())
   end

fun time (f: unit -> unit) =
   let
      val {children = {utime = u, stime = s}, ...} = Time.times ()
      val _ = f ()
      val {children = {utime = u', stime = s'}, ...} = Time.times ()
   in
      {system = Time.- (s', s), user = Time.- (u', u)}
   end

fun setEnv z = raise Fail "setEnv unimplemented"
   (* FIXME MLton.ProcEnv.setenv *)

val exec = fn (c, a) => exec (c, a, In.standard, Out.standard)
   
end
