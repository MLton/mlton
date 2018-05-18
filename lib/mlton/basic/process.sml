(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Process: PROCESS =
struct

local
   open Trace.Immediate
in
   val messageStr = messageStr
end

fun system s =
   let
      val status = OS.Process.system s
   in
      if OS.Process.isSuccess status
         then ()
      else Error.bug (concat ["Process.system: command failed: ", s])
   end

structure Command =
   struct
      type t = In.t * Out.t -> unit

      fun layout _ = Layout.str "<command>"
   end

type command = Command.t

structure Pid = Pid

structure PosixStatus =
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
   end

structure Status =
   struct
      type t = OS.Process.status
   end

fun succeed (): 'a =
   let open OS.Process
   in exit success
   end

val succeed = Trace.trace ("Process.succeed", Unit.layout, Unit.layout) succeed

(* This song and dance is so that succeed can have the right type, unit -> 'a,
 * instead of unit -> unit.
 *)
val succeed: unit -> 'a = fn () => (succeed (); Error.bug "Process.succeed")

fun fork (c: unit -> unit): Pid.t =
   case Posix.Process.fork () of
      NONE => (Trace.Immediate.inChildProcess ()
               ; let open OS.Process
                 in exit ((c (); success) handle _ => failure)
                 end)
    | SOME pid => pid

val fork = Trace.trace ("Process.fork", Command.layout, Pid.layout) fork

fun closes l = List.foreach (l, FileDesc.close)

val pname = "<process>"

fun forkIn (c: Out.t -> unit): Pid.t * In.t =
   let
      val {infd, outfd} = FileDesc.pipe ()
      val pid = fork (fn () =>
                      (FileDesc.close infd
                       ; c (MLton.TextIO.newOut (outfd, pname))))
      val _ = FileDesc.close outfd
   in
      (pid, MLton.TextIO.newIn (infd, pname))
   end

fun forkOut (c: In.t -> unit): Pid.t * Out.t =
   let
      val {infd, outfd} = FileDesc.pipe ()
      val pid = fork (fn () =>
                      (FileDesc.close outfd
                       ; c (MLton.TextIO.newIn (infd, pname))))
      val _ = FileDesc.close infd
   in
      (pid, MLton.TextIO.newOut (outfd, pname))
   end

fun forkInOut (c: In.t * Out.t -> unit): Pid.t * In.t * Out.t =
   let
      val {infd = in1, outfd = out1} = FileDesc.pipe ()
      val {infd = in2, outfd = out2} = FileDesc.pipe ()
      val pid = fork (fn () =>
                      (closes [in1, out2]
                       ; c (MLton.TextIO.newIn (in2, pname),
                            MLton.TextIO.newOut (out1, pname))))
      val _ = closes [in2, out1]
   in (pid,
       MLton.TextIO.newIn (in1, pname),
       MLton.TextIO.newOut (out2, pname))
   end

fun wait (p: Pid.t): unit =
   let val (p', s) = Posix.Process.waitpid (Posix.Process.W_CHILD p, [])
   in if p <> p'
         then Error.bug (concat ["Process.wait: expected pid ",
                                  Pid.toString p,
                                  " but got pid ",
                                  Pid.toString p'])
      else ()
         ; (case s of
               PosixStatus.W_EXITED => ()
             | _ => raise Fail (concat [PosixStatus.toString s]))
   end

val wait = Trace.trace ("Process.wait", Pid.layout, Unit.layout) wait

val run = wait o fork

(* doubleFork avoids creating zombies. *)
fun doubleFork (c: unit -> unit): unit =
   run (fn () => ignore (fork c))

structure Posix =
   struct
      open Posix
      structure Process =
         struct
            open Process

            val wait =
               Trace.trace ("Process.Posix.Process.wait", Unit.layout,
                           Layout.tuple2 (Pid.layout, PosixStatus.layout))
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
                | _ => Error.bug (concat ["Process.waits: child ",
                                          Pid.toString pid,
                                          " failed with ",
                                          PosixStatus.toString status])
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

fun exec (c: string, a: string list, ins: In.t, out: Out.t): unit =
   let
      open FileDesc
   in
      if MLton.isMLton
         then (move {from = MLton.TextIO.inFd ins,
                     to = stdin}
               ; move {from = MLton.TextIO.outFd out,
                       to = stdout}
               ; move {from = MLton.TextIO.outFd Out.error,
                       to = stderr})
      else ()
      ; (Posix.Process.execp (c, c :: a)
         handle _ => (Out.output (Out.error,
                                  (concat ("unable to exec "
                                           :: List.separate (c :: a, " "))))
                      ; OS.Process.exit OS.Process.failure))
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

fun makeCommandLine (commandLine: string list -> unit) args =
   ((commandLine args; OS.Process.success)
    handle e =>
       let
          val out = Out.error
       in
          Out.output (out, concat ["unhandled exception: ", Exn.toString e, "\n"])
          ; (case Exn.history e of
                [] => ()
              | l => (Out.output (out, "with history: \n")
                      ; List.foreach
                        (l, fn s =>
                         Out.output (out, concat ["\t", s, "\n"]))))
          ; OS.Process.failure
       end)

fun makeMain z (): unit =
   OS.Process.exit (makeCommandLine z (CommandLine.arguments ()))

fun basename s = #file (OS.Path.splitDirFile s)

val commandName = Promise.lazy (fn () => basename (CommandLine.name ()))

local open Posix.SysDB Posix.ProcEnv
in
   fun su (name: string): unit =
      let val p = getpwnam name
      in setgid (Passwd.gid p)
         ; setuid (Passwd.uid p)
      end
   val su = Trace.trace ("Process.su", String.layout, Unit.layout) su
   fun userName () = Passwd.name (getpwuid (getuid ()))
end

fun fail x = raise Fail x

local
   val z = Posix.ProcEnv.uname ()
   fun lookup s =
      case List.peek (z, fn (s', _) => s = s') of
         NONE => fail (concat [s, " unknown"])
       | SOME (_, s) => s
in
   fun hostName () = lookup "nodename"
end

val getEnv = Posix.ProcEnv.getenv

fun glob (s: string): string list =
   String.tokens (collect (call ("bash", ["-c", "ls " ^ s])),
                 fn c => c = #"\n")

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
   val delay = Time.fromMilliseconds 1
   val maxDelay = Time.minutes 1
in
   fun try (f: unit -> 'a, msg: string): 'a =
      let
         fun loop (delay: Time.t): 'a =
            if Time.> (delay, maxDelay)
               then fail msg
            else (f () handle _ => (ignore (sleep delay)
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
   ("Process.ps", Unit.layout,
    List.layout (fn {name, pid, state, ...} =>
                 Layout.record [("pid", Pid.layout pid),
                                ("name", String.layout name),
                                ("state", State.layout state)]))
   ps

fun callWithIn (name, args, f: In.t -> 'a) =
   let
      val pid = Unix.execute (name, args)
      val ins = Unix.textInstreamOf pid
   in
      Exn.finally
      (fn () => f ins,
       fn () => ignore (Unix.reap pid))
   end

fun callWithOut (name, args, f: Out.t -> 'a) =
   let
      val pid = Unix.execute (name, args)
      val out = Unix.textOutstreamOf pid
   in
      Exn.finally
      (fn () => f out,
       fn () => ignore (Unix.reap pid))
   end

(*
 * text    data     bss     dec     hex filename
 * 3272995       818052   24120 4115167  3ecadf mlton
 *)
fun size (f: File.t): {text: int, data: int, bss: int}  =
   let
      val fail = fn () => fail (concat ["size failed on ", f])
   in
      File.withTemp
      (fn sizeRes =>
       let
          val _ = OS.Process.system (concat ["size ", f, ">", sizeRes])
       in
          File.withIn
          (sizeRes, fn ins =>
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
       end)
   end

fun time (f: unit -> unit) =
   let
      val {children = {utime = u, stime = s}, ...} = Time.times ()
      val _ = f ()
      val {children = {utime = u', stime = s'}, ...} = Time.times ()
   in
      {system = Time.- (s', s), user = Time.- (u', u)}
   end

val setEnv = MLton.ProcEnv.setenv

val exec = fn (c, a) => exec (c, a, In.standard, Out.standard)

local
   open MLton.Process
in
   val spawn = spawn
   val spawne = spawne
   val spawnp = spawnp
end

end
