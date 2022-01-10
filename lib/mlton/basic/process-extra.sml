(* Copyright (C) 2017,2022 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure ProcessExtra: PROCESS_EXTRA =
struct

open Process

local
   open Trace.Immediate
in
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
      type t = OS.Process.status
   end

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

val run = waitChildPid o fork

(* doubleFork avoids creating zombies. *)
fun doubleFork (c: unit -> unit): unit =
   run (fn () => ignore (fork c))

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
   in In.inputAll ins before (In.close ins; waitChildPid pid)
   end

fun doesSucceed c = (run c; true) handle Fail _ => false

val doesSucceed =
   Trace.trace ("Process.doesSucceed", Function.layout, Bool.layout)
   doesSucceed

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

local
   val z = Posix.ProcEnv.uname ()
   fun lookup s =
      case List.peek (z, fn (s', _) => s = s') of
         NONE => fail (concat [s, " unknown"])
       | SOME (_, s) => s
in
   fun hostName () = lookup "nodename"
end

fun glob (s: string): string list =
   String.tokens (collect (call ("bash", ["-c", "ls " ^ s])),
                 fn c => c = #"\n")

fun watch (f: unit -> unit) =
   let
      fun loop () =
         waitChildPid (fork f)
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

val sleep = Posix.Process.sleep

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

val getEnv = Posix.ProcEnv.getenv
val setEnv = MLton.ProcEnv.setenv

val exec = fn (c, a) => exec (c, a, In.standard, Out.standard)

end
