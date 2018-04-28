(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure InitScript: INIT_SCRIPT =
struct

val messageStr = Trace.Immediate.messageStr
val op / = String./

(* Losely based on /etc/sysconfig/init *)
local
   open Console open Foreground CharRendition
   fun make (settings, msg) =
      let
         val color = concat [moveToColumn 60, "[  ", set settings, msg,
                            set [Default], "  ]\n"]
         val normal = concat [" [ ", msg, " ]\n"]
      in fn () =>
         print (case Process.getEnv "TERM" of
                  SOME "linux" => color
                | SOME "xterm" => color
                | _ => normal)
      end
in
   val succeed = make ([Bold, Foreground BrightGreen], "OK")
   val fail = make ([Bold, Foreground BrightRed], "FAILED")
   val warn = make ([Bold, Foreground Yellow], "PASSED")
end

fun wrap (th: unit -> unit): unit =
   ((th () handle e => (fail (); raise e))
    ; succeed ())

fun startStop {name, action, log, thunk, usage} =
   let
      val me = Pid.current ()
      fun getProc () =
         List.peek (Process.ps (), fn {name = n, pid, ...} =>
                    n = name andalso not (Pid.equals (me, pid)))
      val isRunning = isSome o getProc
      fun start () =
         if isRunning ()
            then print (concat [name, " is already running\n"])
         else
            wrap
            (fn () =>
             let
                val _ = print (concat ["Starting ", name, ":"])
                val _ = Out.close Out.error
                val _ = Out.set (Out.error, Out.openAppend log)
                val _ =
                   Process.doubleFork
                   (fn () =>
                    let
                       val _ = In.close In.standard
                       val _ = Out.close Out.standard
                       val _ = Posix.ProcEnv.setpgid {pid = NONE, pgid = NONE}
                       val _ =
                          Signal.setHandler
                          (Posix.Signal.term, Signal.Handler.handler (fn _ =>
                           Thread.new
                           (fn () =>
                            (messageStr "received Signal.term -- exiting"
                             ; Process.succeed ()))))
                    in
                       thunk ()
                    end)
             in ()
             end)
      fun status () =
         print (concat [name,
                        if isRunning ()
                           then " is running\n"
                        else " is not running\n"])
      fun stop () =
         case getProc () of
            NONE => print (concat [name, " is not running\n"])
          | SOME {pgrp, ...} => 
               wrap (fn () =>
                     (print (concat ["Shutting down ", name, ":"])
                      ; Process.signalGroup (pgrp, Posix.Signal.term)))
   in case action of
      "start" => start ()
    | "status" => status ()
    | "stop" => stop ()
    | _ => usage "must start|status|stop"
   end
end
