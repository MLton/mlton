(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonSignal: MLTON_SIGNAL_EXTRA =
struct

open Posix.Signal
structure Prim = PrimitiveFFI.Posix.Signal
structure Error = PosixError
structure SysCall = Error.SysCall
val restart = SysCall.restartFlag

type t = signal

type how = C_Int.t

fun raiseInval () =
   let
      open PosixError
   in
      raiseSys inval
   end

val validSignals = 
   Array.tabulate 
   (C_Int.toInt Prim.NSIG, fn i => 
    SysCall.syscallErr
    ({clear = false, restart = false, errVal = C_Int.fromInt ~1}, fn () =>
     {return = Prim.sigismember (fromInt i),
      post = fn _ => true,
      handlers = [(Error.inval, fn () => false)]}))

structure Mask =
   struct
      datatype t =
         AllBut of signal list
       | Some of signal list

      val allBut = AllBut
      val some = Some

      val all = allBut []
      val none = some []

      fun read () =
         Some
         (Array.foldri
          (fn (i, b, sigs) =>
           if b
              then let
                      val s = fromInt i
                      val res =
                         SysCall.simpleResult
                         (fn () => Prim.sigismember s)
                   in
                      if res = C_Int.fromInt 1
                         then s::sigs
                         else sigs
                   end
              else sigs)
          []
          validSignals)

      fun write m =
         case m of
            AllBut signals =>
               (SysCall.simple Prim.sigfillset
                ; List.app (fn s => SysCall.simple (fn () => Prim.sigdelset s)) signals)
          | Some signals =>
               (SysCall.simple Prim.sigemptyset
                ; List.app (fn s => SysCall.simple (fn () => Prim.sigaddset s)) signals)

      local
         fun make (how: how) (m: t) =
            (write m; SysCall.simpleRestart (fn () => Prim.sigprocmask how))
      in
         val block = make Prim.SIG_BLOCK
         val unblock = make Prim.SIG_UNBLOCK
         val setBlocked = make Prim.SIG_SETMASK
         fun getBlocked () = (make Prim.SIG_BLOCK none; read ())
      end

      local
         fun member (sigs, s) = List.exists (fn s' => s = s') sigs
      in
         fun isMember (mask, s) =
            if Array.sub (validSignals, toInt s)
               then case mask of
                       AllBut sigs => not (member (sigs, s))
                     | Some sigs => member (sigs, s)              
               else raiseInval ()
      end
   end

structure Handler =
   struct
      datatype t =
         Default
       | Handler of MLtonThread.Runnable.t -> MLtonThread.Runnable.t
       | Ignore
       | InvalidSignal
   end

datatype handler = datatype Handler.t

local
   val r = ref C_Int.zero
in
   fun initHandler (s: signal): Handler.t =
      SysCall.syscallErr
      ({clear = false, restart = false, errVal = C_Int.fromInt ~1}, fn () =>
       {return = Prim.isDefault (s, r),
        post = fn _ => if !r <> C_Int.zero then Default else Ignore,
        handlers = [(Error.inval, fn () => InvalidSignal)]})
end

val (getHandler, setHandler, handlers) =
   let
      val handlers = Array.tabulate (C_Int.toInt Prim.NSIG, initHandler o fromInt)
      val _ =
         Cleaner.addNew
         (Cleaner.atLoadWorld, fn () =>
          Array.modifyi (initHandler o fromInt o #1) handlers)
   in
      (fn s: t => Array.sub (handlers, toInt s),
       fn (s: t, h) => if Primitive.MLton.Profile.isOn andalso s = prof
                          then raiseInval ()
                       else Array.update (handlers, toInt s, h),
       handlers)
   end

val gcHandler = ref Ignore

fun handled () =
   Mask.some
   (Array.foldri
    (fn (s, h, sigs) =>
     case h of 
        Handler _ => (fromInt s)::sigs
      | _ => sigs) [] handlers)

structure Handler =
   struct
      open Handler

      val default = Default
      val ignore = Ignore

      val isDefault = fn Default => true | _ => false
      val isIgnore = fn Ignore => true | _ => false

      val handler =
         (* This let is used so that Thread.setHandler is only used if
          * Handler.handler is used.  This prevents threads from being part
          * of every program.
          *)
         let
            (* As far as C is concerned, there is only one signal handler.
             * As soon as possible after a C signal is received, this signal
             * handler walks over the array of all SML handlers, and invokes any
             * one for which a C signal has been received.
             *
             * Any exceptions raised by a signal handler will be caught by
             * the topLevelHandler, which is installed in thread.sml.
             *)
            val _ =
               PosixError.SysCall.blocker :=
               (fn () => let
                            val m = Mask.getBlocked ()
                            val () = Mask.block (handled ())
                         in
                            fn () => Mask.setBlocked m
                         end)

            val () =
               MLtonThread.setSignalHandler
               (fn t =>
                let
                   val mask = Mask.getBlocked ()
                   val () = Mask.block (handled ())
                   val fs = 
                      case !gcHandler of
                         Handler f => if Prim.isPendingGC () <> C_Int.zero 
                                         then [f] 
                                         else []
                       | _ => []
                   val fs =
                      Array.foldri
                      (fn (s, h, fs) =>
                       case h of
                          Handler f =>
                             if Prim.isPending (fromInt s) <> C_Int.zero
                                then f::fs 
                                else fs
                        | _ => fs) fs handlers
                   val () = Prim.resetPending ()
                   val () = Mask.setBlocked mask
                in
                   List.foldl (fn (f, t) => f t) t fs
                end)
         in
            Handler
         end

      fun simple (f: unit -> unit) = handler (fn t => (f (); t))
   end

val setHandler = fn (s, h) =>
   case (getHandler s, h) of
      (InvalidSignal, _) => raiseInval ()
    | (_, InvalidSignal) => raiseInval ()
    | (Default, Default) => ()
    | (_, Default) => 
         (setHandler (s, Default)
          ; SysCall.simpleRestart (fn () => Prim.default s))
    | (Handler _, Handler _) =>
         setHandler (s, h)
    | (_, Handler _) =>
         (setHandler (s, h)
          ; SysCall.simpleRestart (fn () => Prim.handlee s))
    | (Ignore, Ignore) => ()
    | (_, Ignore) => 
         (setHandler (s, Ignore)
          ; SysCall.simpleRestart (fn () => Prim.ignore s))

fun suspend m =
   (Mask.write m
    ; Prim.sigsuspend ()
    ; MLtonThread.switchToSignalHandler ())

fun handleGC f =
   (Prim.handleGC ()
    ; gcHandler := Handler.simple f)

end
