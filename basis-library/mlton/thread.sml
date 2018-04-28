(* Copyright (C) 2014 Matthew Fluet.
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonThread:> MLTON_THREAD_EXTRA =
struct

structure Prim = Primitive.MLton.Thread

fun die (s: string): 'a =
   (PrimitiveFFI.Stdio.print s
    ; PrimitiveFFI.Posix.Process.exit 1
    ; let exception DieFailed
      in raise DieFailed
      end)

val gcState = Primitive.MLton.GCState.gcState

structure AtomicState =
   struct
      datatype t = NonAtomic | Atomic of int
   end

local
   open Prim
in
   val atomicBegin = atomicBegin
   val atomicEnd = atomicEnd
   val atomicState = fn () =>
      case atomicState () of
         0wx0 => AtomicState.NonAtomic
       | w => AtomicState.Atomic (Word32.toInt w)
end

fun atomically f =
   (atomicBegin (); DynamicWind.wind (f, atomicEnd))

datatype 'a thread =
   Dead
 | Interrupted of Prim.thread
 | New of 'a -> unit
 (* In Paused (f, t), f is guaranteed to not raise an exception. *)
 | Paused of ((unit -> 'a) -> unit) * Prim.thread

datatype 'a t = T of 'a thread ref

structure Runnable =
   struct
      type t = unit t
   end

fun prepend (T r: 'a t, f: 'b -> 'a): 'b t =
   let
      val t =
         case !r of
            Dead => raise Fail "prepend to a Dead thread"
          | Interrupted _ => raise Fail "prepend to a Interrupted thread"
          | New g => New (g o f)
          | Paused (g, t) => Paused (fn h => g (f o h), t)
   in r := Dead
      ; T (ref t)
   end

fun prepare (t: 'a t, v: 'a): Runnable.t =
   prepend (t, fn () => v)

fun new f = T (ref (New f))

local
   local
      val func: (unit -> unit) option ref = ref NONE
      val base: Prim.preThread =
         let
            val () = Prim.copyCurrent ()
         in
            case !func of
               NONE => Prim.savedPre gcState
             | SOME x =>
                  (* This branch never returns. *)
                  let
                     (* Atomic 1 *)
                     val () = func := NONE
                     val () = atomicEnd ()
                     (* Atomic 0 *)
                  in
                     (x () handle e => MLtonExn.topLevelHandler e)
                     ; die "Thread didn't exit properly.\n"
                  end
         end
   in
      fun newThread (f: unit -> unit) : Prim.thread =
         let
            (* Atomic 2 *)
            val () = func := SOME f
         in
            Prim.copy base
         end
   end
   val switching = ref false
in
   fun 'a atomicSwitch (f: 'a t -> Runnable.t): 'a =
      (* Atomic 1 *)
      if !switching
         then let
                 val () = atomicEnd ()
                 (* Atomic 0 *)
              in
                 raise Fail "nested Thread.switch"
              end
      else
         let
            val _ = switching := true
            val r : (unit -> 'a) ref = 
               ref (fn () => die "Thread.atomicSwitch didn't set r.\n")
            val t: 'a thread ref =
               ref (Paused (fn x => r := x, Prim.current gcState))
            fun fail e = (t := Dead
                          ; switching := false
                          ; atomicEnd ()
                          ; raise e)    
            val (T t': Runnable.t) = f (T t) handle e => fail e
            val primThread =
               case !t' before t' := Dead of
                  Dead => fail (Fail "switch to a Dead thread")
                | Interrupted t => t
                | New g => (atomicBegin (); newThread g)
                | Paused (f, t) => (f (fn () => ()); t)
            val _ = switching := false
            (* Atomic 1 when Paused/Interrupted, Atomic 2 when New *)
            val _ = Prim.switchTo primThread (* implicit atomicEnd() *)
            (* Atomic 0 when resuming *)
         in
            !r ()
         end

   fun switch f =
      (atomicBegin ()
       ; atomicSwitch f)
end

fun fromPrimitive (t: Prim.thread): Runnable.t =
   T (ref (Interrupted t))

fun toPrimitive (t as T r : unit t): Prim.thread =
   case !r of
      Dead => die "Thread.toPrimitive saw Dead.\n"
    | Interrupted t => 
         (r := Dead
          ; t)
    | New _ =>
         switch
         (fn cur : Prim.thread t =>
          prepare
          (prepend (t, fn () =>
                    switch
                    (fn t' : unit t =>
                     prepare (cur, toPrimitive t'))),
           ()))
    | Paused (f, t) =>
         (r := Dead
          ; f (fn () => ()) 
          ; t)


local
   val signalHandler: Prim.thread option ref = ref NONE
   datatype state = Normal | InHandler
   val state: state ref = ref Normal
in
   fun amInSignalHandler () = InHandler = !state

   fun setSignalHandler (f: Runnable.t -> Runnable.t): unit =
      let
         val _ = Primitive.MLton.installSignalHandler ()
         fun loop (): unit =
            let
               (* Atomic 1 *)
               val _ = state := InHandler
               val t = f (fromPrimitive (Prim.saved gcState))
               val _ = state := Normal
               val _ = Prim.finishSignalHandler gcState
               val _ =
                  atomicSwitch
                  (fn (T r) =>
                   let
                      val _ =
                         case !r of
                            Paused (f, _) => f (fn () => ())
                          | _ => raise die "Thread.setSignalHandler saw strange thread"
                   in
                      t
                   end) (* implicit atomicEnd () *)
            in
               loop ()
            end
         val p =
            toPrimitive
            (new (fn () => loop () handle e => MLtonExn.topLevelHandler e))
         val _ = signalHandler := SOME p
      in
         Prim.setSignalHandler (gcState, p)
      end

   fun switchToSignalHandler () =
      let
         (* Atomic 0 *)
         val () = atomicBegin ()
         (* Atomic 1 *)
         val () = Prim.startSignalHandler gcState (* implicit atomicBegin () *)
         (* Atomic 2 *)
      in
         case !signalHandler of
            NONE => raise Fail "no signal handler installed"
          | SOME t => Prim.switchTo t (* implicit atomicEnd() *)
      end
end


local

in
   val register: int * (MLtonPointer.t -> unit) -> unit =
      let
         val exports =
            Array.array (Int32.toInt (Primitive.MLton.FFI.numExports),
                         fn _ => raise Fail "undefined export")
         val worker : (Prim.thread * Prim.thread option ref) option ref = ref NONE
         fun mkWorker (): Prim.thread * Prim.thread option ref =
            let
               val thisWorker : (Prim.thread * Prim.thread option ref) option ref = ref NONE
               val savedRef : Prim.thread option ref = ref NONE
               fun workerLoop () =
                  let
                     (* Atomic 1 *)
                     val p = Primitive.MLton.FFI.getOpArgsResPtr ()
                     val _ = atomicEnd ()
                     (* Atomic 0 *)
                     val i = MLtonPointer.getInt32 (MLtonPointer.getPointer (p, 0), 0)
                     val _ =
                        (Array.sub (exports, Int32.toInt i) p)
                        handle e =>
                           (TextIO.output
                            (TextIO.stdErr, "Call from C to SML raised exception.\n")
                            ; MLtonExn.topLevelHandler e)
                     (* Atomic 0 *)
                     val _ = atomicBegin ()
                     (* Atomic 1 *)
                     val _ = worker := !thisWorker
                     val _ = Prim.setSaved (gcState, valOf (!savedRef))
                     val _ = savedRef := NONE
                     val _ = Prim.returnToC () (* implicit atomicEnd() *)
                  in
                     workerLoop ()
                  end
               val workerThread = toPrimitive (new workerLoop)
               val _ = thisWorker := SOME (workerThread, savedRef)
            in
               (workerThread, savedRef)
            end
         fun handlerLoop (): unit =
            let
               (* Atomic 2 *)
               val saved = Prim.saved gcState
               val (workerThread, savedRef) =
                  case !worker of
                     NONE => mkWorker ()
                   | SOME (workerThread, savedRef) =>
                        (worker := NONE
                         ; (workerThread, savedRef))
               val _ = savedRef := SOME saved
               val _ = Prim.switchTo (workerThread) (* implicit atomicEnd() *)
            in
               handlerLoop ()
            end
         val handlerThread = toPrimitive (new handlerLoop)
         val _ = Prim.setCallFromCHandler (gcState, handlerThread)
      in
         fn (i, f) => Array.update (exports, i, f)
      end
end

end
