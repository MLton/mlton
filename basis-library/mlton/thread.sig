(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.int

signature MLTON_THREAD =
   sig
      structure AtomicState :
         sig
            datatype t = NonAtomic | Atomic of int
         end
      val atomically: (unit -> 'a) -> 'a
      val atomicBegin: unit -> unit
      val atomicEnd: unit -> unit
      val atomicState: unit -> AtomicState.t

      structure Runnable :
         sig
            type t
         end

      type 'a t

      (* atomicSwitch f
       * as switch, but assumes an atomic calling context.  Upon
       * switch-ing back to the current thread, an implicit atomicEnd is
       * performed.
       *)
      val atomicSwitch: ('a t -> Runnable.t) -> 'a
      (* new f
       * create a new thread that, when run, applies f to
       * the value given to the thread.  f must terminate by
       * switch-ing to another thread or exiting the process.
       *)
      val new: ('a -> unit) -> 'a t
      (* prepend(t, f)
       * create a new thread (destroying t in the process) that first
       * applies f to the value given to the thread and then continues
       * with t.  This is a constant time operation.
       *)
      val prepend: 'a t * ('b -> 'a) -> 'b t
      (* prepare(t, v)
       * create a new runnable thread (destroying t in the process)
       * that will evaluate t on v.
       *)
      val prepare: 'a t * 'a -> Runnable.t
      (* switch f 
       * apply f to the current thread to get rt, and then start
       * running thread rt.  It is an error for f to
       * perform another switch.  f is guaranteed to run
       * atomically.
       *)
      val switch: ('a t -> Runnable.t) -> 'a
   end

signature MLTON_THREAD_EXTRA =
   sig
      include MLTON_THREAD

      val amInSignalHandler: unit -> bool
      val register: int * (unit -> unit) -> unit
      val setSignalHandler: (Runnable.t -> Runnable.t) -> unit
      val switchToSignalHandler: unit -> unit
   end
