(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature MLTON_THREAD =
   sig
      structure AtomicState :
	 sig
	    datatype t = NonAtomic | Atomic of int
	 end
      val atomicBegin: unit -> unit
      val atomicEnd: unit -> unit
      val atomically: (unit -> 'a) -> 'a
      val atomicState: unit -> AtomicState.t

      type 'a t
      type ready_t

      (* new f creates a new thread that will apply f to whatever is thrown
       * to the thread.  f must terminate by throwing to another thread or
       * exiting the process.
       *)
      val new: ('a -> unit) -> 'a t
      (* prepend(t, f)
       * Create a new thread (destroying t in the process) that evaluates
       * f and passes the result to t.
       *)
      val prepend: 'a t * ('b -> 'a) -> 'b t
      (* ready(t)
       * Create a new ready thread (destroying t in the process)
       * that will evaluate t.
       *)
      val prep: unit t -> ready_t
      (* readyFn(t, f)
       * Create a new ready thread (destroying t in the process)
       * that will evaluate t on f ().
       *)
      val prepFn: 'a t * (unit -> 'a) -> ready_t
      (* readyVal(t, v)
       * Create a new ready thread (destroying t in the process)
       * that will evaluate t on v.
       *)
      val prepVal: 'a t * 'a -> ready_t
      (* Equivalences
       *
       * prepFn(t, f) ==
       * prep(prepend(t, f))
       *
       * prepVal(t, v) ==
       * prepFn(t, fn () => v)
       *
       *)
      (* switch f = rt
       * Applies f to the current thread, and then switches to rt.  
       * f runs in a critical section.
       * It is an error for f to call switch.
       *)
      val switch: ('a t -> ready_t) -> 'a
      (* atomicSwitch is as above,
       * but assumes an atomic calling context.
       *)
      val atomicSwitch: ('a t -> ready_t) -> 'a
   end

signature MLTON_THREAD_EXTRA =
   sig
      include MLTON_THREAD

      val amInSignalHandler: unit -> bool
      val register: int * (unit -> unit) -> unit
      val setHandler: (ready_t -> ready_t) -> unit
      val switchToHandler: unit -> unit
   end
