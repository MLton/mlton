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
      (* switch f = (t, x)
       * Applies f to the current thread, and then switches to t with
       * argument x.  f runs in a critical section.
       * It is an error for f to call switch.
       *)
      val switch: ('a t -> 'b t * 'b) -> 'a
      (* switch' is a generalization of switch that evaluates the thunk
       * x in the context of t (i.e. t's stack and exception handlers are in
       * place).
       *)
      val switch': ('a t -> 'b t * (unit -> 'b)) -> 'a
      (* atomicSwitch and atomicSwitch' are as above,
       * but assume an atomic calling context.
       *)
      val atomicSwitch': ('a t -> 'b t * (unit -> 'b)) -> 'a
      val atomicSwitch: ('a t -> 'b t * 'b) -> 'a

      (*
      (* One-shot continuations. *)
      (* capture f
       * Applies f to the current thread.
       * If f returns or raises, then it implicitly escapes to the
       * current thread.
       *)
      val capture: ('a t -> 'a) -> 'a
      (* escape (t, x)
       * Switch to t with argument x.
       * It is illegal for another thread to later escape to t.
       *)
      val escape: 'a t * 'a -> 'b
      (* escape' (t, th)
       * Generalization of escape that evaluates the thunk th in the
       * context of t (i.e., t's stack and exception handlers are in
       * place).
       *)
      val escape': 'a t * (unit -> 'a) -> 'b

      val atomicCapture: ('a t -> 'a) -> 'a
      val atomicEscape: 'a t * 'a -> 'b
      val atomicEscape': 'a t * (unit -> 'a) -> 'b
      *)
   end

signature MLTON_THREAD_EXTRA =
   sig
      include MLTON_THREAD

      val amInSignalHandler: unit -> bool
      val register: int * (unit -> unit) -> unit
      val setHandler: (unit t -> unit t) -> unit
      val switchToHandler: unit -> unit
   end
