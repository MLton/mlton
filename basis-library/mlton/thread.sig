(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature MLTON_THREAD =
   sig
      type 'a t

      val atomicBegin: unit -> unit
      val atomicEnd: unit -> unit
      val atomically: (unit -> 'a) -> 'a
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
   end

signature MLTON_THREAD_EXTRA =
   sig
      include MLTON_THREAD

      val amInSignalHandler: unit -> bool
      val register: int * (unit -> unit) -> unit
      val setHandler: (unit t -> unit t) -> unit
      val switchToHandler: unit -> unit
   end
