(* imp-queue.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

signature IMP_QUEUE =
   sig
      type 'a t

      val clean: 'a t * ('a -> bool) -> unit
      val cleanAndDeque: 'a t * ('a -> bool) -> 'a option
      val cleanPrefix: 'a t * ('a -> bool) -> unit
      val deque: 'a t -> 'a option
      val empty: 'a t -> bool
      val enque: 'a t * 'a -> unit
      val enqueAndClean: 'a t * 'a * ('a -> bool) -> unit
      val new: unit -> 'a t
      val peek: 'a t -> 'a option
      val reset: 'a t -> unit
   end
