(* fun-queue.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

signature FUN_QUEUE =
   sig
      type 'a t

      val clean: 'a t * ('a -> bool) -> 'a t
      val cleanAndDeque: 'a t * ('a -> bool) -> 'a option * 'a t
      val cleanPrefix: 'a t * ('a -> bool) -> 'a t
      val deque: 'a t -> ('a * 'a t) option
      val empty: 'a t -> bool
      val enque: 'a t * 'a -> 'a t
      val enqueAndClean: 'a t * 'a * ('a -> bool) -> 'a t
      val new: unit -> 'a t
      val peek: 'a t -> 'a option
   end
