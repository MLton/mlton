(* fun-prio-queue.sig
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

signature FUN_PRIORITY_QUEUE_ARG =
   sig
      structure Key :
         sig
            type t
            val compare : t * t -> order
         end
   end

signature FUN_PRIORITY_QUEUE =
   sig
      include FUN_PRIORITY_QUEUE_ARG

      structure Elt:
         sig
            type 'a t
            val key: 'a t -> Key.t
            val value: 'a t -> 'a
         end

      type 'a t

      val clean: 'a t * ('a Elt.t -> bool) -> 'a t
      val cleanAndDeque: 'a t * ('a Elt.t -> bool) -> 'a Elt.t option * 'a t
      val cleanPrefix: 'a t * ('a Elt.t -> bool) -> 'a t
      val deque: 'a t -> ('a Elt.t * 'a t) option
      val empty: 'a t -> bool
      val enque: 'a t * Key.t * 'a -> 'a t
      val enqueAndClean: 'a t * Key.t * 'a * ('a Elt.t -> bool) -> 'a t
      val new: unit -> 'a t
      val peek: 'a t -> 'a Elt.t option
   end
