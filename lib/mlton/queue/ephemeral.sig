signature EPHEMERAL_QUEUE =
   sig
      type 'a t
      val isEmpty: 'a t -> bool
      val enque: 'a t * 'a -> unit
      val deque: 'a t -> 'a
   end
