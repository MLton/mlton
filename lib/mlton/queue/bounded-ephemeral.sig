signature BOUNDED_EPHEMERAL_QUEUE =
   sig
      include EPHEMERAL_QUEUE

      structure I: INTEGER
      val empty: I.t -> '1a t
      val size: 'a t -> I.t
      val maxSize: '1a t -> I.t
      val isFull: '1a t -> bool
   end
