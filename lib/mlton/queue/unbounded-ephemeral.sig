signature UNBOUNDED_EPHEMERAL_QUEUE =
   sig
      include EPHEMERAL_QUEUE
      
      val empty: unit -> 'a t
   end
