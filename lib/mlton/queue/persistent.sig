signature PERSISTENT_QUEUE =
   sig
      include BASIC_PERSISTENT_QUEUE
  
      val deque: 'a t -> 'a * 'a t
   end
