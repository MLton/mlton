signature QUEUE =
   sig
      type 'a t

      val deque: 'a t -> ('a * 'a t) option
      val empty: unit -> 'a t
      val enque: 'a t * 'a -> 'a t
      val isEmpty: 'a t -> bool
   end
