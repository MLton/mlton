signature BASIC_PERSISTENT_QUEUE =
   sig
      type 'a t

      val empty: unit -> 'a t
      val isEmpty: 'a t -> bool
      val destruct: 'a t -> ('a * 'a t) option
      val enque: 'a t * 'a -> 'a t
   end
