signature QUEUE =
   sig
      type 'a t

      val deque: 'a t -> ('a * 'a t) option
      val empty: unit -> 'a t
      val enque: 'a t * 'a -> 'a t
      val foldAnyOrder: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val foldr: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val isEmpty: 'a t -> bool
      val toList: 'a t -> 'a list
   end
