signature ESCAPE =
   sig
      type 'a t

      val escape: 'a t * 'a -> 'b
      val new: ('a t -> 'a) -> 'a
   end
