signature MLTON_CONT =
   sig
      type 'a t

      val callcc: ('a t -> 'a) -> 'a
      val prepend: 'a t * ('b -> 'a) -> 'b t
      val throw: 'a t * 'a -> 'b
      val throw': 'a t * (unit -> 'a) -> 'b
   end
