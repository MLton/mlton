signature MLTON_WEAK =
   sig
      type 'a t

      val get: 'a t -> 'a option
      val new: 'a -> 'a t
   end
