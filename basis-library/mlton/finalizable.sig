signature MLTON_FINALIZABLE =
   sig
      type 'a t

      val addFinalizer: 'a t * ('a -> unit) -> unit
      val finalizeBefore: 'a t * 'b t -> unit
      val new: 'a -> 'a t
      val withValue: 'a t * ('a -> 'b) -> 'b
   end
