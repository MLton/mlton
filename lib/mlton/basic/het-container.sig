signature HET_CONTAINER =
   sig
      type t

      val new: unit -> {make: 'a -> t,
			pred: t -> bool,
			peek: t -> 'a option}
   end
