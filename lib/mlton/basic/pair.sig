signature PAIR =
   sig
      structure X: T
      structure Y: T

      type t = X.t * Y.t
      val equals: t * t -> bool
      val layout: t -> Layout.t
   end
