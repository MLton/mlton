signature T =
   sig
      type t
	 
      val equals: t * t -> bool
      val layout: t -> Layout.t
   end

