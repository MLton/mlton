signature SUM_STRUCTS =
   sig
      structure X: T
      structure Y: T
   end

signature SUM =
   sig
      include SUM_STRUCTS
	 
      datatype t =
	 X of X.t
       | Y of Y.t

      val equals: t * t -> bool
      val layout: t -> Layout.t
      val map: t * (X.t -> 'a) * (Y.t -> 'a) -> 'a
      val outX: t -> X.t
      val outY: t -> Y.t
   end
