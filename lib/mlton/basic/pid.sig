signature PID =
   sig
      type t
	 
      val current: unit -> t
      val equals: t * t -> bool
      val fromString: string -> t option
      val layout: t -> Layout.t
      val parent: unit -> t
      val toString: t -> string
   end
