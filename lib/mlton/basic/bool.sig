signature BOOL =
   sig
      type t

      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val fromString: string -> t option
      val layout: t -> Layout.t
      val toString: t -> string
   end where type t = bool
