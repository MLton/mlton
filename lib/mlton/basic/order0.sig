signature ORDER0 =
   sig
      type t

      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val < : t * t -> bool
      val <= : t * t -> bool
      val > : t * t -> bool
      val >= : t * t -> bool
      val max: t * t -> t
      val min: t * t -> t
   end
