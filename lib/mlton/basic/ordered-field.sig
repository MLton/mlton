signature ORDERED_FIELD_STRUCTS =
   sig
      include ORDERED_RING

      val inverse: t -> t
   end

signature ORDERED_FIELD =
   sig
      include ORDERED_FIELD_STRUCTS

      val / : t * t -> t
   end

