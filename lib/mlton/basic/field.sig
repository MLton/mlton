signature FIELD_STRUCTS =
   sig
      include RING

      val inverse: t -> t
   end

signature FIELD =
   sig
      include FIELD_STRUCTS

      val / : t * t -> t
   end

