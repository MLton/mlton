signature FLAT_LATTICE_STRUCTS =
   sig
      structure Point:
	 sig
	    type t

	    val equals: t * t -> bool
	    val layout: t -> Layout.t
	 end
   end

signature FLAT_LATTICE =
   sig
      include FLAT_LATTICE_STRUCTS
	 
      type t

      val <= : t * t -> bool
      val forcePoint: t * Point.t -> bool
      val layout: t -> Layout.t
      val lowerBound: t * Point.t -> bool
      val new: unit -> t
      val point: Point.t -> t
      val upperBound: t * Point.t -> bool
   end
