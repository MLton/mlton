signature PROFILE_DATA =
   sig
      type t

      val equals: t * t -> bool
      val free: t -> unit
      val malloc: unit -> t
      val reset: t -> unit
      val write: t * string -> unit
   end
