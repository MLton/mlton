type int = Int.t
   
signature UNIQUE_ID =
   sig
      type t

      val equals: t * t -> bool
      val layout: t -> Layout.t
      val new: unit -> t
      val toString: t -> string
   end
