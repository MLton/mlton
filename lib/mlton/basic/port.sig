type int = Int.t
   
signature PORT =
   sig
      type t = int

      val equals: t * t -> bool
      val http: t
   end
   
