type int = Int.t

signature TYCON_KIND_STRUCTS = 
   sig
   end

signature TYCON_KIND = 
   sig
      include TYCON_KIND_STRUCTS
      
      datatype t =
	 Arity of int
       | Nary

      val equals: t * t -> bool
      val layout: t -> Layout.t
   end
