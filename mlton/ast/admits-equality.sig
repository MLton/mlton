signature ADMITS_EQUALITY_STRUCTS = 
   sig
   end

signature ADMITS_EQUALITY = 
   sig
      include ADMITS_EQUALITY_STRUCTS
      
      datatype t = Always | Never | Sometimes

      val layout: t -> Layout.t
      val or: t * t -> t
      val toString: t -> string
   end
