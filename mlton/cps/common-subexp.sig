signature COMMON_SUBEXP_STRUCTS = 
   sig
      include SHRINK
   end

signature COMMON_SUBEXP = 
   sig
      include COMMON_SUBEXP_STRUCTS
      
      val eliminate: Program.t -> Program.t
   end
