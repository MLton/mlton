signature COMMON_BLOCK_STRUCTS = 
   sig
      include SHRINK
   end

signature COMMON_BLOCK = 
   sig
      include COMMON_BLOCK_STRUCTS
      
      val eliminate: Program.t -> Program.t
   end
