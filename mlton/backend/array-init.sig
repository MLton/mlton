signature ARRAY_INIT_STRUCTS = 
   sig
      structure Rssa: RSSA
   end

signature ARRAY_INIT = 
   sig
      include ARRAY_INIT_STRUCTS
      
      val insert: Rssa.Program.t -> Rssa.Program.t
   end
