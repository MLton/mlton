type int = Int.t
   
signature LIMIT_CHECK_STRUCTS = 
   sig
      structure Rssa: RSSA
   end

signature LIMIT_CHECK = 
   sig
      include LIMIT_CHECK_STRUCTS

      val insert: Rssa.Program.t -> Rssa.Program.t
   end
