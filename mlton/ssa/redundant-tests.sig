signature REDUNDANT_TESTS_STRUCTS = 
   sig
      include SHRINK
   end

signature REDUNDANT_TESTS = 
   sig
      include REDUNDANT_TESTS_STRUCTS
      
      val simplify: Program.t -> Program.t
   end
