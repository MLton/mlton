signature IMPLEMENT_HANDLERS_STRUCTS = 
   sig
      structure Ssa: SSA
   end

signature IMPLEMENT_HANDLERS = 
   sig
      include IMPLEMENT_HANDLERS_STRUCTS
      
      val doit: Ssa.Program.t -> Ssa.Program.t
   end
