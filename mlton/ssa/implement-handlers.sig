signature IMPLEMENT_HANDLERS_STRUCTS = 
   sig
      include SHRINK
   end

signature IMPLEMENT_HANDLERS = 
   sig
      include IMPLEMENT_HANDLERS_STRUCTS
      
      val doit: Program.t -> Program.t
   end
