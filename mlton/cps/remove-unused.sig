type int = Int.t
   
signature REMOVE_UNUSED_STRUCTS = 
   sig
      include SHRINK
   end

signature REMOVE_UNUSED = 
   sig
      include REMOVE_UNUSED_STRUCTS
      
      val remove: Program.t -> Program.t
   end


functor TestRemoveUnused(S: REMOVE_UNUSED) = 
struct

open S

val _ = Assert.assert("RemoveUnused", fn () => true)

end
