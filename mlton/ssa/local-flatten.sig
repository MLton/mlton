signature LOCAL_FLATTEN_STRUCTS = 
   sig
      include SHRINK
   end

signature LOCAL_FLATTEN = 
   sig
      include LOCAL_FLATTEN_STRUCTS

      (* Intraprocedural flattening. *)
      val flatten: Program.t -> Program.t
   end


functor TestLocalFlatten (S: LOCAL_FLATTEN): sig end = 
struct

val _ = print "TestLocalFlatten\n"

open S

val _ = Assert.assert ("LocalFlatten", fn () => true)

end
