structure ProfileAlloc: MLTON_PROFILE =
struct
   
structure P = Profile (open Primitive.MLton.ProfileAlloc)
open P

val _ = Cleaner.addNew (Cleaner.atExit, P.cleanAtExit)
val _ = Cleaner.addNew (Cleaner.atLoadWorld, P.cleanAtLoadWorld)

end
