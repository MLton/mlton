structure MLtonProfileAlloc: MLTON_PROFILE =
struct
   
structure P = MLtonProfile (open Primitive.MLton.ProfileAlloc)
open P

val _ =
   if not isOn
      then ()
   else
      (Cleaner.addNew (Cleaner.atExit, P.cleanAtExit)
       ; Cleaner.addNew (Cleaner.atLoadWorld, fn () =>
			 (P.cleanAtLoadWorld ()
			  ; init ()))
       ; init ())

end
