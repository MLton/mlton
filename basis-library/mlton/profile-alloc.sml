structure MLtonProfileAlloc: MLTON_PROFILE =
struct

structure Prim = Primitive.MLton.ProfileAlloc
structure P = MLtonProfile (open Prim)
open P

val _ =
   if not isOn
      then ()
   else
      (Cleaner.addNew (Cleaner.atExit, fn () =>
		       (Prim.done ()
			; P.cleanAtExit ()))
       ; Cleaner.addNew (Cleaner.atLoadWorld, fn () =>
			 (P.cleanAtLoadWorld ()
			  ; init ()))
       ; init ())

end
