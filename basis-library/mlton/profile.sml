structure Profile: MLTON_PROFILE =
   struct
      open Primitive.MLton.Profile

      val reset = if profile
		    then reset
		    else fn () => ()
      val write = if profile
		    then fn f => write (String.nullTerm f)
		    else fn f => ()
   end