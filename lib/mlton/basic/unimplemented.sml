structure Unimplemented =
   struct
      val op equals = fn _ => Error.unimplemented "equals"
      fun layout _ = Error.unimplemented "layout"
      fun output _ = Error.unimplemented "output"
   end
