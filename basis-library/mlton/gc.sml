structure GC =
   struct
      open Primitive.GC

      val collect = fn () => collect (0w0, true)
   end
