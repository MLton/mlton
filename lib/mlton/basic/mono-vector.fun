functor MonoVector (Elt: T) =
   struct
      open Vector
      type t = Elt.t t
      val equals = fn (a, a') => equals (a, a', Elt.equals)
      val layout = layout Elt.layout
   end
