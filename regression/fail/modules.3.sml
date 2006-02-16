functor F (type t
           datatype u = U of t
           eqtype v
           sharing type t = v) =
   struct
      fun f (u: u) = u = u
   end
