functor F () =
   struct
      datatype t = T of int -> int
   end

functor G () =
   struct
      structure S = F ()

      fun f (x: S.t) = x = x
   end
