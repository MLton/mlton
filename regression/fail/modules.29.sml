(* Generativity of functors. *)
functor F () =
   struct
      datatype t = T
   end
structure S1 = F ()
structure S2 = F ()
val _ = S1.T = S2.T
