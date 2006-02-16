structure T =
   struct
      structure X =
         struct
            type t = int
         end
   end
signature S =
   sig
      structure T: sig end
      val x: T.X.t
   end
