structure S:
   sig
      datatype t = A | B of unit
   end =
   struct
      datatype t = A | B of int

      val rec B = fn () => A
   end
