structure S:
   sig
      datatype t = A | B
   end =
   struct
      datatype t = A | B
      datatype u = B
   end
