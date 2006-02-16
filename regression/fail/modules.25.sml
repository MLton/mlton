structure S:
   sig
      structure T:
         sig
            datatype t = A | B
         end
   end =
   struct
      structure T =
         struct
            datatype t = B | C
         end
   end
