structure S:
   sig
      eqtype t
      structure Z:
         sig
            datatype u = U
         end
   end =
   struct
      structure Z =
         struct
            datatype u = U
         end
      datatype t = datatype Z.u
      structure Z =
         struct
            type u = Z.u
            datatype z = datatype Z.u
         end
   end
