structure S:
   sig
      eqtype t
      structure Z:
         sig
            datatype u = U
         end where type u = t
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
