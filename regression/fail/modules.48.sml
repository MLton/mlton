signature S =
   sig
      datatype t = T
   end

structure S1: S =
   struct
      datatype t = T
    end

structure S2: S where type t = int = S1
