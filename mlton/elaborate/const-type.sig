signature CONST_TYPE =
   sig
      datatype t = Bool | Real | String | Word

      val toString: t -> string
   end
