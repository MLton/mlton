signature CONST_TYPE =
   sig
      datatype t = Bool | Int | Real | String | Word

      val toString: t -> string
   end
