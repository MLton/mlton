signature TREE =
   sig
      datatype 'a t = T of 'a * 'a t list
   end
