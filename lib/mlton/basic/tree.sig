signature TREE =
   sig
      datatype 'a t = T of 'a * 'a t list
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
   end
