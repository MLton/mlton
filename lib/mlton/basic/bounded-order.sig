signature BOUNDED_ORDER =
   sig
      structure O: ORDER
      include ORDER
      val inject: O.t -> t
      val project: t -> O.t
      val largest: t
      val smallest: t
   end
