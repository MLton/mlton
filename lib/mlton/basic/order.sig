signature ORDER =
   sig
      include ORDER0
      val layout: t -> Layout.t
   end
