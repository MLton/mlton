signature EXN =
   sig
      type t = exn

      val layout: t -> Layout.t
   end
