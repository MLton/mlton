signature EXN =
   sig
      type t = exn

      val history: t -> string list
      val name: t -> string
      val layout: t -> Layout.t
   end
