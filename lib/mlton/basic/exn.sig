signature EXN =
   sig
      type t = exn

      exception Bind
      exception Match
      exception Overflow
      exception Subscript
      
      val history: t -> string list
      val name: t -> string
      val layout: t -> Layout.t
   end
