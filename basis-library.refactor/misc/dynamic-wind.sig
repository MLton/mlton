signature DYNAMIC_WIND =
   sig
      val wind: (unit -> 'a) * (unit -> unit) -> 'a
   end
