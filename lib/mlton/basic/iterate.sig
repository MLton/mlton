signature ITERATE =
   sig
      val iterate: 'a * ('a -> bool) * ('a -> 'a) -> 'a
      (* iterate(s, p, f) = f(...f(f(s))) until satisfies p *)

      val whileDo: (unit -> bool) * (unit -> unit) -> unit

      val repeatUntil: (unit -> unit) * (unit -> bool) -> unit
   end
