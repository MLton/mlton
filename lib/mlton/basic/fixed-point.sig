signature FIXED_POINT = 
   sig
      val fix: {start: 'a,
		step: 'a -> 'a,
		equals: 'a * 'a -> bool} -> 'a

      val fix': ((unit -> unit) -> unit) -> unit
   end
