signature RELATION =
   sig
      datatype t = datatype order (* from the pervasive environment *)

      val compare: ('a * 'a -> t)
	 -> {equals: 'a * 'a -> bool,
	     < : 'a * 'a -> bool,
	     > : 'a * 'a -> bool,
	     >= : 'a * 'a -> bool,
	     <= : 'a * 'a -> bool,
	     min: 'a * 'a -> 'a,
	     max: 'a * 'a -> 'a}
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val lessEqual: {< : 'a * 'a -> bool,
		       equals: 'a * 'a -> bool}
	 -> {> : 'a * 'a -> bool,
	     >= : 'a * 'a -> bool,
	     <= : 'a * 'a -> bool,
	     min: 'a * 'a -> 'a,
	     max: 'a * 'a -> 'a,
	     compare: 'a * 'a -> t}
   end
