type int = Int.t

signature TAB =
   sig
      val make: Out.t * string -> {reset: unit -> unit,
				    right: unit -> unit,
				    left: unit -> unit,
				    indent: unit -> unit}
   end
