signature CONTROL =
   sig
      val all: unit -> {name: string,
			value: string} list
      val control: {name: string,
		    default: 'a,
		    toString: 'a -> string} -> 'a ref
      val setDefaults: unit -> unit
   end
