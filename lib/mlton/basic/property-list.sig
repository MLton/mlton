type int = Int.t

signature PROPERTY_LIST = 
   sig
      type t

      (* remove all properties from the list *)
      val clear: t -> unit
      (* pointer equality of property lists *)
      val equals: t * t -> bool
      val length: t -> int
      (* create an empty property list *)
      val new: unit -> t
      (* create a new property *)
      val newProperty:
	 unit -> {
                  (* See if a property is in a property list.
		   * NONE if it isn't.
		   *)
		  peek: t -> 'a option,
		  (* get a property from a property list.
		   * Raises error if the property isn't there.
		   *)
		  get: t -> 'a,
		  (* set(p, v, f)
		   * Change the value of property.
		   * Create a new property and call f if it isn't already there.
		   *)
		  set: t * 'a * (unit -> unit) -> unit,
		  (* Add the value of the property -- must not already exist. *)
		  add: t * 'a -> unit,
		  (* Remove a property from a property list.
		   * Noop if the property isn't there.
		   *)
		  remove: t -> unit
		  }
      val stats: unit -> Layout.t
   end
