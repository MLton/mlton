signature PS =
   sig
      structure State =
	 struct
	    datatype t =
	       Running | Sleeping
	 end

      val ps: unit -> {pid: Pid.t,
		       commandName: string,
		       args: string list,
		       state: State.t} list
   end
