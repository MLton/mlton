signature INIT_SCRIPT =
   sig
      val startStop: {name: string,
		      action: string,
		      thunk: unit -> unit,
		      usage: string -> unit} -> unit
   end
