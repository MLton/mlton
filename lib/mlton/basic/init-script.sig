signature INIT_SCRIPT =
   sig
      val startStop: {name: string,
		      action: string,
		      log: File.t,
		      thunk: unit -> unit,
		      usage: string -> unit} -> unit
   end
