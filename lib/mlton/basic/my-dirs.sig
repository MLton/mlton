signature MY_DIRS =
   sig
      val dirs: unit -> {home: Dir.t,
			  sml: Dir.t,
			  smlnj: Dir.t,
			  bin: Dir.t,
			  binFiles: Dir.t,
			  heap: Dir.t,
			  src: Dir.t,
			  compiler: Dir.t}
      val exportFn: string * (string * string list -> int) -> unit
      val exportML: string -> bool
   end
