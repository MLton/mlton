signature OUTSTREAM =
   sig
      type t
	 
      val close: t -> unit
      val error: t
      val fluidLet: t * t * (unit -> 'a) -> 'a
      val flush: t -> unit
      val ignore: t * (unit -> 'a) -> 'a
      val layout: t -> Layout.t
      val newline: t -> unit
      val openAppend: string -> t
      val openOut: string -> t
      val output: t * string -> unit
      val output1: t * char -> unit
      val outputc: t -> string -> unit
      val outputSubstr: t * Substring.t -> unit
      val print: string -> unit
      val set: t * t -> unit
      val standard: t
      val withClose: t * (t -> 'a) -> 'a
      val withNull: (t -> 'a) -> 'a
   end
