signature EXPORT =
   sig
      val exportFn: File.t * (string * string list -> OS.Process.status) -> unit
   end
