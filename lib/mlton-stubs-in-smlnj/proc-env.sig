signature MLTON_PROC_ENV =
   sig
      val setenv: {name: string, value: string} -> unit
   end
