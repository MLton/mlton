signature MLTON_PROC_ENV =
   sig
      val setenv: {name: string, value: string} -> unit
      val setgroups: Posix.ProcEnv.gid list -> unit
   end
