signature MLTON_PROCESS =
   sig
      val spawn: {path: string, args: string list} -> unit
      val spawne: {path: string, args: string list, env: string list} -> unit
      val spawnp: {file: string, args: string list} -> unit
   end
