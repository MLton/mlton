signature MLTON_PROCESS =
   sig
      type pid = Posix.Process.pid

      val exit: int -> 'a
      val spawn: {path: string, args: string list} -> pid
      val spawne: {path: string, args: string list, env: string list} -> pid
      val spawnp: {file: string, args: string list} -> pid
   end
