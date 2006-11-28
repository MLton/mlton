signature UNIX_1997 =
   sig
      type proc
      type signal

      val executeInEnv: string * string list * string list -> proc 
      val execute: string * string list -> proc 
      val streamsOf: proc -> TextIO.instream * TextIO.outstream 
      val reap: proc -> OS.Process.status 
      val kill: proc * signal -> unit
   end
