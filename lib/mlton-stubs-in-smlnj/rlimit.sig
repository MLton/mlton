signature MLTON_RLIMIT =
   sig
      type rlim = word
	       
      val infinity: rlim

      type resource
	       
      val cpuTime: resource             (* CPU     CPU time in seconds *)
      val coreFileSize: resource        (* CORE    max core file size *)
      val dataSize: resource            (* DATA    max data size *)
      val fileSize: resource            (* FSIZE   Maximum filesize *)
      val lockedInMemorySize: resource  (* MEMLOCK max locked-in-memory address space *)
      val numFiles: resource            (* NOFILE  max number of open files *)  
      val numProcesses: resource        (* NPROC   max number of processes *)
      val residentSetSize: resource     (* RSS     max resident set size *)
      val stackSize: resource           (* STACK   max stack size *)
      val virtualMemorySize: resource   (* AS      address space (virtual memory) limit *)
      
      val get: resource -> {hard: rlim, soft: rlim}
      val set: resource * {hard: rlim, soft: rlim} -> unit
   end
