type word = Word.word
   
signature MLTON_RLIMIT =
   sig
      type rlim = word
	       
      val infinity: rlim

      type t
	       
      val cpuTime: t             (* CPU     CPU time in seconds *)
      val coreFileSize: t        (* CORE    max core file size *)
      val dataSize: t            (* DATA    max data size *)
      val fileSize: t            (* FSIZE   Maximum filesize *)
      val lockedInMemorySize: t  (* MEMLOCK max locked address space *)
      val numFiles: t            (* NOFILE  max number of open files *)  
      val numProcesses: t        (* NPROC   max number of processes *)
      val residentSetSize: t     (* RSS     max resident set size *)
      val stackSize: t           (* STACK   max stack size *)
      val virtualMemorySize: t   (* AS      virtual memory limit *)
      
      val get: t -> {hard: rlim, soft: rlim}
      val set: t * {hard: rlim, soft: rlim} -> unit
   end
