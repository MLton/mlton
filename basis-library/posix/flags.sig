signature POSIX_FLAGS =
   sig
      eqtype flags

      val toWord: flags -> SysWord.word 
      val wordTo: SysWord.word -> flags 
      val flags: flags list -> flags 
      val allSet: flags * flags -> bool 
      val anySet: flags * flags -> bool
   end

signature POSIX_FLAGS_EXTRA =
   sig
      include POSIX_FLAGS
      val empty: flags
   end
