signature BIT_FLAGS =
   sig
      eqtype flags

      val toWord: flags -> SysWord.word 
      val fromWord: SysWord.word -> flags 
      val all: flags
      val flags: flags list -> flags 
      val intersect: flags list -> flags
      val clear: flags * flags -> flags
      val allSet: flags * flags -> bool 
      val anySet: flags * flags -> bool
   end

signature BIT_FLAGS_EXTRA =
   sig
      include BIT_FLAGS
      val empty: flags
   end
