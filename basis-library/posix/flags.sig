signature BIT_FLAGS =
   sig
      eqtype flags

      val all: flags
      val allSet: flags * flags -> bool 
      val anySet: flags * flags -> bool
      val clear: flags * flags -> flags
      val flags: flags list -> flags 
      val fromWord: SysWord.word -> flags 
      val intersect: flags list -> flags
      val toWord: flags -> SysWord.word 
   end

signature BIT_FLAGS_EXTRA =
   sig
      include BIT_FLAGS

      val empty: flags
   end
