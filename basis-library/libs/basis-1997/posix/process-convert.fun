functor PosixProcessConvert
        (structure Process: POSIX_PROCESS) :
        POSIX_PROCESS_1997 =
  struct
     open Process
     structure W =
        struct
	   open W
	   structure Flags = FlagsConvert(structure Flags = W)
	   open Flags
	end
  end