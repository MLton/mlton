functor PosixIOConvert
        (structure IO: POSIX_IO) :
        POSIX_IO_1997 =
  struct
     open IO
     structure FD =
        struct
	   open FD
	   structure Flags = FlagsConvert(structure Flags = FD)
	   open Flags
	end
     structure O =
        struct
	   open O
	   structure Flags = FlagsConvert(structure Flags = O)
	   open Flags
	end

  end