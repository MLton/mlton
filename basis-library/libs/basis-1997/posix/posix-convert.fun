functor PosixConvert
        (structure Posix : POSIX) :
        POSIX_1997 = 
  struct
     open Posix
     structure Process = PosixProcessConvert(structure Process = Process)
     structure FileSys = PosixFileSysConvert(structure FileSys = FileSys)
     structure IO = PosixIOConvert(structure IO = IO)
     structure TTY = PosixTTYConvert(structure TTY = TTY)
  end
