functor IOConvert
        (structure IO: IO) :
        IO_1997 =
  struct
     open IO
     exception TerminatedStream
  end