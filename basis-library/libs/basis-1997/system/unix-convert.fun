functor UnixConvert
        (structure Unix: UNIX) :
        UNIX_1997 =
  struct
     open Unix
     type proc = (TextIO.instream, TextIO.outstream) proc
  end
