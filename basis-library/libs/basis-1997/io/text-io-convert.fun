functor TextIOConvert
        (structure TextIO: TEXT_IO) :
        TEXT_IO_1997 =
  struct
     open TextIO

     structure StreamIO =
        struct
	   open StreamIO
	   val inputAll = #1 o inputAll
	end
  end
