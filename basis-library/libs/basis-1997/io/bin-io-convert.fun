functor BinIOConvert
        (structure BinIO: BIN_IO) :
        BIN_IO_1997 =
  struct
     open BinIO

     structure StreamIO =
        struct
	   open StreamIO
	   val inputAll = #1 o inputAll
	end
  end
