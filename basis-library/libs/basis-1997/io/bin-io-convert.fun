(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

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
