(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
