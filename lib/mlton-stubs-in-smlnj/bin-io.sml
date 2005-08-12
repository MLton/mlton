(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure BinIO =
   struct
      open OpenInt32 BinIO

      fun inputN (ins, n) = BinIO.inputN (ins, toInt n)
      fun canInput (ins, n) = BinIO.canInput (ins, toInt n)
   end
