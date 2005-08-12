(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure TextIO =
   struct
      open OpenInt32 TextIO

      fun inputN (ins, n) = TextIO.inputN (ins, toInt n)
      fun canInput (ins, n) = TextIO.canInput (ins, toInt n)
   end
