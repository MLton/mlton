(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure OpenInt32 =
   struct
      val toInt = Pervasive.Int32.toInt
      val fromInt = Pervasive.Int32.fromInt
      val fromIntOpt =
         fn NONE => NONE
          | SOME i => SOME (fromInt i)
      val toIntOpt =
         fn NONE => NONE
          | SOME i => SOME (toInt i)
   end
