(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure PosixSignal: POSIX_SIGNAL =
   struct
      open PosixPrimitive.Signal

      val toWord = SysWord.fromInt
      val fromWord = SysWord.toInt
   end
