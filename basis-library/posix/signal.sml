(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixSignal: POSIX_SIGNAL_EXTRA =
   struct
      open PosixPrimitive.Signal

      type signal = t

      val fromWord = fromInt o SysWord.toInt
      val toWord = SysWord.fromInt o toInt
   end
