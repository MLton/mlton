(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure PosixSignal: POSIX_SIGNAL_EXTRA =
   struct
      open PosixPrimitive.Signal

      type signal = t

      val fromWord = fromInt o SysWord.toInt
      val toWord = SysWord.fromInt o toInt
   end
