(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature MLTON_ITIMER =
   sig
      datatype t =
	 Prof
       | Real
       | Virtual

      val set: t * {interval: Time.time, value: Time.time} -> unit
      val signal: t -> Posix.Signal.signal
   end
