(* Copyright (C) 2002-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
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
