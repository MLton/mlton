(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature MLTON_RUSAGE =
   sig
      type t = {utime: Time.time, (* user time *)
		stime: Time.time  (* system time *)
		}
	 
      val rusage: unit -> {children: t,
			   gc: t,
			   self: t}
   end
