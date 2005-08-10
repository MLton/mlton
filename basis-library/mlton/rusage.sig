(* Copyright (C) 1999-2001 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
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
