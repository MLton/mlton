(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature COMPUTATION = 
   sig
      structure Time: TIME
	 
      type t

      val keepAll: t * (string -> bool) -> t
      val inspect: t -> unit
      val output: t * Out.t -> unit
      val outputCalls: t * Out.t -> unit
      val outputTimes: t * Out.t -> unit
      val time: t -> Time.t
   end
