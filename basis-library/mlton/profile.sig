(* Copyright (C) 2002-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature MLTON_PROFILE =
   sig
      structure Data:
	 sig
	    type t

	    val equals: t * t -> bool
	    val free: t -> unit
	    val malloc: unit -> t
	    val write: t * string -> unit
	 end

      val isOn: bool (* a compile-time constant *)
      val withData: Data.t * (unit -> 'a) -> 'a
   end
