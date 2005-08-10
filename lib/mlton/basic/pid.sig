(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature PID =
   sig
      type t
	 
      val current: unit -> t
      val equals: t * t -> bool
      val fromString: string -> t option
      val layout: t -> Layout.t
      val parent: unit -> t
      val toString: t -> string
   end
