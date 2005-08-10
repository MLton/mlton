(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t

signature TAB =
   sig
      val make: Out.t * string -> {reset: unit -> unit,
				    right: unit -> unit,
				    left: unit -> unit,
				    indent: unit -> unit}
   end
