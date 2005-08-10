(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature INIT_SCRIPT =
   sig
      val startStop: {name: string,
		      action: string,
		      log: File.t,
		      thunk: unit -> unit,
		      usage: string -> unit} -> unit
   end
