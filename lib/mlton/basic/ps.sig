(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature PS =
   sig
      structure State =
	 struct
	    datatype t =
	       Running | Sleeping
	 end

      val ps: unit -> {pid: Pid.t,
		       commandName: string,
		       args: string list,
		       state: State.t} list
   end
