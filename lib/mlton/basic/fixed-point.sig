(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature FIXED_POINT = 
   sig
      val fix: {start: 'a,
		step: 'a -> 'a,
		equals: 'a * 'a -> bool} -> 'a

      val fix': ((unit -> unit) -> unit) -> unit
   end
