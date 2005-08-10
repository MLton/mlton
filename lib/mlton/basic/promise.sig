(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature PROMISE =
   sig
      type 'a t
	 
      exception Force

      val delay: (unit -> 'a) -> 'a t
      val force: 'a t -> 'a
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val lazy: (unit -> 'a) -> (unit -> 'a)
      val reset: 'a t * (unit -> 'a) -> unit
   end
