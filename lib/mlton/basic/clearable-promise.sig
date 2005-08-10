(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature CLEARABLE_PROMISE =
   sig
      type 'a t

      exception Force

      val clear: 'a t -> unit
      val delay: (unit -> 'a) -> 'a t
      val force: 'a t -> 'a
   end
