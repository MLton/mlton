(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
   
signature RESIZABLE_ARRAY =
   sig
      include ARRAY

      val addToEnd: 'a t * 'a -> unit
      val deleteLast: 'a t -> 'a
      val empty: unit -> 'a t
      val subOption: 'a t * int -> 'a option
   end
