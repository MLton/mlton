(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t

signature INSERTION_SORT =
   sig
      (* The comparison function ('a * 'a -> bool) for should be the <= funtion,
       * not just <.
       * This is necessary to handle duplicate elements.
       *)
      val sort: 'a array * ('a * 'a -> bool) -> unit
   end
