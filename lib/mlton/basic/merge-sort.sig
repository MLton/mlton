(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature MERGE_SORT =
   sig
      (* The comparison function ('a * 'a -> bool) for any of these should
       * always be the <= funtion, not just <
       * This is necessary to handle lists with duplicate elements.
       *)
      val make: ('a * 'a -> bool) -> {isSorted: 'a list -> bool,
				      merge: 'a list * 'a list -> 'a list,
				      sort: 'a list -> 'a list}
      val merge: 'a list * 'a list * ('a * 'a -> bool) -> 'a list
      val sort: 'a list * ('a * 'a -> bool) -> 'a list
   end
