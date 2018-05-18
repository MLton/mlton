(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MERGE_SORT =
   sig
      type 'a t

      (* The comparison function ('a * 'a -> bool) for any of these should
       * always be the <= funtion, not just <.
       * This is necessary to handle duplicate elements.
       *)
      val make: ('a * 'a -> bool) -> {isSorted: 'a t -> bool,
                                      merge: 'a t * 'a t -> 'a t,
                                      sort: 'a t -> 'a t}
      val isSorted: 'a t * ('a * 'a -> bool) -> 'a t
      val merge: 'a t * 'a t * ('a * 'a -> bool) -> 'a t
      val sort: 'a t * ('a * 'a -> bool) -> 'a t
   end

functor TestMergeSort (S: MERGE_SORT) =
struct

open S

val _ =
   Assert.assert
   ("TestMergeSort", fn () =>
    let
       fun check (l: int list): bool =
          List.insertionSort (l, op <=) = toList (sort (fromList l, op <=))
    in
       List.forall
       ([[],
         [1],
         [1,2],
         [1,2,3],
         [2,1,3],
         [1,2,3,4,5],
         [3,5,6,7,8,1,2,3,6,4]],
        check)
    end)

end
