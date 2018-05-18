(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature QUICK_SORT =
   sig
      (* The comparison function ('a * 'a -> bool) for should be the <= funtion,
       * not just <.
       * This is necessary to handle duplicate elements.
       *)
      (* sortArray mutates the array it is passed and returns the same array *)
      val sortArray: 'a array * ('a * 'a -> bool) -> unit
      val sortList: 'a list * ('a * 'a -> bool) -> 'a list
      val sortVector: 'a vector * ('a * 'a -> bool) -> 'a vector
   end

functor TestQuickSort (S: QUICK_SORT): sig end =
struct

val _ = print "TestQuickSort\n"

open S

val _ =
   List.foreach
   ([Array.array (0, 0),
     Array.tabulate (100, fn _ => Random.natLessThan 10000)],
    fn a => ignore (sortArray (a, op <=)))

end
