(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature QUICK_SORT =
   sig
      (* The comparison function ('a * 'a -> bool) for should be the <= funtion,
       * not just <.
       * This is necessary to handle duplicate elements.
       *)
      val sort: 'a array * ('a * 'a -> bool) -> unit
   end

functor TestQuickSort (S: QUICK_SORT): sig end =
struct

val _ = print "TestQuickSort\n"
   
open S

val _ =
   List.foreach
   ([Array.array (0, 0),
     Array.tabulate (100, fn _ => Random.int ())],
    fn a => sort (a, op <=))
   
end
