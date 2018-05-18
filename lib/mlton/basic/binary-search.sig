(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BINARY_SEARCH =
   sig
      (* largest(a, f)
       * Pre: if 0 <= i < j < Array.size a
       *         andalso f (Array.sub (a, j))
       *      then f (Array.sub (a, i))
       * Returns the largest i such that f (Array.sub (a, i))
       *)
      val largest: 'a array * ('a -> bool) -> int option
      val search: 'a array * ('a -> order) -> int option
      (* smallest(a, f)
       * Pre: if 0 <= i < j < Array.size a
       *         and f(Array.sub(a, i))
       *      then f(Array.sub(a, j)
       * Returns the smallest i such that f(Array.sub(a, i))
       *)
      val smallest: 'a array * ('a -> bool) -> int option
   end

functor TestBinarySearch (S: BINARY_SEARCH): sig end =
struct

open S

val _ =
   Assert.assert
   ("TestBinarySearch", fn () =>
    let
       val n = 17
       val a = Array.fromList (Pervasive.List.tabulate (n, fn i => i))
    in Int.forall (0, n, fn i =>
                   SOME i = search (a, fn x => Int.compare (i, x)))
    end)

end
