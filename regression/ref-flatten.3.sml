(*
 * This example tests for a bug that was in refFlatten at one point.  The idea
 * is to allocate a ref cell outside a loop, and then allocate a tuple containing
 * the ref cell at each iteration of the loop.  At one point, refFlatten
 * mistakenly flattened the ref cell, which meant that it wasn't shared across
 * all the tuples allocated in the loop, as it should have been.
 *)
fun loop i =
   if i = 0
      then ()
   else
      let
         val r = ref 13
         val l = List.tabulate (10, fn i => (r, ref i))
         val (r1, r2) = List.nth (l, 0)
         val () = r1 := !r2
         val (r1, _) = List.nth (l, 1)
         val () = print (concat [Int.toString (!r1), "\n"])
      in
         loop (i - 1)
      end

val () = loop 2
