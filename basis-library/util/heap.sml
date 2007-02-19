(* Copyright (C) 2007-2007 Wesley W. Terpstra
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Heap:
   sig
      (* Sorts the provided array relative to the lessthan argument*)
      val heapSort: 'a array * ('a * 'a -> bool) -> unit
      (* Precondition: array is 0+ true values followed by 0+ false values *)
      (* Finds the index of the first array entry where: f x = false *)
      val binarySearch: 'a array * ('a -> bool) -> int
   end =
   struct
      fun heapSort (a : 'a array, lessthan : 'a * 'a -> bool) =
         let 
            open Array
            
            (* Push the hole down until value > both children *)
            fun pushHoleDown ( hole, end_of_heap, value ) =
               let
                  val left_child = Int.+ (Int.* (hole, 2), 1)
                  val right_child = Int.+ (left_child, 1)
               in
                  (* Recursion: two children *)
                  if Int.< (right_child, end_of_heap)
                  then let val left_value = sub (a, left_child)
                           val right_value = sub (a, right_child)
                           val (bigger_child, bigger_value) =
                               if lessthan (left_value, right_value)
                               then (right_child, right_value)
                               else (left_child, left_value)
                       in  if lessthan (bigger_value, value)
                           then update (a, hole, value)
                           else (update (a, hole, bigger_value);
                                 pushHoleDown (bigger_child, end_of_heap, value))
                       end
                  (* Base case: one child *)
                  else if right_child = end_of_heap
                  then let val left_value = sub (a, left_child)
                       in  if lessthan (left_value, value)
                           then update (a, hole, value)
                           else (update (a, hole, left_value);
                                 update (a, left_child, value))
                       end
                  (* Base case: no children *)
                  else update (a, hole, value)
               end
            
            (* Move largest element to end_of_table, then restore invariant *)
            fun sortHeap end_of_heap =
               let val end_of_heap = Int.- (end_of_heap, 1)
               in  if end_of_heap = 0 then () else
                   let val value = sub (a, end_of_heap)
                   in  update (a, end_of_heap, sub (a, 0));
                       pushHoleDown (0, end_of_heap, value);
                       sortHeap end_of_heap
               end end
            
            (* Start at last node w/ parent, loop till 0: push down *)
            val heapSize = Array.length a
            fun heapify i =
               if i = 0 then () else
               let val i = Int.- (i, 1)
               in  pushHoleDown (i, heapSize, sub (a, i));
                   heapify i
               end
         in
            if Int.<= (heapSize, 1) then () else
            (heapify (Int.div (heapSize, 2)); sortHeap heapSize)
         end
      
      fun binarySearch (a : 'a array, f : 'a -> bool) =
         let
            fun loop (lower, upper) = 
               (* Base case: one element left *)
               if Int.- (upper, lower) = 1
               then if f (Array.sub (a, lower)) then upper else lower
               (* Recursive case: check middle *)
               else let val mid = Int.div (Int.+ (lower, upper), 2)
                    in  if f (Array.sub (a, mid))
                        then loop (mid, upper)
                        else loop (lower, mid)
                    end
            val size = Array.length a
         in
            if size = 0 then 0 else loop (0, size)
         end
   end
