(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Dijkstra (S : SHORTEST_PATH_STRUCTS): SHORTEST_PATH =
struct

open S

structure Heap = FibonacciHeap (structure Key = Weight)
structure Elt = Heap.Elt

fun shortestPath {graph, weight, source} =
   let
      val {get: Node.t -> Node.t Heap.Elt.t option, set, destroy} =
         Property.destGetSetOnce (Node.plist, Property.initConst NONE)
      val elt = valOf o get
      fun distanceOption n = Option.map (get n, Elt.key)
      val distance = valOf o distanceOption 
      val fringe: Node.t Heap.t = Heap.empty ()
      fun addToFringe (n: Node.t, d: Weight.t): unit =
         set (n, SOME (Heap.insert (fringe, d, n)))
      fun relax (n: Node.t, e: Edge.t): unit =
         let val n' = Edge.to e
            val d = Weight.+ (distance n, weight e)
         in case distanceOption n' of
            NONE => addToFringe (n', d)
          | SOME d' => if Weight.< (d, d')
                           then Heap.decreaseKey (fringe, elt n', d)
                       else ()
         end
   in addToFringe (source, Weight.zero)
      ; while not (Heap.isEmpty fringe)
        do let val n = Heap.deleteMin fringe
           in List.foreach (Node.successors n, fn e => relax (n, e))
           end
      ; distanceOption
   end

end
