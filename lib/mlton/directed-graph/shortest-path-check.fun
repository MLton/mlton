(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ShortestPathCheck (S: SHORTEST_PATH_CHECK_STRUCTS): SHORTEST_PATH_CHECK =
struct

open S

structure Answer =
   struct      
      datatype t =
         Shortest
       | SourceNonZero
       | PredecessorReachable of Node.t * Edge.t
       | Relaxable of Node.t * Edge.t
       | NoPath of Node.t

      fun layout (a, layoutNode) =
         let open Layout
         in case a of
            Shortest =>
               str "The distances are correct shortest path distances."
          | SourceNonZero => str "The distance to the source must be zero."
          | PredecessorReachable (n, e) =>
               let val n = Node.layout n
                  val n' = Node.layout (Edge.to e)
               in align
                  [str "The distances are contradictory.",
                   seq [str "1. There is an edge from ", n, str " to ", n'],
                   seq [str "2. ", n, str " has a finite distancEdge."],
                   seq [str "3. ", n', str " has an infinite distancEdge."]]
               end
          | NoPath n =>
               seq [str "There is not a valid predecessor path from ",
                   layoutNode n, str " to the sourcEdge."]
          | Relaxable (n, e) =>
               let val n = layoutNode n
                  val n' = layoutNode (Edge.to e)
               in align
                  [str "The distances are not shortest path distances.",
                   seq [str "The edge from ", n, str " to ", n',
                       str " can be relaxed."]]
               end
         end
   end

structure Set = DisjointSet

fun check {graph, source, weight, distance} =
   case distance source of
      NONE => Answer.SourceNonZero
    | SOME d => 
          if not (Weight.equals (Weight.zero, d))
             then Answer.SourceNonZero
          else
             let
                exception Answer of Answer.t
                val {get = set, destroy, ...} =
                   Property.destGet (Node.plist,
                                     Property.initFun (fn _ => Set.singleton ()))
                fun union (n, n') = Set.union (set n, set n')
                fun checkRelax (n, e) =
                   let val n' = Edge.to e
                   in case distance n of
                      NONE => () 
                    | SOME d =>
                         case distance n' of
                            NONE =>
                               raise Answer (Answer.PredecessorReachable (n, e))
                          | SOME d' =>
                               let val d'' = Weight.+ (d, weight e)
                               in if Weight.< (d'', d')
                                     then raise Answer (Answer.Relaxable (n, e))
                                  else if Weight.equals (d', d'')
                                          then union (n, n')
                                       else ()
                               end
                   end
                val _ = foreachEdge (graph, checkRelax)
                val sourceSet = set source
                fun canReachSource n =
                   let val equiv = Set.equals (set n, sourceSet)
                   in case distance n of
                      NONE => ()
                    | SOME _ => if equiv
                                   then ()
                                else raise Answer (Answer.NoPath n)
                   end
             in (foreachNode (graph, canReachSource)
                 ; Answer.Shortest) handle Answer a => a
             end

end
