(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SHORTEST_PATH_CHECK_STRUCTS =
   sig
      include DIRECTED_GRAPH
      structure Weight: WEIGHT
   end

signature SHORTEST_PATH_CHECK =
   sig
      include SHORTEST_PATH_CHECK_STRUCTS

      structure Answer:
         sig
            datatype t =
               Shortest
             | SourceNonZero
             | PredecessorReachable of Node.t * Edge.t
             | Relaxable of Node.t * Edge.t
             | NoPath of Node.t

            val layout: t * (Node.t -> Layout.t) -> Layout.t
         end

      val check: {graph: t,
                  source: Node.t,
                  weight: Edge.t -> Weight.t,
                  distance: Node.t -> Weight.t option}
         -> Answer.t
   end
