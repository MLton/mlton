(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*--------------------------------------------------------*)
(*                     Classify Edges                     *)
(*--------------------------------------------------------*)

fun classifyEdges g {discover: Node.t -> int,
                     finish: Node.t -> int} =
   let val cs = {tree = ref [], cross = ref [], back = ref [], forward = ref []}
      fun classify e = let val n = E.tail e
                           val n' = E.head e
                       in if discover n' > discover n then #forward cs
                          else if finish n' = ~1 then #back cs
                               else #cross cs
                       end
   in (cs, P.T{handleTreeEdge = LU.push (#tree cs),
               handleNonTreeEdge = fn e => LU.push (classify e) e,
               startNode = P.ignore, finishNode = P.ignore,
               startTree = P.ignore, finishTree = P.ignore,
               finishDfs = P.ignore})
   end
