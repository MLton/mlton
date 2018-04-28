(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

      val classifyEdges: Graph.t -> {discover: Graph.Node.t -> int,
                                      finish: Graph.Node.t -> int}
         -> {tree: Graph.Edge.t list ref,
             forward: Graph.Edge.t list ref,
             back: Graph.Edge.t list ref,
             cross: Graph.Edge.t list ref}
         * Param.t
