(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature SHORTEST_PATH_STRUCTS =
   sig
      include DIRECTED_GRAPH
      structure Weight: WEIGHT
   end

signature SHORTEST_PATH =
   sig
      include SHORTEST_PATH_STRUCTS
	 
      val shortestPath: {graph: t,
			 weight: Edge.t -> Weight.t,
			 source: Node.t}
	 -> Node.t -> Weight.t option
   end
