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
