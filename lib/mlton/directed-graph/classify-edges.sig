
      val classifyEdges: Graph.t -> {discover: Graph.Node.t -> int,
				      finish: Graph.Node.t -> int}
	 -> {tree: Graph.Edge.t list ref,
	     forward: Graph.Edge.t list ref,
	     back: Graph.Edge.t list ref,
	     cross: Graph.Edge.t list ref}
	 * Param.t
