
functor x86LoopInfo(S: X86_LOOP_INFO_STRUCTS) : X86_LOOP_INFO =
struct
  open S
  open x86

  structure Graph = DirectedGraph
  structure Node = Graph.Node
  structure Edge = Graph.Edge

  val tracer = x86.tracer

  datatype t = T of {loopForest : Label.t list Tree.t list,
		     getLoopInfo : Label.t -> 
		                   {loopHeader: bool,
				    treeAt: {up: Label.t list Tree.t,
					     down: Label.t list Tree.t} option}}
    
  fun createLoopInfo {chunk = Chunk.T {blocks, ...}, farLoops}
    = let
	val G = Graph.new ()

	val nodeInfo as {get = getNodeInfo : Node.t -> Label.t,
			 set = setNodeInfo, ...}
	  = Property.getSetOnce
	    (Node.plist,
	     Property.initRaise ("x86LoopInfo:getNodeInfo", Node.layout))

	val info as {get = getInfo : Label.t -> Node.t,
		     destroy = destInfo}
	  = Property.destGet
	    (Label.plist,
	     Property.initFun (fn l => let
					 val n = Graph.newNode G
					 val _ = setNodeInfo(n, l)
				       in
					 n
				       end))

	val loopInfo as {get = getLoopInfo : 
			       Label.t -> 
			       {loopHeader: bool,
				treeAt: {up: x86.Label.t list Tree.t,
					 down: x86.Label.t list Tree.t} option},
			 set = setLoopInfo, ...}
	  = Property.getSetOnce
	    (Label.plist,
	     Property.initRaise ("x86LoopInfo:getLoopInfo", Label.layout))

	val rootLabel = Label.newString "root"
	val root = getInfo rootLabel

	fun addEdge edge
	  = (Graph.addEdge (G, edge) ; ())
	fun addEdge' edge
	  = if Node.hasEdge edge
	      then ()
	      else addEdge edge

	val _
	  = List.foreach
	    (blocks,
	     fn block as Block.T {entry, transfer, ...}
	      => let
		   val label = Entry.label entry
		   val node = getInfo label

		   fun doit' target
		     = let
			 val node' = getInfo target
		       in
			 addEdge {from = node, to = node'}
		       end
		   fun doit'' target
		     = let
			 val node' = getInfo target
		       in
			 if farLoops
			   then addEdge {from = node, to = node'}
			   else addEdge {from = root, to = node'}
		       end

		   datatype z = datatype Transfer.t
		 in
		   if Entry.isFunc entry
		     then addEdge {from = root, to = node}
		     else () ;
		   case transfer
		     of Goto {target, ...} 
		      => doit' target
		      | Iff {truee, falsee, ...} 
		      => (doit' truee; 
			  doit' falsee)
		      | Switch {cases, default, ...}
		      => (doit' default;
			  Transfer.Cases.foreach(cases, doit'))
		      | Tail {...}
		      => ()
		      | NonTail {return, handler, ...}
		      => (doit'' return;
			  case handler 
			    of SOME handler => doit'' handler
			     | NONE => ())
		      | Return {...}
		      => ()
		      | Raise {...}
		      => ()
		      | Runtime {return, ...}
		      => (doit'' return)
		      | CCall {dst, return, ...}
		      => (doit' return)
		 end)

	val {forest, graphToForest, headers, isHeader, loopNodes, parent, ...}
	  = Graph.loopForestSteensgaard {graph = G, root = root}

	val loopForest
	  = let
	      val (finished_roots, unfinished_roots)
		= List.fold
		  (Graph.nodes forest,
		   ([],[]),
		   fn (node, (finished_roots, unfinished_roots)) 
		    => (case parent node
			  of NONE => (case Node.successors node
					of nil => (node::finished_roots,
						   unfinished_roots)
					 | _ => (finished_roots,
						 node::unfinished_roots))
			   | SOME _ => (finished_roots, unfinished_roots)))

	      fun doit {node, up}
		= let
		    val loopLabels = List.map(loopNodes node, getNodeInfo)

		    val (finished_nodes, unfinished_nodes)
		      = List.fold
		        (Node.successors node,
			 ([], []),
			 fn (edge, (finished_nodes, unfinished_nodes))
			  => let
			       val node = Edge.to edge
			     in
			       case Node.successors node
				 of nil => (node::finished_nodes,
					    unfinished_nodes)
				  | _ => (finished_nodes,
					  node::unfinished_nodes)
			     end)

		    val up = Tree.T (loopLabels, Vector.fromList up)
		    val down 
		      = Tree.T (loopLabels, 
				Vector.fromListMap
				(unfinished_nodes, fn node =>
				 doit {node = node, up = [up]}))
		  in
		    List.foreach
		    (finished_nodes, 
		     fn node => let
				  val node' = hd (loopNodes node)
				in 
				  setLoopInfo
				  (getNodeInfo node', 
				   {loopHeader = isHeader node',
				    treeAt = SOME {up = up, 
						   down = down}})
				end) ;
		    down
		  end
	    in
	      List.foreach
	      (finished_roots,
	       fn node => let
			    val node' = hd (loopNodes node)
			  in 
			    setLoopInfo(getNodeInfo node', 
					{loopHeader = isHeader node',
					 treeAt = NONE})
			  end) ;
	      List.map
	      (unfinished_roots,
	       fn node => doit {node = node, up = []})
	    end

	val _ = destInfo ()
      in
	T {loopForest = loopForest, getLoopInfo = getLoopInfo}
      end

  val (createLoopInfo, createLoopInfo_msg)
    = tracer
      "createLoopInfo"
      createLoopInfo
    
  fun getLoopTreeAt (T {getLoopInfo, ...}, label) = #treeAt (getLoopInfo label)

  fun getLoopForest (T {loopForest, ...}) = loopForest

  fun getLoopDepth (T {getLoopInfo, ...}, l)
    = (case (#treeAt (getLoopInfo l))
	 of NONE => NONE
          | SOME {up, ...}
	  => let
	       fun depth' (Tree.T (labels, tree), d)
		 = (case Vector.length tree
		      of 0 => d
		       | 1 => depth' (Vector.sub (tree, 0), d + 1)
		       | _ => Error.bug "x86LoopInfo:depth'")
	       fun depth tree = depth' (tree, 0: int)
	     in
	       SOME (depth up)
	     end)

  fun isLoopHeader (T {getLoopInfo, ...}, l)
    = #loopHeader (getLoopInfo l)

  fun getLoopDistance (T {getLoopInfo, ...}, from, to)
    = (case (#treeAt (getLoopInfo from), #treeAt (getLoopInfo to))
	 of (NONE, _) => NONE
	  | (_, NONE) => NONE
	  | (SOME {up = up_from, ...}, SOME {up = up_to, ...})
	  => let
	       fun depth' (Tree.T (labels, tree), d)
		 = (case Vector.length tree
		      of 0 => d
		       | 1 => depth' (Vector.sub (tree, 0), d + 1)
		       | _ => Error.bug "x86LoopInfo:depth'")
	       fun depth tree = depth' (tree, 0:int)

	       val Tree.T (labels_to, _) = up_to
	       val Tree.T (labels_from, _) = up_from
	     in
	       if List.contains(labels_to, from, Label.equals)
		  orelse
		  List.contains(labels_from, to, Label.equals)
		 then SOME ((depth up_to) - (depth up_from))
		 else NONE 
	     end)
	   
end
