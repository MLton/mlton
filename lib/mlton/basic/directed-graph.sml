(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure DirectedGraph: DIRECTED_GRAPH = 
struct

structure Types =
   struct
      datatype node = Node of {successors: edge list ref,
			       plist: PropertyList.t}
      and edge = Edge of {from: node,
			  to: node,
			  plist: PropertyList.t}
   end

structure Edge =
   struct
      datatype t = datatype Types.edge
	 
      local
	 fun make sel (Edge r) = sel r
      in
	 val from = make #from
	 val plist = make #plist
	 val to = make #to
      end
   end

structure Node =
   struct
      type edge = Types.edge
      datatype t = datatype Types.node
	 
      fun layout _ = Layout.str "node"

      fun successors (Node {successors, ...}) = !successors
      fun plist (Node {plist, ...}) = plist

      fun equals (n, n') = PropertyList.equals (plist n, plist n')

      fun new () = Node {successors = ref [],
			 plist = PropertyList.new ()}

      fun equals (n, n') = PropertyList.equals (plist n, plist n')

      fun hasEdge {from, to} =
	 List.exists (successors from, fn e => equals (to, Edge.to e))
   end

structure Edge =
   struct
      structure Node = Node
	 
      open Edge

      fun new {from, to} =
	 Edge {from = from,
	       to = to,
	       plist = PropertyList.new ()}
	 
      fun equals (e, e') = PropertyList.equals (plist e, plist e')
   end

structure DfsParam =
   struct
      type t = {startNode: Node.t -> unit,
		finishNode: Node.t -> unit,
		handleTreeEdge: Edge.t -> unit,
		handleNonTreeEdge: Edge.t -> unit,
		startTree: Node.t -> unit,
		finishTree: Node.t -> unit,
		finishDfs: unit -> unit}
	 
      fun ignore _ = ()

      fun finishNode f = {finishNode = f,
			  startNode = ignore,
			  handleTreeEdge = ignore,
			  handleNonTreeEdge = ignore,
			  startTree = ignore,
			  finishTree = ignore,
			  finishDfs = ignore}

      fun handleTreeEdge f = {finishNode = ignore,
			      startNode = ignore,
			      handleTreeEdge = f,
			      handleNonTreeEdge = ignore,
			      startTree = ignore,
			      finishTree = ignore,
			      finishDfs = ignore}


      fun startNode f = {finishNode = ignore,
			 startNode = f,
			 handleTreeEdge = ignore,
			 handleNonTreeEdge = ignore,
			 startTree = ignore,
			 finishTree = ignore,
			 finishDfs = ignore}
	 
      fun seq f g a = (f a; g a)
	 
      fun combine ({startNode, finishNode,
		    handleTreeEdge, handleNonTreeEdge,
		    startTree, finishTree, finishDfs}: t,
		   {startNode = sn, finishNode = fin,
		    handleTreeEdge = ht, handleNonTreeEdge = hn,
		    startTree = st, finishTree = ft, finishDfs = fd}: t): t =
	 {startNode = seq startNode sn,
	  finishNode = seq finishNode fin,
	  handleTreeEdge = seq handleTreeEdge ht,
	  handleNonTreeEdge = seq handleNonTreeEdge hn,
	  startTree = seq startTree st,
	  finishTree = seq finishTree ft,
	  finishDfs = seq finishDfs fd}
   end

(*---------------------------------------------------*)
(*                  graph datatype                   *)
(*---------------------------------------------------*)

datatype t = T of {nodes: Node.t list ref}

fun nodes (T {nodes, ...}) = !nodes

fun new () = T {nodes = ref []}
   
fun newNode (T {nodes, ...}) =
   let val n = Node.new ()
   in List.push (nodes, n)
      ; n
   end

fun addEdge (_, e as {from as Node.Node {successors, ...}, to}) =
   let val e = Edge.new e
   in List.push (successors, e)
      ; e
   end

fun layoutDot (T {nodes, ...},
	       mkOptions : {nodeName: Node.t -> string} ->
	       {edgeOptions: Edge.t -> Dot.EdgeOption.t list,
		nodeOptions: Node.t -> Dot.NodeOption.t list,
		options: Dot.GraphOption.t list,
		title: string}): Layout.t =
   let
      val ns = !nodes
      val c = Counter.new 0
      val {get = nodeId, rem, ...} =
	 Property.get
	 (Node.plist,
	  Property.initFun
	  (fn _ => concat ["n", Int.toString (Counter.next c)]))
      val {edgeOptions, nodeOptions, options, title} =
	 mkOptions {nodeName = nodeId}
      val nodes =
	 List.revMap
	 (ns, fn n as Node.Node {successors, ...} =>
	  {name = nodeId n,
	   options = nodeOptions n,
	   successors = List.revMap (!successors, fn e =>
				     {name = nodeId (Edge.to e),
				      options = edgeOptions e})})
      val res = 
	 Dot.layout {nodes = nodes,
		     options = options,
		     title = title}
      val _ = List.foreach (ns, rem)
   in
      res
   end

(*--------------------------------------------------------*)
(*                   Depth-First Search                   *)
(*--------------------------------------------------------*)

fun dfsNodes (g, ns, {startNode, finishNode,
		      handleTreeEdge, handleNonTreeEdge,
		      startTree, finishTree, finishDfs}) =
   let
      val {get = hasBeenVisited, set = setVisited, destroy, ...} =
	 Property.destGetSet (Node.plist, Property.initConst false)
      fun visit n =
	 (startNode n
	  ; setVisited (n, true)
	  ; List.foreach (Node.successors n, fn e =>
			  let val n' = Edge.to e
			  in if hasBeenVisited n'
				then handleNonTreeEdge e
			     else (visit n'; handleTreeEdge e)
			  end)
	  ; finishNode n)
   in List.foreach (ns, fn n =>
		    if hasBeenVisited n
		       then ()
		    else (startTree n; visit n; finishTree n))
      ; destroy ()
      ; finishDfs ()
   end

fun dfs (g, p) = dfsNodes (g, nodes g, p)

fun dfsTree (g, {root: Node.t, nodeValue: Node.t -> 'a}): 'a Tree.t =
   let
      val {get = nodeInfo, ...} =
	 Property.get
	 (Node.plist, Property.initFun (fn n => {children = ref [],
						 value = nodeValue n}))

      val handleTreeEdge =
	 fn Edge.Edge {from, to, ...} => List.push (#children (nodeInfo from), to)

      val _ = dfsNodes (g, [root], DfsParam.handleTreeEdge handleTreeEdge)

      fun treeAt (n: Node.t): 'a Tree.t =
	 let
	    val {children, value} = nodeInfo n
	 in
	    Tree.T (value, Vector.fromListMap (!children, treeAt))
	 end
   in
      treeAt root
   end

fun display {graph, layoutNode, display} =
   dfs (graph,
	DfsParam.startNode
	(fn n =>
	 display let open Layout
		 in seq [layoutNode n,
			 str " ",
			 list (List.revMap (Node.successors n,
					    layoutNode o Edge.to))]
		 end))
   
fun foreachDescendent (g, n, f) =
   dfsNodes (g, [n], DfsParam.finishNode f)

(*--------------------------------------------------------*)
(*                         Times                          *)
(*--------------------------------------------------------*)

fun discoverFinishTimes g =
   let val time: int ref = ref 0
      val {get = discover, set = setDiscover, destroy = destroyDiscover, ...} =
	 Property.destGetSetOnce (Node.plist,
				  Property.initRaise ("discover", Node.layout))
      val {get = finish, set = setFinish, destroy = destroyFinish, ...} =
	 Property.destGetSetOnce (Node.plist,
				  Property.initRaise ("finish", Node.layout))
   in {destroy = fn () => (destroyDiscover (); destroyFinish ()),
       discover = discover,
       finish = finish,
       param = {startNode = fn n => (Int.inc time; setDiscover (n, !time)),
		finishNode = fn n => (Int.inc time; setFinish (n, !time)),
		handleTreeEdge = DfsParam.ignore,
		handleNonTreeEdge = DfsParam.ignore,
		startTree = DfsParam.ignore,
		finishTree = DfsParam.ignore,
		finishDfs = DfsParam.ignore}}
   end

(*--------------------------------------------------------*)
(*                        Foreach                         *)
(*--------------------------------------------------------*)

fun foreachNode (g, f) = List.foreach (nodes g, f)
   
fun foreachEdge (g, edge) =
   foreachNode (g, fn n as Node.Node {successors, ...} =>
		List.foreach (!successors, fn e => edge (n, e)))

(*--------------------------------------------------------*)
(*                    Topological Sort                    *)
(*--------------------------------------------------------*)

exception TopologicalSort

fun topSortParam g =
   let
      val {get = amVisiting, set = setVisiting, destroy, ...} =
	 Property.destGetSet (Node.plist,
			     Property.initRaise ("visiting", Node.layout))
      val ns = ref []
   in (ns, {startNode = fn n => amVisiting n := true,
	    finishNode = fn n => (amVisiting n := false; List.push (ns,n)),
	    handleNonTreeEdge =
	    fn Edge.Edge {from, to, ...} => if ! (amVisiting to)
					      then raise TopologicalSort
					   else (),
	    startTree = DfsParam.ignore, finishTree = DfsParam.ignore,
	    handleTreeEdge = DfsParam.ignore,
	    finishDfs = destroy})
   end

fun topologicalSort g = let val (ns, p) = topSortParam g
			in dfs (g, p); !ns
			end

(* from Aho, Hopcroft, Ullman section 5.5 *)

fun stronglyConnectedComponents g =
   let
      val {get = discover: Node.t -> int, set = setDiscover,
	   destroy = destroyDiscover, ...} =
	 Property.destGetSetOnce (Node.plist,
				 Property.initRaise ("discover", Node.layout))
      val {get = low: Node.t -> int ref, destroy = destroyLow, ...} =
	 Property.destGet (Node.plist, Property.initFun (fn _ => ref ~1))
      val {get = isOnStack: Node.t -> bool, set = setOnStack,
	   destroy = destroyStack, ...} =
	 Property.destGetSet (Node.plist,
			     Property.initRaise ("isOnStack", Node.layout))
      val stack = ref []
      val components = ref []
      val time = ref 0
      fun pop () = let val n = List.pop stack
		  in setOnStack (n, false); n
		  end
      fun popTo n = let fun popTo () = let val n' = pop ()
				      in if Node.equals (n,n') then [n]
					 else n' :: (popTo ())
				      end
		    in popTo ()
		    end
      fun startNode n = (Int.inc time
			 ; setDiscover (n, !time)
			 ; low n := !time
			 ; setOnStack (n, true)
			 ; List.push (stack, n))
      fun finishNode n = if discover n = ! (low n)
			     then List.push (components, popTo n)
			 else ()
      fun updateLow (Edge.Edge {from, to, ...}) =
	 let val lto = ! (low to)
	    val lfrom = low from
	 in if lto < !lfrom
	       then lfrom := lto
	    else ()
	 end
      val handleTreeEdge = updateLow
      fun handleNonTreeEdge e = 
	 if isOnStack (Edge.to e)
	    then updateLow e
	 else ()
      val p = {startNode = startNode, finishNode = finishNode,
	       handleTreeEdge = handleTreeEdge,
	       handleNonTreeEdge = handleNonTreeEdge,
	       startTree = DfsParam.ignore, finishTree = DfsParam.ignore,
	       finishDfs = DfsParam.ignore}
   in dfs (g, p)
      ; destroyLow ()
      ; destroyStack ()
      ; destroyDiscover ()
      ; !components
   end

(*--------------------------------------------------------*)
(*                    Dominators                          *)
(*--------------------------------------------------------*)

(* This is an implementation of the Lengauer/Tarjan dominator algorithm, as
 * described on p. 185-191 of Muchnick's "Advanced Compiler Design and
 * Implementation"
 *)
structure NodeInfo =
   struct
      type t = {ancestor: Node.t ref,
		bucket: Node.t list ref,
		child: Node.t ref,
		dfn: int ref, (* depth first number *)
		idom: Node.t ref,
		label: Node.t ref,
		parent: Node.t ref,
		preds: Node.t list ref,
		sdno: int ref, (* semidominator dfn *)
		size: int ref}
   end

fun validDominators (graph,
		     {root: Node.t,
		      idom: Node.t -> Node.t}): bool =
   (* Check for each edge v --> w that idom w dominates v.
    * FIXME: It should first check that idom describes a tree rooted at root.
    *)
   DynamicWind.withEscape
   (fn escape =>
    let
       fun dominates (a: Node.t, b: Node.t): bool =
	  let
	     fun loop b =
		Node.equals (a, b)
		orelse (not (Node.equals (b, root))
			andalso loop (idom b))
	  in loop b
	  end
       val _ =
	  foreachEdge (graph, fn (_, Edge.Edge {from, to, ...}) =>
		       if dominates (idom to, from)
			  then ()
		       else escape false)
    in true
    end)

datatype idomRes =
   Idom of Node.t
  | Root
  | Unreachable

fun dominators (graph, {root}) =
   let
      val n0 = Node.new ()
      fun newNode (n: Node.t): NodeInfo.t =
	 {ancestor = ref n0,
	  bucket = ref [],
	  child = ref n0,
	  dfn = ref ~1,
	  idom = ref n0,
	  label = ref n,
	  parent = ref n0,
	  preds = ref [],
	  sdno = ref ~1,
	  size = ref 1}
      val {get = nodeInfo: Node.t -> NodeInfo.t, rem = remove, ...} =
	 Property.get (Node.plist, Property.initFun newNode)
      local
	 fun 'a make (sel: NodeInfo.t -> 'a ref) =
	    (sel o nodeInfo, ! o sel o nodeInfo)
      in
	 val (ancestor', ancestor) = make #ancestor
	 val (bucket', bucket) = make #bucket
	 val (child', child) = make #child
	 val (dfn', dfn) = make #dfn
	 val (idom', idom) = make #idom
	 val (label', label) = make #label
	 val (parent', parent) = make #parent
	 val (preds', preds) = make #preds
	 val (sdno', sdno) = make #sdno
	 val (size', size) = make #size
      end
      val _ = size' n0 := 0
      (* nodes is an array of nodes indexed by dfs number. *)
      val numNodes = List.length (nodes graph)
      val nodes = Array.new (numNodes, n0)
      fun ndfs i = Array.sub (nodes, i)
      val dfnCounter = ref 0
      fun dfs (v: Node.t): unit =
	 let
	    val i = !dfnCounter
	    val _ = Int.inc dfnCounter
	    val _ = dfn' v := i
	    val _ = sdno' v := i
	    val _ = Array.update (nodes, i, v)
	    val _ =
	       List.foreach
	       (Node.successors v, fn Edge.Edge {to = w, ...} =>
		let
		   val _ = List.push (preds' w, v)
		in if sdno w = ~1
		      then (parent' w := v
			    ; dfs w)
		   else ()
		end)
	 in ()
	 end
      val _ = dfs root
      val numNodes = !dfnCounter
      (* compress ancestor path to node v to the node whose label has the
       * maximal (minimal?) semidominator number. 
       *)
      fun compress (v: Node.t): unit =
	 if Node.equals (n0, ancestor (ancestor v))
	    then ()
	 else let
		 val _ = compress (ancestor v)
		 val _ =
		    if sdno (label (ancestor v)) < sdno (label v)
		       then label' v := label (ancestor v)
		    else ()
		 val _ = ancestor' v := ancestor (ancestor v)
	      in ()
	      end
      fun eval (v: Node.t): Node.t =
	 (* Determine the ancestor of v whose semidominator has the minimal
	  * depth-first number.
	  *)
	 if Node.equals (ancestor v, n0)
	    then label v
	 else let
		 val _ = compress v
	      in
		 if sdno (label (ancestor v)) >= sdno (label v)
		    then label v
		 else label (ancestor v)
	      end
      fun link (v: Node.t, w: Node.t): unit =
	 let
	    fun loop s =
	       if sdno (label w) < sdno (label (child s))
		  then
		     if size s + size (child (child s)) >= 2 * size (child s)
			then (ancestor' (child s) := s
			      ; child' s := child (child s)
			      ; loop s)
		     else (size' (child s) := size s
			   ; ancestor' s := child s
			   ; loop (child s))
	       else s
	    val s = loop w
	    val _ = label' s := label w
	    val _ = size' v := size v + size w
	    val s =
	       if size v < 2 * size w
		  then
		     let
			val tmp = child v
			val _ = child' v := s
		     in tmp
		     end
	       else s
	    fun loop s =
	       if Node.equals (s, n0)
		  then ()
	       else (ancestor' s := v
		     ; loop (child s))
	    val _ = loop s
	 in ()
	 end
      val _ =
	 Int.forDown
	 (1, numNodes, fn i =>
	  let
	     (* Compute initial values for semidominators and store nodes with
	      * the same semidominator in the same bucket.
	      *)
	     val w = ndfs i
	     val min = List.fold (preds w, sdno w, fn (n, min) =>
				  Int.min (min, sdno (eval n)))
	     val _ = sdno' w := min
	     val _ = List.push (bucket' (ndfs min), w)
	     val _ = link (parent w, w)
	     (* Compute immediate dominators for nodes in the bucket of w's
	      * parent.
	      *)
	     val _ =
		List.foreach
		(bucket (parent w), fn v =>
		 let
		    val u = eval v
		 in
		    idom' v := (if sdno u < sdno v
				   then u
				else parent w)
		 end)
	     val _ = bucket' (parent w) := []
	  in ()
	  end)
      (* Adjust immediate dominators of nodes whose current version of the
       * immediate dominator differs from the node with the depth-first number
       * of the node's semidominator.
       *)
      val _ =
	 Int.for
	 (1, numNodes, fn i =>
	  let
	     val w = ndfs i
	  in
	     if Node.equals (idom w, ndfs (sdno w))
		then ()
	     else idom' w := idom (idom w)
	  end)
      val _ = idom' root := root
(*       val _ = Assert.assert ("dominators", fn () =>
 *  			     validDominators (graph, {root = root,
 *  						      idom = idom}))
 *)
      val {get = idomFinal, set = setIdom, ...} =
	 Property.getSetOnce (Node.plist, Property.initConst Unreachable)
      val _ = setIdom (root, Root)
      val _ = Int.for (1, numNodes, fn i =>
		       let
			  val n = ndfs i
		       in
			  setIdom (n, Idom (idom n))
		       end)
      val _ = Int.for (0, numNodes, fn i => remove (ndfs i))
   in {idom = idomFinal}
   end

fun dominatorTree (graph, {root: Node.t, nodeValue: Node.t -> 'a}): 'a Tree.t =
   let
      val {idom} = dominators (graph, {root = root})
      val {get = nodeInfo, ...} =
	 Property.get (Node.plist,
		       Property.initFun (fn n => {children = ref [],
						  value = nodeValue n}))
      val _ =
	 List.foreach
	 (nodes graph, fn n =>
	  case idom n of
	     Idom n' => List.push (#children (nodeInfo n'), n)
	   | Root => ()
	   | Unreachable => ())
      fun treeAt (n: Node.t): 'a Tree.t =
	 let
	    val {children, value} = nodeInfo n
	 in
	    Tree.T (value, Vector.fromListMap (!children, treeAt))
	 end
   in
      treeAt root
   end

(*--------------------------------------------------------*)
(*                   Loop Forest                          *)
(*--------------------------------------------------------*)

(* This is an implementation of the G. Ramalingam loop forest construction,
 * as described in "On Loops, Dominators, and Dominance Frontiers"
 * (originally in PLDI00; revised technical report at
 * http://www.research.ibm.com/people/r/rama/Papers/ibmtr21513.revised.ps).
 *)

structure LoopForest =
   struct
      (* Every node in the graph will appear exactly once in a notInLoop
       * vector in the loop forest.
       * Every node that is a loop header will appear in exactly one headers
       * vector.
       *)
      datatype t = T of {loops: {headers: Node.t vector,
				 child: t} vector,
			 notInLoop: Node.t vector}

      val empty = T {loops = Vector.new0 (),
		     notInLoop = Vector.new0 ()}

      fun single n = T {loops = Vector.new0 (),
			notInLoop = Vector.new1 n}

      fun layoutDot (forest: t,
		     {nodeName: Node.t -> string,
		      options: Dot.GraphOption.t list,
		      title: string}) =
	 let
	    open Dot
	    fun label ns =
	       NodeOption.label
	       (Layout.toString (Vector.layout (Layout.str o nodeName) ns))
	    val c = Counter.new 0
	    fun newName () = concat ["n", Int.toString (Counter.next c)]
	    val nodes = ref []
	    fun loop (T {loops, notInLoop}) =
	       let
		  val n = newName ()
		  val _ = List.push (nodes, {name = n,
					     options = [label notInLoop,
							NodeOption.Shape Box],
					     successors = []})
	       in
		  Vector.fold
		  (loops, [n], fn ({headers, child}, ac) =>
		   let
		      val n = newName ()
		      val _ =
			 List.push
			 (nodes, {name = n,
				  options = [label headers,
					     NodeOption.Shape Ellipse],
				  successors =
				  List.revMap (loop child, fn n =>
					       {name = n, options = []})})
		   in
		      n :: ac
		   end)
	       end
	    val _ = loop forest
	 in
	    Dot.layout {nodes = !nodes,
			options = options,
			title = title}
	 end   
   end

(* This code assumes everything is reachable from the root.
 * Otherwise it may loop forever.
 *)
fun loopForestSteensgaard (g: t, {root: Node.t}): LoopForest.t =
   let
      val {get =
	   nodeInfo:
	   Node.t -> {class: int ref,
		      isHeader: bool ref,
		      (* The corresponding node in the next subgraph. *)
		      next: Node.t option ref,
		      (* The corresponding node in the original graph. *)
		      original: Node.t},
	   set = setNodeInfo, 
	   rem = remNodeInfo, ...} =
	 Property.getSet
	 (Node.plist, Property.initRaise ("loopForestSteensgaard", Node.layout))
      fun newNodeInfo (n, original) =
	 setNodeInfo (n, {class = ref ~1,
			  isHeader = ref false,
			  next = ref NONE,
			  original = original})
      val _ = List.foreach (nodes g, fn n => newNodeInfo (n, n))
      (* Treat the root as though there is an external edge into it. *)
      val _ = #isHeader (nodeInfo root) := true
      val c = Counter.new 0
      (* Before calling treeFor, nodeInfo must be defined for all nodes in g. *)
      fun treeFor (g: t): LoopForest.t  =
	 let
	    val sccs = stronglyConnectedComponents g
	    (* Put nodes in the same scc into the same class. *)
	    val _ = List.foreachi 
	            (sccs, fn (i, ns) =>
		     List.foreach
		     (ns, fn n =>
		      #class (nodeInfo n) := i))
	    (* Set nodes to be headers if they are the target of an edge whose
	     * source is in another scc.
             * This is a bit of an abuse of terminology, since it also marks
             * as headers nodes that are in their own trivial (one node) scc.
	     *)
	    val _ =
	       List.foreach
	       (nodes g, fn n =>
		let
		   val {class = ref class, ...} = nodeInfo n
		   val _ =
		      List.foreach
		      (Node.successors n, fn e =>
		       let
			  val {class = ref class', isHeader, ...} =
			     nodeInfo (Edge.to e)
		       in
			  if class = class'
			     then ()
			  else isHeader := true
		       end)
		in
		   ()
		end)
	    (* Accumulate the subtrees. *)
	    val loops = ref []
	    val notInLoop = ref []
	    val _ =
	       List.foreach
	       (sccs, fn ns =>
		case ns of
		   [n] =>
		      let
			 val {original, ...} = nodeInfo n
		      in
			 if List.exists (Node.successors n, fn e =>
					 Node.equals (n, Edge.to e))
			    then
			       List.push (loops,
					  {headers = Vector.new1 original,
					   child = LoopForest.single original})
			 else List.push (notInLoop, original)
		      end
		 | _ =>
		      let
			 (* Build a new subgraph of the component, sans edges
			  * that go into headers.
			  *)
			 val g' = new ()
			 val headers = ref []
			 (* Create all the new nodes. *)
			 val _ =
			    List.foreach
			    (ns, fn n =>
			     let
				val {next, original, ...} = nodeInfo n
				val n' = newNode g'
				val _ = next := SOME n'
				val _ = newNodeInfo (n', original)
			     in
				()
			     end)
			 (* Add all the edges. *)
			 val _ =
			    List.foreach
			    (ns, fn from =>
			     let
				val {class = ref class, isHeader, next,
				     original, ...} = nodeInfo from
				val from' = valOf (!next)
				val _ =
				   if !isHeader
				      then List.push (headers, original)
				   else ()
			     in
				List.foreach
				(Node.successors from, fn e =>
				 let
				    val to = Edge.to e
				    val info as {class = ref class', 
						 isHeader = isHeader',
						 next = next', ...} =
				       nodeInfo to
				 in
				    if class = class'
				       andalso not (!isHeader')
				       then (addEdge (g', {from = from',
							   to = valOf (!next')})
					     ; ())
				    else ()
				 end)
			     end)
			 (* We're done with the old graph, so delete the
			  * nodeInfo.
			  *)
			 val _ = List.foreach (ns, remNodeInfo)
			 val headers = Vector.fromList (!headers)
			 val child = treeFor g'
		      in
			 List.push (loops, {child = child,
					    headers = headers})
		      end)
	 in
	    LoopForest.T {loops = Vector.fromList (!loops),
			  notInLoop = Vector.fromList (!notInLoop)}
	 end
   in
      treeFor g
   end

end
