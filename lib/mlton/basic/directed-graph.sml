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

   (*       fun removeSuccessor (Node {successors, ...}, n) =
    * 	 successors := List.removeFirst (!successors, fn Edge.Edge {to, ...} =>
    * 					equals (n, to))
    *)
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

(*fun removeEdge (_, {from, to}) = Node.removeSuccessor (from, to) *)

structure LayoutDot =
   struct
      open Layout

      fun escapeString s =
	 String.translate
	 (s, fn c =>
	  if Char.isPrint c
	     then (case c of
		      #"\"" => "\\\""
		      | _ => Char.toString c)
	  else
	     case c of
		#"\n" => "\\n"
	      | #"\t" => "\\t"
	      | c => concat ["\\", Int.format (Char.ord c, StringCvt.OCT)])

      val dquote = "\""
      fun quote s = concat [dquote, s, dquote]
      fun lab (name, value) = concat [name, " = ", quote value]
      fun optionsToString (opts: 'a list, toString: 'a -> string, sep): string =
	 concat (List.separate (List.map (opts, toString), concat [sep, " "]))
      fun layoutOptions z = str (optionsToString z)
      val boolToString = Bool.toString
      val intToString = Int.toString
      fun realToString x = Real.format (x, Real.Format.fix (SOME 2))
      fun real2ToString (x, y) =
	 concat [realToString x, ", ", realToString y]
	 
      datatype clusterRank =
	 Global
	| Local
	| None

      val clusterRankToString =
	 fn Global => "global"
	  | Local => "local"
	  | None => "none"

      datatype color =
	 Black
	  | Red
	  | HSB of real * real * real

      val colorToString =
	 fn Black => "black"
	  | Red => "red"
	  | HSB (h, s, b) => concat [realToString h, " ",
				     realToString s, " ",
				     realToString b]
	       

      datatype direction =
	 Backward
	  | Both
	  | Forward
	  | None

      val directionToString =
	 fn Backward => "backward"
	  | Both => "both"
	  | Forward => "forward"
	  | None => "none"

      datatype fontFamily =
	 Courier
	  | Helvetica
	  | Symbol
	  | Times

      val fontFamilyToString =
	 fn Courier => "Courier"
	  | Helvetica => "Helvetica"
	  | Symbol => "Symbol"
	  | Times => "Times"
	       
      datatype fontWeight =
	 Bold
	  | Italic
	  | Roman

      val fontWeightToString =
	 fn Bold => "Bold"
	  | Italic => "Italic"
	  | Roman => "Roman"
	       
      type fontName = fontFamily * fontWeight
	 
      fun fontNameToString (f, w) =
	 concat [fontFamilyToString f, "-", fontWeightToString w]

      datatype orientation =
	 Landscape
	| Portrait

      val orientationToString =
	 fn Landscape => "landscape"
	  | Portrait => "portrait"
	       
      datatype polygonOption =
	 Distortion of real
	  | Orientation of int
	  | Peripheries of int
	  | Skew of real

      fun polygonOptionToString opt =
	 lab (case opt of
		 Distortion r => ("distortion", realToString r)
	       | Orientation i => ("orientation", intToString i)
	       | Peripheries p => ("peripheries", intToString p)
	       | Skew s => ("skew", realToString s))

      datatype rank = Max | Min | Same

      val rankToString =
	 fn Max => "max"
	  | Min => "min"
	  | Same => "same"

      datatype rankDir =
	 LeftToRight
	  | TopToBottom

      val rankDirToString =
	 fn LeftToRight => "LR"
	  | TopToBottom => "TB"

      datatype ratio =
	 Auto
	  | Compress
	  | Fill
	  | WidthOverHeight of real

      val ratioToString =
	 fn Auto => "auto"
	  | Compress => "compress"
	  | Fill => "fill"
	  | WidthOverHeight r => realToString r

      datatype shape =
	 Box
	  | Circle
	  | Diamond
	  | Ellipse
	  | Plaintext
	  | Polygon of {sides: int,
			options: polygonOption list}

      val shapeToString =
	 fn Box => "box"
	  | Circle => "circle"
	  | Diamond => "diamond"
	  | Ellipse => "ellipse"
	  | Plaintext => "plaintext"
	  | Polygon {sides, options} =>
	       concat
	       ["polygon, ",
		lab ("sides", intToString sides),
		case options of
		   [] => ""
		 | _ => concat [", ",
				optionsToString (options,
						 polygonOptionToString,
						 ",")]]

      datatype style =
	 BoldStyle
	  | Dashed
	  | Dotted
	  | Filled
	  | Invisible
	  | Solid

      val styleToString =
	 fn BoldStyle => "bold"
	  | Dashed => "dashed"
	  | Dotted => "dotted"
	  | Filled => "filled"
	  | Invisible => "invisible"
	  | Solid => "solid"

      structure EdgeOption =
	 struct
	    datatype t =
	       Color of color
	     | Decorate of bool (* connect edge label to edge *)
	     | Dir of direction
	     | FontColor of color
	     | FontName of fontName
	     | FontSize of int (* points *)
	     | Label of string
	     | Minlen of int
	     | Style of style
	     | Weight of int

	    fun toString opt =
	       lab (case opt of
		       Color c => ("color", colorToString c)
		     | Decorate d => ("decorate", boolToString d)
		     | Dir d => ("dir", directionToString d)
		     | FontColor c => ("fontcolor", colorToString c)
		     | FontName n => ("fontname", fontNameToString n)
		     | FontSize s => ("fontsize", intToString s)
		     | Label l => ("label", escapeString l)
		     | Minlen n => ("minlen", intToString n)
		     | Style s => ("style", styleToString s)
		     | Weight n => ("weight", intToString n))
	 end

      structure NodeOption =
	 struct
	    datatype t =
	       Color of color
	     | FontColor of color
	     | FontName of fontName
	     | FontSize of int (* points *)
	     | Height of real (* inches *)
	     | Label of string
	     | Shape of shape
	     | Width of real (* inches *)

	    fun toString opt =
	       lab (case opt of
		       Color c => ("color", colorToString c)
		     | FontColor c => ("fontcolor", colorToString c)
		     | FontName n => ("fontname", fontNameToString n)
		     | FontSize s => ("fontsize", intToString s)
		     | Height r => ("height", realToString r)
		     | Label l => ("label", escapeString l)
		     | Shape s => ("shape", shapeToString s)
		     | Width r => ("width", realToString r))
	 end

      structure GraphOption =
	 struct
	    datatype t =
	       Center of bool
	     | Color of color (* *)
	     | Concentrate of bool
	     | FontColor of color
	     | FontName of fontName
	     | FontSize of int (* points *)
	     | Label of string
	     | Margin of real * real (* inches *)
	     | Mclimit of real (* mincross iterations multiplier *)
	     | NodeSep of real (* inches *)
	     | Nslimit of int
	     | Orientation of orientation
	     | Page of {height: real, width: real} (* inches *)
	     | Rank of rank * Node.t list
	     | RankDir of rankDir
	     | RankSep of real (* inches *)
	     | Ratio of ratio
	     | Size of {height: real, width: real} (* inches *)

	    fun toString (opt, nodeId) =
	       case opt of
		  Center x => lab ("center", boolToString x)
		| Color x => lab ("color", colorToString x)
		| Concentrate x => lab ("concentrate", boolToString x)
		| FontColor x => lab ("fontcolor", colorToString x)
		| FontName x => lab ("fontname", fontNameToString x)
		| FontSize x => lab ("fontsize", intToString x)
		| Label x => lab ("label", escapeString x)
		| Margin x => lab ("margin", real2ToString x)
		| Mclimit x => lab ("mclimit", realToString x)
		| NodeSep x => lab ("nodesep", realToString x)
		| Nslimit n => lab ("nslimit", intToString n)
		| Orientation x => lab ("orientation", orientationToString x)
		| Page {height, width} =>
		     lab ("page", real2ToString (width, height))
		| RankDir x => lab ("rankdir", rankDirToString x)
		| Rank (r, ns) =>
		     concat ["{ ",
			     lab ("rank ", rankToString r),
			     "; ",
			     concat (List.map (ns, fn n =>
					       concat [nodeId n, " "])),
			     "}"]
		| RankSep x => lab ("ranksep", realToString x)
		| Ratio x => lab ("ratio", ratioToString x)
		| Size {height, width} =>
		     lab ("size", real2ToString (width, height))
	 end
      
      fun layout {graph = T {nodes, ...},
		  title: string,
		  options,
		  edgeOptions: Edge.t -> EdgeOption.t list,
		  nodeOptions: Node.t -> NodeOption.t list}: Layout.t =
	 let
	    val c = Counter.new 0
	    val {get = nodeId, destroy, ...} =
	       Property.destGet
	       (Node.plist,
		Property.initFun
		(fn _ => seq [str "n", str (Int.toString (Counter.next c))]))
	    val displayMultipleEdges = true
	    val g =
	       align
	       [str (concat ["digraph \"", title, "\" {"]),
		layoutOptions
		(GraphOption.Label title :: options,
		 fn opt => GraphOption.toString (opt, Layout.toString o nodeId),
		 ";"),
		align (List.map
		       (!nodes, fn n as Node.Node {successors, ...} =>
			let val id = nodeId n
			in align
			   [seq [id,
				 str " [",
				 layoutOptions
				 (nodeOptions n, NodeOption.toString, ","),
				 str "]"],
			    align (List.map
				   (!successors, fn e =>
				    seq [id, str " -> ", nodeId (Edge.to e),
					 str " [",
					 layoutOptions (edgeOptions e,
							EdgeOption.toString,
							","),
					 str "]"]))]
			end)),
		str "}"]
	 in destroy ()
	    ; g
	 end
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

(* fun removeBackEdges g =
 *    let
 *       val discoverTime = Counter.new 0
 *       val {get, destroy, ...} =
 * 	 Property.newDest
 * 	 (Node.plist, Property.initFun (fn _ => {time = Counter.next discoverTime,
 * 						alive = ref true}))
 *       val ignore = DfsParam.ignore
 *    in dfs
 *       (g, {startNode = fn n => (get n; ()),
 * 	   finishNode = fn n => #alive (get n) := false,
 * 	   handleNonTreeEdge =
 * 	   fn e as Edge.Edge {from, to, ...} =>
 * 	   let val {alive, time} = get to
 * 	   in if !alive andalso time < #time (get from)
 * 		 then removeEdge (g, e)
 * 	      else ()
 * 	   end,
 * 	   handleTreeEdge = ignore,
 * 	   startTree = ignore,
 * 	   finishTree = ignore,
 * 	   finishDfs = ignore})
 *    end
 *)

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
(*                         Random                         *)
(*--------------------------------------------------------*)
(*
fun maxNumEdges n = n * (n - 1)
   
fun random {numNodes,numEdges} =
   let val max = maxNumEdges numNodes
   in if numNodes < 0 orelse numEdges < 0 orelse numEdges > max
	 then Error.error "random"
      else let val g = new ()
	       val needed = ref numEdges
	       val remaining = ref max
	       fun maybeAddEdge (n,n') =
		  (if Int.random (1, !remaining) <= !needed
		      then (addEdge (g, Node.fromInt n, Node.fromInt n')
			 ; IntRef.dec needed)
		   else ()
		      ; IntRef.dec remaining)
	       val minNode = 0
	       val maxNode = numNodes - 1
	       fun directed n =
		  Int.foreach (0, maxNode, fn n' =>
			      if n = n' then () else maybeAddEdge (n,n'))
	       fun undirected n =
		  Int.foreach (n + 1, maxNode, fn n' => maybeAddEdge (n,n'))
	       val addEdges = if isDirected then directed
			      else undirected
	   in Int.foreach (minNode, maxNode, addEdges)
	      ; g
	   end
   end
*)
(*--------------------------------------------------------*)
(*                         Cycle                          *)
(*--------------------------------------------------------*)
(*
fun cycleParam g =
   let val {get = isActive, set = setActive} =
      nodeInfo (g, fn _ => false)
      val cycle = ref false
   in (cycle, {startNode = fn n => setActive (n, true),
	       finishNode = fn n => setActive (n, false),
	       handleNonTreeEdge =
	       fn (n, e) => let val n' = Edge.otherNode (e,n)
			   in if isActive n' then cycle := true
			      else ()
			   end,
			handleTreeEdge = DfsParam.ignore,
			startTree = DfsParam.ignore,
			finishTree = DfsParam.ignore,
			finishDfs = DfsParam.ignore})
   end

fun isCyclic g = let val (cycle, p) = cycleParam g
		 in dfs (g, p); !cycle
		 end
*)

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

(*--------------------------------------------------------*)
(*                       Transpose                        *)
(*--------------------------------------------------------*)
(*
fun transposeParam g =
   let val gt = new ()
      fun handleEdge (n, e) = let val n' = Edge.otherNode (e,n)
			    in addEdge (gt,n',n); ()
			    end
   in (gt, {handleTreeEdge = handleEdge,
	    handleNonTreeEdge = handleEdge,
	    finishDfs = DfsParam.ignore,
	    startNode = DfsParam.ignore, finishNode = DfsParam.ignore,
	    startTree = DfsParam.ignore, finishTree = DfsParam.ignore})
   end

fun transpose g = let val (gt, p) = transposeParam g
		  in dfs (g, p); gt
		  end
  *) 
(*--------------------------------------------------------*)
(*             Strongly Connected Components              *)
(*--------------------------------------------------------*)
   
(* from Cormen, Leiserson, and Rivest 23.5 *)
(*
fun sccCLR g =
   let
      val (gt, p) = transposeParam g
      val ns = ref []
      val p' = P.finishNode (fn n => List.push (ns,n))
      val components = ref []
      val component = ref []
      fun startNode n = List.push (component,n)
      fun startTree _ = component := []
      fun finishTree _ = List.push (components, !component)
      val pt = {startNode = startNode,
		   startTree = startTree,
		   finishTree = finishTree,
		   finishNode = DfsParam.ignore,
		   finishDfs = DfsParam.ignore,
		   handleTreeEdge = DfsParam.ignore,
		   handleNonTreeEdge = DfsParam.ignore}
   in dfs (g, P.combine (p, p'))
      ; dfsNodes (gt, !ns, pt)
      ; !components
   end
*)

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

fun validDominators {graph: t,
		     root: Node.t,
		     idom: Node.t -> Node.t}: bool =
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

fun dominators {graph, root} =
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
      val {get = nodeInfo: Node.t -> NodeInfo.t} =
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
      val _ =
	 if !dfnCounter = numNodes
	    then ()
	 else Error.bug "dominators: graph is not connected"
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
      val _ = Assert.assert ("dominators", fn () =>
			     validDominators {graph = graph,
					      root = root,
					      idom = idom})
   in {idom = idom}
   end

fun dominatorTree {graph: t, root: Node.t} =
   let
      val {idom} = dominators {graph = graph, root = root}
      val tree = new ()
      val {get = graphToTree} =
	 Property.get (Node.plist, Property.initFun (fn _ => newNode tree))
      val _ =
	 List.foreach
	 (nodes graph, fn n =>
	  if Node.equals (n, root)
	     then ()
	  else (addEdge (tree, {from = graphToTree (idom n),
				to = graphToTree n})
		; ()))
   in
      {tree = tree,
       graphToTree = graphToTree}
   end


end
