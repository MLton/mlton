type int = Int.t

signature DIRECTED_GRAPH = 
   sig
      structure Node: 
	 sig
	    type edge
	    type t

	    val equals: t * t -> bool
	    val hasEdge: {from: t, to: t} -> bool
	    val layout: t -> Layout.t
	    val plist: t -> PropertyList.t
	    val successors: t -> edge list
	 end
      structure Edge:
	 sig
	    type t

	    val equals: t * t -> bool
	    val plist: t -> PropertyList.t
	    val to: t -> Node.t
	 end
      sharing type Node.edge = Edge.t

      (* depth first search *)
      structure DfsParam:
	 sig
	    type t = {startNode: Node.t -> unit,
		      finishNode: Node.t -> unit,
		      handleTreeEdge: Edge.t -> unit,
		      handleNonTreeEdge: Edge.t -> unit,
		      startTree: Node.t -> unit,
		      finishTree: Node.t -> unit,
		      finishDfs: unit -> unit}
	    val finishNode: (Node.t -> unit) -> t
	    val ignore: 'a -> unit
	    val combine: t * t -> t
	 end

      (* the main graph type *)
      type t

      val addEdge: t * {from: Node.t, to: Node.t} -> Edge.t
      val dfs: t * DfsParam.t -> unit
      val dfsNodes: t * Node.t list * DfsParam.t -> unit
      val discoverFinishTimes: t -> {discover: Node.t -> int,
				     finish: Node.t -> int,
				     destroy: unit -> unit,
				     param: DfsParam.t}
      val display:
	 {graph: t,
	  layoutNode: Node.t -> Layout.t,
	  display: Layout.t -> unit} -> unit
      (* dominators {graph, root}
       * Pre: All nodes in graph must be reachable from root.
       *      This condition is checked.
       *)
      val dominators:
	 {graph: t,
	  root: Node.t}
	 ->
	 (* The immediate dominator of each node.
	  * The root's immediate dominator is itself.
	  *)
	 {idom: Node.t -> Node.t}
      val dominatorTree:
	 {graph: t, root: Node.t}
	 -> {graphToTree: Node.t -> Node.t,
	     tree: t}
      val foreachDescendent: t * Node.t * (Node.t -> unit) -> unit
      val foreachEdge: t * (Node.t * Edge.t -> unit) -> unit
      val foreachNode: t * (Node.t -> unit) -> unit
(*      exception Input *)
(*     val input: In.t * (In.t -> 'a)* (In.t -> 'b) -> t * 'a * (Edge.t -> 'b) *)
(*      val isCyclic: t -> bool*)
(*      val layoutDaVinci: t * (Node.t -> Layout.t) -> Layout.t *)
      val new: unit -> t
      val newNode: t -> Node.t
      val nodes: t -> Node.t list
(*      val random: {numNodes: int, numEdges: int} -> t*)
(*      val removeBackEdges: t -> unit *)
      (* removeEdge fails if edge isn't there. *)
(*      val removeEdge: t * Edge.t -> unit *)
      (* Strongly-connected components.
       * Each component is given as a list of nodes.
       * The components are returned topologically sorted.
       *)
      val stronglyConnectedComponents: t -> Node.t list list
      exception TopologicalSort
      val topologicalSort: t -> Node.t list
(*      val transpose: t -> t *)

      structure LayoutDot:
	 sig
	    datatype color =
	       Black
	     | Red
	     | HSB of real * real * real
	    (* (Hue, Saturation, Brightness).  All between 0 and 1. *)
	    datatype direction =
	       Backward
	     | Both
	     | Forward
	     | None
	    datatype fontFamily =
	       Courier
	     | Helvetica
	     | Symbol
	     | Times
	    datatype fontWeight =
	       Bold
	     | Italic
	     | Roman
	    type fontName = fontFamily * fontWeight
	    datatype orientation =
	       Landscape
	     | Portrait
	    datatype polygonOption =
	       Distortion of real (* -1.0 <= r <= 1.0 *)
	     | Orientation of int (* 0 <= i <= 360.  Clockwise rotation from
				   * X axis in degrees.
				   *)
	     | Peripheries of int
	     | Skew of real (* -1.0 <= r <= 1.0 *)
	    datatype rank = Max | Min | Same
	    datatype rankDir =
	       LeftToRight
	     | TopToBottom
	    datatype ratio =
	       Auto
	     | Compress
	     | Fill
	     | WidthOverHeight of real
	    datatype shape =
	       Box
	     | Circle
	     | Diamond
	     | Ellipse
	     | Plaintext
	     | Polygon of {sides: int,
			   options: polygonOption list}
	    datatype style =
	       BoldStyle
	     | Dashed
	     | Dotted
	     | Filled
	     | Invisible
	     | Solid
	    structure EdgeOption:
	       sig
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
	       end
	    structure NodeOption:
	       sig
		  datatype t =
		     Color of color
		   | FontColor of color
		   | FontName of fontName
		   | FontSize of int (* points *)
		   | Height of real (* inches *)
		   | Label of string
		   | Shape of shape
		   | Width of real (* inches *)
	       end
	    structure GraphOption:
	       sig
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
		   | Nslimit of int (* network simplex limit *)
		   | Orientation of orientation
		   | Page of {height: real, width: real} (* inches *)
		   | Rank of rank * Node.t list
		   | RankDir of rankDir
		   | RankSep of real (* inches *)
		   | Ratio of ratio
		   | Size of {height: real, width: real} (* inches *)
	       end
	    
	    val layout: {graph: t,
			 title: string,
			 options: GraphOption.t list,
			 edgeOptions: Edge.t -> EdgeOption.t list,
			 nodeOptions: Node.t -> NodeOption.t list} -> Layout.t
	 end
   end


functor TestDirectedGraph (S: DIRECTED_GRAPH): sig end =
struct

open S

(* Section 7.3 of Muchnick. *)
local
   val g = new ()
   val {get = name, set = setName} =
      Property.getSetOnce (Node.plist,
			   Property.initRaise ("name", Node.layout))
   val node = String.memoize (fn s =>
			      let
				 val n = newNode g
				 val _ = setName (n, s)
			      in n
			      end)
   val _ =
      List.foreach ([("entry", "B1"),
		     ("B1", "B2"),
		     ("B1", "B3"),
		     ("B2", "exit"),
		     ("B3", "B4"),
		     ("B4", "B5"),
		     ("B4", "B6"),
		     ("B5", "exit"),
		     ("B6", "B4")], fn (from, to) =>
		    (addEdge (g, {from = node from, to = node to})
		     ; ()))
   val _ =
      File.withOut
      ("/tmp/z.dot", fn out =>
       let
	  open LayoutDot
       in
	  Layout.output (layout {graph = g,
				 title = "Muchnick",
				 options = [],
				 edgeOptions = fn _ => [],
				 nodeOptions = fn n => let open NodeOption
						       in [Label (name n)]
						       end},
			 out)
	  ; Out.newline out
       end)
   val {idom} = dominators {graph = g, root = node "entry"}
   val g2 = new ()
   val {get = oldNode, set = setOldNode} =
      Property.getSetOnce (Node.plist,
			   Property.initRaise ("oldNode", Node.layout))
   val {get = newNode} =
      Property.get (Node.plist,
		    Property.initFun (fn n =>
				      let
					 val n' = newNode g2
					 val _ = setOldNode (n', n)
				      in n'
				      end))
   val _ = foreachNode (g, fn n =>
			(addEdge (g2, {from = newNode (idom n),
				       to = newNode n})
			 ; ()))
   val _ =
      File.withOut
      ("/tmp/z2.dot", fn out =>
       let
	  open LayoutDot
       in
	  Layout.output
	  (layout {graph = g2,
		   title = "dom",
		   options = [],
		   edgeOptions = fn _ => [],
		   nodeOptions = fn n => let open NodeOption
					 in [Label (name (oldNode n))]
					 end},
	   out)
	  ; Out.newline out
       end)
in
end

end
