(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
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
	    type ('a, 'b, 'c, 'd) t =
	       'a
	       * (Node.t * 'a
		  -> ('b
		      * (Node.t * 'b -> ('c
					 * (Edge.t * 'c -> 'c)
					 * (Edge.t * 'c -> 'b * ('d -> 'c))
					 * ('c -> 'd)))
		      * ('d -> 'a)))
	    type 'a u = ('a, 'a, 'a, 'a) t

	    val discoverFinishTimes:
	       unit -> (int u * {discover: Node.t -> int,
				 finish: Node.t -> int,
				 destroy: unit -> unit})
	    val finishNode: (Node.t -> unit) -> unit u
	    val startNode: (Node.t -> unit) -> unit u
	 end

      (* the main graph type *)
      type t

      val addEdge: t * {from: Node.t, to: Node.t} -> Edge.t
      val dfs: t * ('a, 'b, 'c, 'd) DfsParam.t -> 'a
      val dfsNodes: t * Node.t list * ('a, 'b, 'c, 'd) DfsParam.t -> 'a
      val dfsTree: t * {root: Node.t, nodeValue: Node.t -> 'a} -> 'a Tree.t
      val display:
	 {graph: t,
	  layoutNode: Node.t -> Layout.t,
	  display: Layout.t -> unit} -> unit
      (* dominators (graph, {root})
       * Returns the immediate dominator relation for the subgraph of graph
       * rooted at root.
       *  idom n = Root           if n = root
       *  idom n = Idom n'        where n' is the immediate dominator of n
       *  idom n = Unreachable    if n is not reachable from root
       *)
      datatype idomRes =
	 Idom of Node.t
       | Root
       | Unreachable
      val dominators: t * {root: Node.t} -> {idom: Node.t -> idomRes}
      val dominatorTree: t * {root: Node.t, nodeValue: Node.t -> 'a} -> 'a Tree.t
      val foreachDescendent: t * Node.t * (Node.t -> unit) -> unit
      val foldNodes: t * 'a * (Node.t * 'a -> 'a) -> 'a
      val foreachEdge: t * (Node.t * Edge.t -> unit) -> unit
      val foreachNode: t * (Node.t -> unit) -> unit
      (* ignoreNodes (g, f) builds a graph g' that looks like g, except that g'
       * does not contain nodes n such that f n, and that for every path in g
       * of the form n0 -> n1 -> ... -> nm, where n0 and nm are not ignored and
       * n1, ..., n_m-1 are ignored, there is an edge in g'.
       *)
      val ignoreNodes:
	 t * (Node.t -> bool) -> t * {destroy: unit -> unit,
				      newNode: Node.t -> Node.t}
      val layoutDot:
	 t * ({nodeName: Node.t -> string}
	      -> {edgeOptions: Edge.t -> Dot.EdgeOption.t list,
		  nodeOptions: Node.t -> Dot.NodeOption.t list,
		  options: Dot.GraphOption.t list,
		  title: string})
	 -> Layout.t
      structure LoopForest: 
	 sig 
	   datatype t = T of {loops: {headers: Node.t vector,
				      child: t} vector,
			      notInLoop: Node.t vector}
	 end
      val loopForestSteensgaard: t * {root:Node.t} -> LoopForest.t
      val new: unit -> t
      val newNode: t -> Node.t
      val nodes: t -> Node.t list
      val numNodes: t -> int
      (* quotient (g, v)
       * Pre: v should be an equivalence relation on the nodes of g.  That is,
       *   each node in g should appear exactly once in some vector in v.
       * The result is a graph with one node per equivalence class, and an edge
       * between classes iff there is an edge between nodes in the classes.
       *)
      val quotient:
	 t * (Node.t vector vector)
	 -> t * {destroy: unit -> unit,
		 newNode: Node.t -> Node.t}
      (* Strongly-connected components.
       * Each component is given as a list of nodes.
       * The components are returned topologically sorted.
       *)
      val stronglyConnectedComponents: t -> Node.t list list
      val subgraph: t * (Node.t -> bool) -> t * {destroy: unit -> unit,
						 newNode: Node.t -> Node.t}
      (* topologicalSort g returns NONE if there is a cycle in g.
       * Otherwise, returns then nodes in g in a list such that if there is a
       * path in g from n to n', then n appears before n' in the list.
       *)
      val topologicalSort: t -> Node.t list option
      val transpose: t -> t * {destroy: unit -> unit,
			       newNode: Node.t -> Node.t}
   end


functor TestDirectedGraph (S: DIRECTED_GRAPH): sig end =
struct

open S

(* Section 7.3 of Muchnick. *)
local
   val g = new ()
   val {get = name, set = setName, ...} =
      Property.getSetOnce (Node.plist,
			   Property.initRaise ("name", Node.layout))
   val node = String.memoize (fn s =>
			      let
				 val n = newNode g
				 val _ = setName (n, s)
			      in n
			      end)
   val _ =
      List.foreach ([("entry\nfoo", "B1"),
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
	  open Dot
       in
	  Layout.output (layoutDot
			 (g, fn _ =>
			  {title = "Muchnick",
			   options = [],
			   edgeOptions = fn _ => [],
			   nodeOptions = fn n => [NodeOption.label (name n)]}),
			 out)
	  ; Out.newline out
       end)
   val {idom} = dominators (g, {root = node "entry\nfoo"})
   val g2 = new ()
   val {get = oldNode, set = setOldNode, ...} =
      Property.getSetOnce (Node.plist,
			   Property.initRaise ("oldNode", Node.layout))
   val {get = newNode, ...} =
      Property.get (Node.plist,
		    Property.initFun (fn n =>
				      let
					 val n' = newNode g2
					 val _ = setOldNode (n', n)
				      in n'
				      end))
   val _ = foreachNode (g, fn n =>
			case idom n of
			   Idom n' =>
			      (addEdge (g2, {from = newNode n',
					     to = newNode n})
			       ; ())
			 | _ => ())
   val _ =
      File.withOut
      ("/tmp/z2.dot", fn out =>
       let
	  open Dot
       in
	  Layout.output
	  (layoutDot
	   (g2, fn _ =>
	    {title = "dom",
	     options = [],
	     edgeOptions = fn _ => [],
	     nodeOptions = fn n => [NodeOption.label (name (oldNode n))]}),
	   out)
	  ; Out.newline out
       end)
in
end

end
