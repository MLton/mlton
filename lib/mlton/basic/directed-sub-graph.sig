(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DIRECTED_SUB_GRAPH = 
   sig
      (* the main graph type *)
      type t

      structure Node: 
         sig
            type graph
            type edge
            type t

            val equals: t * t -> bool
            val hasEdge: graph * {from: t, to: t} -> bool
            val layout: t -> Layout.t
            val plist: t -> PropertyList.t
            val successors: graph * t -> edge list
         end
      structure Edge:
         sig
            type graph
            type node
            type t

            val equals: t * t -> bool
            val from: graph * t -> node
            val plist: t -> PropertyList.t
            val to: graph * t -> node
         end
      sharing type Node.edge = Edge.t
      sharing type Edge.node = Node.t
      sharing type Node.graph = t
      sharing type Edge.graph = t

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

      (* create a sub-graph from a graph *)
      val subGraph: t * {nodeP: Node.t -> bool, edgeP: Edge.t -> bool} -> t
      val supGraph: t -> t

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
       * Returns idom, where
       *  idom n = the immediate dominator n.
       *  idom root = root.
       *)
      val dominators: t * {root: Node.t} -> {idom: Node.t -> Node.t}
      val dominatorTree: t * {root: Node.t, nodeValue: Node.t -> 'a} -> 'a Tree.t
      val foreachDescendent: t * Node.t * (Node.t -> unit) -> unit
      val foreachEdge: t * (Node.t * Edge.t -> unit) -> unit
      val foreachNode: t * (Node.t -> unit) -> unit
(*      exception Input *)
(*     val input: In.t * (In.t -> 'a)* (In.t -> 'b) -> t * 'a * (Edge.t -> 'b) *)
(*      val isCyclic: t -> bool*)
      val layoutDot:
         t * {title: string,
              options: Dot.GraphOption.t list,
              edgeOptions: Edge.t -> Dot.EdgeOption.t list,
              nodeOptions: Node.t -> Dot.NodeOption.t list} -> Layout.t
      val loopForest:
         {headers: (* graph *) Node.t list -> (* graph *) Node.t list,
          graph: t,
          root: (* graph *) Node.t}
         -> {forest: t,
             graphToForest: (* graph *) Node.t -> (* forest *) Node.t,
             headers: (* graph *) Node.t list -> (* graph *) Node.t list,
             isHeader: (* graph *) Node.t -> bool,
             loopNodes: (* forest *) Node.t -> (* graph *) Node.t list,
             parent: (* forest *) Node.t -> (* forest *) Node.t option}
      val loopForestSteensgaard:
         {graph: t,
          root: (* graph *) Node.t}
         -> {forest: t,
             graphToForest: (* graph *) Node.t -> (* forest *) Node.t,
             headers: (* graph *) Node.t list -> (* graph *) Node.t list,
             isHeader: (* graph *) Node.t -> bool,
             loopNodes: (* forest *) Node.t -> (* graph *) Node.t list,
             parent: (* forest *) Node.t -> (* forest *) Node.t option}
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
   end


functor TestDirectedSubGraph (S: DIRECTED_SUB_GRAPH): sig end =
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
                    ignore (addEdge (g, {from = node from, to = node to})))
   val _ =
      File.withOut
      ("/tmp/z.dot", fn out =>
       let
          open Dot
       in
          Layout.output (layoutDot
                         (g,
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
                        ignore (addEdge (g2, {from = newNode (idom n),
                                              to = newNode n})))
   val _ =
      File.withOut
      ("/tmp/z2.dot", fn out =>
       let
          open Dot
       in
          Layout.output
          (layoutDot
           (g2, {title = "dom",
                 options = [],
                 edgeOptions = fn _ => [],
                 nodeOptions = fn n => [NodeOption.label (name (oldNode n))]}),
           out)
          ; Out.newline out
       end)
in
end

end
