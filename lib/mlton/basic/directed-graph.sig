(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature DIRECTED_GRAPH = 
   sig
      structure Node: 
         sig
            type 'a edge
            type 'a t

            val equals: 'a t * 'a t -> bool
            val hasEdge: {from: 'a t, to: 'a t} -> bool
            val layout: 'a t -> Layout.t
            val plist: 'a t -> PropertyList.t
            val successors: 'a t -> 'a edge list
         end
      structure Edge:
         sig
            type 'a t

            val equals: 'a t * 'a t -> bool
            val layout: 'a t -> Layout.t
            val plist: 'a t -> PropertyList.t
            val to: 'a t -> 'a Node.t
         end
      sharing type Node.edge = Edge.t

      (* depth first search *)
      structure DfsParam:
         sig
            type ('a, 'b, 'c, 'd, 'e) t =
               'b
               * ('a Node.t * 'b
                  -> ('c
                      * ('a Node.t * 'c -> ('d
                                            * ('a Edge.t * 'd -> 'd)
                                            * ('a Edge.t * 'd -> 'c * ('e -> 'd))
                                            * ('d -> 'e)))
                      * ('e -> 'b)))
            type ('a, 'b) u = ('a, 'b, 'b, 'b, 'b) t

            val discoverFinishTimes:
               unit -> (('a, int) u * {discover: 'a Node.t -> int,
                                       finish: 'a Node.t -> int,
                                       destroy: unit -> unit})
            val finishNode: ('a Node.t -> unit) -> ('a, unit) u
            val startNode: ('a Node.t -> unit) -> ('a, unit) u
         end

      (* the main graph type *)
      type 'a t
      type 'a u

      val addEdge: 'a t * {from: 'a Node.t, to: 'a Node.t} -> 'a Edge.t
      val coerce: 'a t -> unit t * {edge: 'a Edge.t -> unit Edge.t,
                                    node: 'a Node.t -> unit Node.t}
      val dfs: 'a t * ('a, 'b, 'c, 'd, 'e) DfsParam.t -> 'b
      val dfsForest: 'a t * {roots: 'a Node.t vector,
                             nodeValue: 'a Node.t -> 'b} -> 'b Tree.t vector
      val dfsNodes: 'a t * 'a Node.t list * ('a, 'b, 'c, 'd, 'e) DfsParam.t -> 'b
      val dfsTree: 'a t * {root: 'a Node.t,
                           nodeValue: 'a Node.t -> 'b} -> 'b Tree.t
      val display:
         {graph: 'a t,
          layoutNode: 'a Node.t -> Layout.t,
          display: Layout.t -> unit} -> unit
      (* dominators (graph, {root})
       * Returns the immediate dominator relation for the subgraph of graph
       * rooted at root.
       *  idom n = Root           if n = root
       *  idom n = Idom n'        where n' is the immediate dominator of n
       *  idom n = Unreachable    if n is not reachable from root
       *)
      datatype 'a idomRes =
         Idom of 'a Node.t
       | Root
       | Unreachable
      val dominators: 'a t * {root: 'a Node.t} -> {idom: 'a Node.t -> 'a idomRes}
      val dominatorTree: 'a t * {root: 'a Node.t,
                                 nodeValue: 'a Node.t -> 'b} -> 'b Tree.t
      val foreachDescendent: 'a t * 'a Node.t * ('a Node.t -> unit) -> unit
      val foldNodes: 'a t * 'b * ('a Node.t * 'b -> 'b) -> 'b
      val foreachEdge: 'a t * ('a Node.t * 'a Edge.t -> unit) -> unit
      val foreachNode: 'a t * ('a Node.t -> unit) -> unit
      (* ignoreNodes (g, f) builds a graph g' that looks like g, except that g'
       * does not contain nodes n such that f n, and that for every path in g
       * of the form n0 -> n1 -> ... -> nm, where n0 and nm are not ignored and
       * n1, ..., n_m-1 are ignored, there is an edge in g'.
       *)
      val ignoreNodes:
         'a t * ('a Node.t -> bool)
         -> 'a u t * {destroy: unit -> unit,
                      newNode: 'a Node.t -> 'a u Node.t}
      val layoutDot:
         'a t * ({nodeName: 'a Node.t -> string}
                 -> {edgeOptions: 'a Edge.t -> Dot.EdgeOption.t list,
                     nodeOptions: 'a Node.t -> Dot.NodeOption.t list,
                     options: Dot.GraphOption.t list,
                     title: string})
         -> Layout.t
      structure LoopForest: 
         sig 
            type 'a t

            val dest: 'a t -> {loops: {headers: 'a Node.t vector,
                                       child: 'a t} vector,
                               notInLoop: 'a Node.t vector}
            val layoutDot:
               'a t * {nodeName: 'a Node.t -> string,
                       options: Dot.GraphOption.t list,
                       title: string}
               -> Layout.t
         end
      val loopForestSteensgaard: 'a t * {root: 'a Node.t} -> 'a LoopForest.t
      val new: unit -> 'a t
      val newNode: 'a t -> 'a Node.t
      val nodes: 'a t -> 'a Node.t list
      val numNodes: 'a t -> int
      (* quotient (g, v)
       * Pre: v should be an equivalence relation on the nodes of g.  That is,
       *   each node in g should appear exactly once in some vector in v.
       * The result is a graph with one node per equivalence class, and an edge
       * between classes iff there is an edge between nodes in the classes.
       *)
      val quotient:
         'a t * ('a Node.t vector vector)
         -> 'a u t * {destroy: unit -> unit,
                      newNode: 'a Node.t -> 'a u Node.t}
      (* Removes node and incident edges. *)
      val removeNode: 'a t * 'a Node.t -> unit
      val removeDuplicateEdges: 'a t -> unit
      (* Strongly-connected components.
       * Each component is given as a list of nodes.
       * The components are returned topologically sorted.
       *)
      val stronglyConnectedComponents: 'a t -> 'a Node.t list list
      val subgraph:
         'a t * ('a Node.t -> bool)
         -> 'a u t * {destroy: unit -> unit,
                      newNode: 'a Node.t -> 'a u Node.t}
      (* topologicalSort g returns NONE if there is a cycle in g.
       * Otherwise, returns then nodes in g in a list such that if there is a
       * path in g from n to n', then n appears before n' in the list.
       *)
      val topologicalSort: 'a t -> 'a Node.t list option
      val transpose: 'a t -> 'a u t * {destroy: unit -> unit,
                                       newNode: 'a Node.t -> 'a u Node.t}
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
                    ignore (addEdge (g, {from = node from, to = node to})))
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
                              ignore (addEdge (g2, {from = newNode n',
                                                    to = newNode n}))
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
