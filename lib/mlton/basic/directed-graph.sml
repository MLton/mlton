(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure DirectedGraph:> DIRECTED_GRAPH = 
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

      fun new () = Node {successors = ref [],
                         plist = PropertyList.new ()}

      fun equals (n, n') = PropertyList.equals (plist n, plist n')

      fun hasEdge {from, to} =
         List.exists (successors from, fn e => equals (to, Edge.to e))

      fun removeDuplicateSuccessors (Node {successors, ...}) =
         let
            val {get, rem, ...} =
               Property.get (plist, Property.initFun (fn _ => ref false))
            val es =
               List.fold (! successors, [], fn (e, ac) =>
                          let
                             val r = get (Edge.to e)
                          in
                             if !r
                                then ac
                             else (r := true ; e :: ac)
                          end)
            val () = List.foreach (es, rem o Edge.to)
            val () = successors := es
         in
            ()
         end
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

      fun layout e =
         Layout.record [("from", Node.layout (from e)),
                        ("to", Node.layout (to e))]
   end

(*---------------------------------------------------*)
(*                  graph datatype                   *)
(*---------------------------------------------------*)

datatype t = T of {nodes: Node.t list ref}

fun coerce g = (g, {edge = fn e => e,
                    node = fn n => n})

fun nodes (T {nodes, ...}) = !nodes

fun foldNodes (g, a, f) = List.fold (nodes g, a, f)

val numNodes = List.length o nodes

fun removeDuplicateEdges (g: t): unit =
   List.foreach (nodes g, Node.removeDuplicateSuccessors)

fun new () = T {nodes = ref []}

fun newNode (T {nodes, ...}) =
   let val n = Node.new ()
   in List.push (nodes, n)
      ; n
   end

fun removeNode (T {nodes, ...}, n) =
   let
      fun nodePred n' = Node.equals (n, n')
      fun edgePred (Edge.Edge {to = n', ...}) = nodePred n'
      val _ =
         nodes := List.removeAll (!nodes, nodePred)
      val _ =
         List.foreach
         (!nodes, fn Node.Node {successors, ...} =>
          successors := List.removeAll (!successors, edgePred))
   in
      ()
   end

fun addEdge (_, e as {from = Node.Node {successors, ...}, ...}) =
   let
      val e = Edge.new e
      val () = List.push (successors, e)
   in
      e
   end
fun addEdge' arg = ignore (addEdge arg)

fun layoutDot (T {nodes, ...},
               mkOptions:
               {nodeName: Node.t -> string}
               -> {edgeOptions: Edge.t -> Dot.EdgeOption.t list,
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

structure DfsParam =
   struct
      type ('a, 'b, 'c, 'd, 'e) t =
         'b
         * (Node.t * 'b
            -> ('c
                * (Node.t * 'c -> ('d
                                   * (Edge.t * 'd -> 'd)
                                   * (Edge.t * 'd -> 'c * ('e -> 'd))
                                   * ('d -> 'e)))
                * ('e -> 'b)))
      type ('a, 'b) u = ('a, 'b, 'b, 'b, 'b) t

      fun startFinishNode (a: 'a,
                           start: Node.t * 'a -> 'a,
                           finish: Node.t * 'a -> 'a): ('b, 'a) u =
         (a,
          fn (_, a) => (a,
                        fn (n, a) =>
                        let
                           val a = start (n, a)
                        in
                           (a, #2, fn (_, a) => (a, fn a => a),
                            fn a => finish (n, a))
                        end,
                        fn a => a))

      fun finishNode (f: Node.t -> unit) =
         startFinishNode ((), fn _ => (), f o #1)

      fun startNode (f: Node.t -> unit) =
         startFinishNode ((), f o #1, fn _ => ())

      fun discoverFinishTimes () =
         let
            val {get = discover, set = setDiscover,
                 destroy = destroyDiscover, ...} =
               Property.destGetSetOnce
               (Node.plist, Property.initRaise ("discover", Node.layout))
            val {get = finish, set = setFinish, destroy = destroyFinish, ...} =
               Property.destGetSetOnce
               (Node.plist, Property.initRaise ("finish", Node.layout))
         in
            (startFinishNode (0: int,
                              fn (n, t) => (setDiscover (n, t); t + 1),
                              fn (n, t) => (setFinish (n, t); t + 1)),
             {destroy = fn () => (destroyDiscover (); destroyFinish ()),
              discover = discover,
              finish = finish})
         end
   end

fun dfsNodes (_: t,
              ns: Node.t list,
              (b, f): ('a, 'b, 'c, 'd, 'e) DfsParam.t) =
   let
      type info = {hasBeenVisited: bool ref}
      val {get = nodeInfo: Node.t -> info, destroy, ...} =
         Property.destGetSet (Node.plist,
                              Property.initFun (fn _ =>
                                                {hasBeenVisited = ref false}))
      val b =
         List.fold
         (ns, b, fn (n, b) =>
          let
             val info as {hasBeenVisited} = nodeInfo n
          in
             if !hasBeenVisited
                then b
             else
                let
                   val (c, startNode, finishTree) = f (n, b)
                   fun visit (n: Node.t, {hasBeenVisited}: info, c: 'c): 'e =
                      let
                         val _ = hasBeenVisited := true
                         val (d, nonTreeEdge, treeEdge, finishNode) =
                            startNode (n, c)
                      in
                         finishNode
                         (List.fold
                          (Node.successors n, d,
                           fn (e, d) =>
                           let
                              val n = Edge.to e
                              val info as {hasBeenVisited} = nodeInfo n
                           in
                              if !hasBeenVisited
                                 then nonTreeEdge (e, d)
                              else
                                 let
                                    val (c, finish) = treeEdge (e, d)
                                 in
                                    finish (visit (n, info, c))
                                 end
                           end))
                      end
                in
                   finishTree (visit (n, info, c))
                end
          end)
      val _ = destroy ()
   in
      b
   end

fun dfs (g, z) = dfsNodes (g, nodes g, z)

fun dfsForest (g, {roots: Node.t vector, nodeValue: Node.t -> 'a}) : 'a Tree.t vector =
   (Vector.fromList o dfsNodes)
   (g, Vector.toList roots,
    ([], fn (_, trees) =>
     let
        fun startNode (n, ()) =
           let
              fun nonTree (_, ts) = ts
              fun tree (_, ts) = ((), fn t => t :: ts)
              fun finish ts = Tree.T (nodeValue n, Vector.fromList ts)
           in
              ([], nonTree, tree, finish)
           end
        fun finishTree t = t :: trees
     in
        ((), startNode, finishTree)
     end))

fun dfsTree (g, {root, nodeValue}) =
   let
      val ts = dfsForest (g, {roots = Vector.new1 root, nodeValue = nodeValue})
   in
      if Vector.length ts = 1
         then Vector.sub (ts, 0)
      else Error.bug "DirectedGraph.dfsTree"
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

fun foreachNode (g, f) = List.foreach (nodes g, f)

fun foreachEdge (g, edge) =
   foreachNode (g, fn n as Node.Node {successors, ...} =>
                List.foreach (!successors, fn e => edge (n, e)))

(*--------------------------------------------------------*)
(*                    Dominators                          *)
(*--------------------------------------------------------*)

fun validDominators (graph,
                     {root: Node.t,
                      idom: Node.t -> Node.t}): bool =
   (* Check for each edge v --> w that idom w dominates v.
    * FIXME: It should first check that idom describes a tree rooted at root.
    *)
   Exn.withEscape
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

val _ = validDominators

datatype 'a idomRes =
   Idom of Node.t
 | Root
 | Unreachable

(* This is an implementation of the simple and fast dominance algorithm
 * described in
 *
 *   A Simple, Fast Dominance Algorithm.
 *   Keith Cooper and Timothy Harvey and Ken Kennedy.
 *   Software Practice and Experience, 2001.
 *   http://citeseer.ist.psu.edu/cooper01simple.html
 *   http://www.cs.rice.edu/~keith/EMBED/dom.pdf
 *
 * This implementation replaced the previous implementation based on the
 * Lengauer/Tarjan algorithm, as described on p. 185-191 of Muchnick's
 * "Advanced Compiler Design and Implementation", and appears to run in
 * less than half the time on a self compile and took about half the
 * amount of code to implement.
 *)
fun dominators (graph, {root}) =
   let
      val unknown = ~2
      val visiting = ~1

      val {get = getNum, set = setNum, rem = remNum, ...} =
         Property.getSet (Node.plist, Property.initConst unknown)

      val nodes = Array.array (numNodes graph, root)
      fun node i = Array.sub (nodes, i)

      fun dfs (n, v) =
         (setNum (v, visiting)
          ; case List.fold
                 (Node.successors v, n, fn (Edge.Edge {to, ...}, n) =>
                  if getNum to = unknown then dfs (n, to) else n) of
               n => (setNum (v, n) ; Array.update (nodes, n, v) ; n+1))
      val numNodes = dfs (0, root)

      val preds = Array.array (numNodes, [])
      fun addPred (t, f) = Array.update (preds, t, f :: Array.sub (preds, t))

      val () = Int.for (0, numNodes, fn i =>
               List.foreach (Node.successors (node i),
                             fn Edge.Edge {to, ...} => addPred (getNum to, i)))

      val () = Array.foreach (nodes, remNum)

      val idoms = Array.array (numNodes, unknown)
      fun idom i = Array.sub (idoms, i)
      fun setIdom (i, d) = Array.update (idoms, i, d)

      val rootNum = numNodes-1
      val () = setIdom (rootNum, rootNum)

      fun intersect (n1, n2) =
         if n1 = n2
            then n1
         else
            let
               fun up (f, t) = if f < t then up (idom f, t) else f
               val n1 = up (n1, n2)
               val n2 = up (n2, n1)
            in
               intersect (n1, n2)
            end

      fun iterate () =
         if Int.foldDown (0, rootNum, false, fn (i, changed) => let
               val new =
                   case Array.sub (preds, i) of
                      [] => raise Fail "bug"
                    | p::ps =>
                      List.fold (ps, p, fn (j, new) =>
                      if idom j <> unknown then intersect (new, j) else new)
            in
               if idom i <> new then (setIdom (i, new) ; true) else changed
            end)
            then iterate ()
         else ()
      val () = iterate ()

      val {get = idomFinal, set = setIdom, ...} =
         Property.getSetOnce (Node.plist, Property.initConst Unreachable)
      val () = setIdom (root, Root)
      val () = Int.for (0, rootNum, fn i =>
               setIdom (node i, Idom (node (idom i))))
   in
      {idom = idomFinal}
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

fun ignoreNodes (g: t, shouldIgnore: Node.t -> bool)
   : t * {destroy: unit -> unit,
          newNode: Node.t -> Node.t} =
   let
      val g' = new ()
      val {destroy, get = newNode, ...} =
         Property.destGet (Node.plist,
                           Property.initFun (fn _ => newNode g'))
      (* reach n is the set of non-ignored nodes that n reaches via
       * nonempty paths through ignored nodes.  It is computed by starting
       * at each node and doing a DFS that only goes through ignored nodes.
       *)
      val {get = reach: Node.t -> Node.t list, ...} =
         Property.get
         (Node.plist,
          Property.initFun
          (fn root =>
           let
              val r = ref []
              val {destroy, get = seen, ...} =
                 Property.destGet (Node.plist,
                                   Property.initFun (fn _ => ref false))
              fun loop n =
                 List.foreach (Node.successors n, fn e =>
                               let
                                  val n = Edge.to e
                                  val s = seen n
                               in
                                  if !s
                                     then ()
                                  else
                                     (s := true
                                      ; if shouldIgnore n
                                           then loop n
                                        else List.push (r, n))
                               end)
              val _ = loop root
              val _ = destroy ()
           in
              !r
           end))
      val _ =
         foreachNode
         (g, fn n =>
          if shouldIgnore n
             then ()
          else
             let
                val from = newNode n
             in
                List.foreach
                (reach n, fn to =>
                 addEdge' (g', {from = from, to = newNode to}))
             end)
   in
      (g', {destroy = destroy,
            newNode = newNode})
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

      fun single n = T {loops = Vector.new0 (),
                        notInLoop = Vector.new1 n}

      fun layoutDot (forest: t,
                     {nodeName: Node.t -> string,
                      options: Dot.GraphOption.t list,
                      title: string}) =
         let
            open Dot
            fun label (ns, max) =
               let
                  val pred =
                     case max of
                        NONE => (fn _ => true)
                      | SOME max => (fn (i, _) => i < max)
                  fun loop ns =
                     let
                        val {no = ms, yes = ns} = Vector.partitioni (ns, pred)
                        val ns = String.concatWith (Vector.toListMap (ns, nodeName), ", ")
                     in
                        if Vector.isEmpty ms
                           then [(ns, Center)]
                           else (ns, Center)::(loop ms)
                     end
               in
                  NodeOption.Label (loop ns)
               end
            val c = Counter.new 0
            fun newName () = concat ["n", Int.toString (Counter.next c)]
            val nodes = ref []
            fun loop (T {loops, notInLoop}, root) =
               let
                  val ms =
                     Vector.fold
                     (loops, [], fn ({headers, child}, ac) =>
                      let
                         val n = newName ()
                         val _ =
                            List.push
                            (nodes, {name = n,
                                     options = [label (headers, SOME 5),
                                                NodeOption.Shape Ellipse],
                                     successors =
                                     List.map (loop (child, false), fn n =>
                                               {name = n, options = []})})
                      in
                         n :: ac
                      end)
                  val n = newName ()
                  val (max, successors) =
                     if root
                        then (SOME 10,
                              case ms of
                                 [] => []
                               | m :: _ => [{name = m,
                                             options = [EdgeOption.Style Invisible]}])
                        else (SOME 5, [])
                  val _ = List.push (nodes, {name = n,
                                             options = [label (notInLoop, max),
                                                        NodeOption.Shape Box],
                                             successors = successors})
               in
                  n :: (List.rev ms)
               end
            val ns = loop (forest, true)
            val options =
               case ns of
                  [] => options
                | _ :: ns =>
                     (GraphOption.Rank (Same, List.map (ns, fn n => {nodeName = n})))
                     :: options
         in
            Dot.layout {nodes = !nodes,
                        options = options,
                        title = title}
         end
      val _ = layoutDot
   end

(* Strongly connected components from Aho, Hopcroft, Ullman section 5.5. *)

fun stronglyConnectedComponents (g: t): Node.t list list =
   let
      val {get = nodeInfo: Node.t -> {dfnumber: int,
                                      isOnStack: bool ref,
                                      lowlink: int ref},
           set = setNodeInfo, destroy, ...} =
         Property.destGetSetOnce (Node.plist,
                                  Property.initRaise ("scc info", Node.layout))
      fun startNode (n, (count, stack, components)) =
         let
            val dfnumber = count
            val count = count + 1
            val lowlink = ref dfnumber
            val stack = n :: stack
            val isOnStack = ref true
            val v = {dfnumber = dfnumber,
                     isOnStack = isOnStack,
                     lowlink = lowlink}
            val _ = setNodeInfo (n, v)
            fun nonTreeEdge (e, z) =
               let
                  val w = nodeInfo (Edge.to e)
                  val _ =
                     if #dfnumber w < #dfnumber v
                        andalso !(#isOnStack w)
                        andalso #dfnumber w < !(#lowlink v)
                        then #lowlink v := #dfnumber w
                     else ()
               in
                  z
               end
            fun treeEdge (e, z) =
               (z,
                fn z =>
                let
                   val w = nodeInfo (Edge.to e)
                   val _ =
                      if !(#lowlink w) < !(#lowlink v)
                         then #lowlink v := !(#lowlink w)
                      else ()
                in
                   z
                end)
            fun finishNode (count, stack, components) =
               if !lowlink = dfnumber
                  then
                     let
                        fun popTo (stack, ac) =
                           case stack of
                              [] => Error.bug "DirectedGraph.stronglyConnectedComponents.finishNode.popTo"
                            | n' :: stack =>
                                 let
                                    val _ = #isOnStack (nodeInfo n') := false
                                    val ac = n' :: ac
                                 in
                                    if Node.equals (n, n')
                                       then (stack, ac)
                                    else popTo (stack, ac)
                                 end
                        val (stack, component) = popTo (stack, [])
                     in
                        (count, stack, component :: components)
                     end
               else (count, stack, components)
         in
            ((count, stack, components),
             nonTreeEdge,
             treeEdge,
             finishNode)
         end
      val (_, _, components) =
         dfs (g, ((0, [], []), fn (_, z) => (z, startNode, fn z => z)))
      val _ = destroy ()
   in
      components
   end

val stronglyConnectedComponents =
   if true
      then stronglyConnectedComponents
   else
   let
      val c = Counter.new 0
   in
      fn g =>
      let
         val nodeCounter = Counter.new 0
         val {get = nodeIndex: Node.t -> int, destroy, ...} =
            Property.destGet
            (Node.plist,
             Property.initFun (fn _ => Counter.next nodeCounter))
         val index = Counter.next c
         val _ =
            File.withOut
            (concat ["graph", Int.toString index, ".dot"], fn out =>
             Layout.output
             (layoutDot (g, fn _ =>
                         {edgeOptions = fn _ => [],
                          nodeOptions = fn n => [Dot.NodeOption.label
                                                 (Int.toString (nodeIndex n))],
                          options = [],
                          title = "scc graph"}),
              out))
         val ns = stronglyConnectedComponents g
         val _ =
            File.withOut
            (concat ["scc", Int.toString index], fn out =>
             Layout.outputl
             (List.layout (List.layout (Int.layout o nodeIndex)) ns,
              out))
         val _ = destroy ()
      in
         ns
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
                                    val {class = ref class', 
                                         isHeader = isHeader',
                                         next = next', ...} = nodeInfo to
                                 in
                                    if class = class'
                                       andalso not (!isHeader')
                                       then addEdge' (g', {from = from',
                                                           to = valOf (!next')})
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

fun quotient (g, vs) =
   let
      val numClasses = Vector.length vs
      val {destroy, get = nodeClass: Node.t -> int, set = setNodeClass, ...} =
         Property.destGetSetOnce (Node.plist,
                                  Property.initRaise ("newNode", Node.layout))
      val g' = new ()
      val newNodes =
         Vector.mapi (vs, fn (i, v) =>
                      let
                         val n' = newNode g'
                         val _ =
                            Vector.foreach (v, fn n => setNodeClass (n, i))
                      in
                         n'
                      end)
      val successors = Array.array (numClasses, [])
      val _ =
         foreachNode
         (g, fn n =>
          let
             val class = nodeClass n
          in
             Array.update
             (successors, class,
              List.fold (Node.successors n,
                         Array.sub (successors, class),
                         fn (e, ac) => nodeClass (Edge.to e) :: ac))
          end)
      (* Eliminate duplicates from successor lists and add the graph edges. *)
      val hasIt = Array.array (numClasses, false)
      val _ =
         Array.foreachi
         (successors, fn (i, cs) =>
          let
             val from = Vector.sub (newNodes, i)
             val _ =
                List.foreach
                (cs, fn c =>
                 if Array.sub (hasIt, c)
                    then ()
                 else (Array.update (hasIt, c, true)
                       ; addEdge' (g', {from = from,
                                        to = Vector.sub (newNodes, c)})))
             val _ =
                List.foreach (cs, fn c => Array.update (hasIt, c, false))
          in
             ()
          end)
      fun newNode n = Vector.sub (newNodes, nodeClass n)
   in
      (g', {destroy = destroy,
            newNode = newNode})
   end

fun subgraph (g: t, keep: Node.t -> bool) =
   let
      val sub = new ()
      val {get = newNode, destroy, ...} =
         Property.destGet (Node.plist,
                           Property.initFun (fn _ => newNode sub))
      val _ = foreachNode (g, fn from =>
                           if not (keep from)
                              then ()
                           else
                              List.foreach
                              (Node.successors from,
                               let
                                  val from = newNode from
                               in
                                  fn e =>
                                  let
                                     val to = Edge.to e
                                  in
                                     if keep to
                                        then
                                           addEdge' (sub, {from = from,
                                                           to = newNode to})
                                     else ()
                                  end
                               end))
   in
      (sub, {destroy = destroy,
             newNode = newNode})
   end

fun topologicalSort (g: t): Node.t list option =
   let
      exception Cycle
      val {get = amVisiting, destroy, ...} =
         Property.destGet (Node.plist, Property.initFun (fn _ => ref false))
      fun doit () =
         dfs (g,
              ([], fn (_, ns) =>
               let
                  fun startNode (n, ns) =
                     let
                        fun nonTree (e, ns) =
                           if !(amVisiting (Edge.to e))
                              then raise Cycle
                           else ns
                        fun tree (_, ns) = (ns, fn ns => ns)
                        fun finish ns = n :: ns
                     in
                        (ns, nonTree, tree, finish)
                     end
                  fun finishTree ns = ns
               in
                  (ns, startNode, finishTree)
               end))
      val res = SOME (doit ()) handle Cycle => NONE
      val _ = destroy ()
   in
      res
   end

fun transpose (g: t) =
   let
      val transpose = new ()
      val {get = newNode, destroy, ...} =
         Property.destGet (Node.plist,
                           Property.initFun (fn _ => newNode transpose))
      val _ = foreachNode (g, fn to =>
                           List.foreach
                           (Node.successors to,
                            let
                               val to = newNode to
                            in
                               fn e =>
                               addEdge' (transpose, {from = newNode (Edge.to e),
                                                     to = to})
                            end))
   in
      (transpose, {destroy = destroy,
                   newNode = newNode})
   end

val transpose =
   if true
      then transpose
   else
   let
      val c = Counter.new 0
   in
      fn g =>
      let
         val nodeCounter = Counter.new 0
         val {get = nodeIndex: Node.t -> int, destroy, ...} =
            Property.destGet
            (Node.plist,
             Property.initFun (fn _ => Counter.next nodeCounter))
         val index = Counter.next c
         val _ =
            File.withOut
            (concat ["graph", Int.toString index, ".dot"], fn out =>
             Layout.output
             (layoutDot (g, fn _ =>
                         {edgeOptions = fn _ => [],
                          nodeOptions = fn n => [Dot.NodeOption.label
                                                 (Int.toString (nodeIndex n))],
                          options = [],
                          title = "transpose graph"}),
              out))
         val z as (g, _) = transpose g
         val _ =
            File.withOut
            (concat ["transpose", Int.toString index, ".dot"], fn out =>
             Layout.output
             (layoutDot (g, fn _ =>
                         {edgeOptions = fn _ => [],
                          nodeOptions = fn n => [Dot.NodeOption.label
                                                 (Int.toString (nodeIndex n))],
                          options = [],
                          title = "transpose graph"}),
              out))
         val _ = destroy ()
      in
         z
      end
   end

structure Node =
   struct
      open Node

      type 'a t = t
      type 'a edge = edge
   end

structure Edge =
   struct
      open Edge

      type 'a t = t
   end

type 'a t = t
type 'a u = unit

structure LoopForest =
   struct
      open LoopForest
      type 'a t = t

      fun dest (T r) = r
   end

end
