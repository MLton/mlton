(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * This pass is based on
 * Contification Using Dominators, by Fluet and Weeks.  ICFP 2001.
 *)

functor Contify (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Transfer

structure Cont =
  struct
    type t = {cont: Label.t, handler: Handler.t}

    fun layout {cont, handler}
      = let
          open Layout
        in
          tuple2 (Label.layout, Handler.layout) (cont, handler)
        end
  end

(* Return = {Uncalled, Unknown} U Cont U Func
 *)
structure Areturn =
  struct
    datatype t
      = Uncalled
      | Unknown
      | Cont of Cont.t
      | Func of Func.t

    fun layout r
      = let 
          open Layout
        in
          case r
            of Uncalled => str "Uncalled"
             | Unknown => str "Unknown"
             | Cont c => Cont.layout c
             | Func f => Func.layout f
        end
  end

structure ContData =
  struct
    datatype t = T of {node: unit DirectedGraph.Node.t option ref,
                       rootEdge: bool ref,
                       prefixes: Func.t list ref}

    fun new () = T {node = ref NONE,
                    rootEdge = ref false,
                    prefixes = ref []}

    local
      fun make s = let
                     fun S' (T r) = s r
                     val S = ! o S'
                   in
                     (S', S)
                   end
    in
      val (node', _) = make #node
      val (rootEdge', _) = make #rootEdge
      val (prefixes', prefixes) = make #prefixes
    end
    fun nodeReset (T {node, ...}) = node := NONE
  end

structure FuncData =
  struct
    datatype t = T of {node: unit DirectedGraph.Node.t option ref,
                       reach: bool ref,
                       callers: {nontail: (Func.t * Cont.t) list ref,
                                 tail: Func.t list ref},
                       callees: {nontail: (Func.t * Cont.t) list ref,
                                 tail: Func.t list ref},
                       A: Areturn.t ref,
                       prefixes: Func.t list ref,
                       finished: bool ref,
                       replace: {label: Label.t,
                                 blocks: Block.t list} option ref,
                       contified: Block.t list list ref}

    fun new () = T {node = ref NONE,
                    reach = ref false,
                    callers = {nontail = ref [], tail = ref []},
                    callees = {nontail = ref [], tail = ref []},
                    A = ref Areturn.Uncalled,
                    prefixes = ref [],
                    finished = ref false,
                    replace = ref NONE,
                    contified = ref []}

    local
      fun make s = let
                     fun S' (T r) = s r
                     val S = ! o S'
                   in
                     (S', S)
                   end
      fun make' s = let
                      fun S' (T r) = s r
                    in
                      S'
                    end
    in
      val (node', _) = make #node
      val (reach', reach) = make #reach
      val callers' = make' #callers
      val callees' = make' #callees
      val (_, A) = make #A
      val (prefixes', prefixes) = make #prefixes
      val (finished', _) = make #finished
      val (_, replace) = make #replace
      val (contified', contified) = make #contified
    end
    fun nodeReset (T {node, ...}) = node := NONE
  end

structure ContFuncGraph =
  struct
    structure Graph = DirectedGraph
    structure Node = Graph.Node

    datatype t = ContNode of Cont.t
               | FuncNode of Func.t
    fun newContFuncGraph {getContData: Cont.t -> ContData.t,
                          getFuncData: Func.t -> FuncData.t}
      = let
          val G = Graph.new ()
          fun addEdge edge
            = ignore (Graph.addEdge (G, edge))
          val {get = getNodeInfo : unit Node.t -> t, 
               set = setNodeInfo, ...}
            = Property.getSetOnce
              (Node.plist,
               Property.initRaise ("nodeInfo", Node.layout))
          fun getFuncNode f
            = let
                val node = FuncData.node' (getFuncData f)
              in
                case !node
                  of SOME n => n
                   | NONE => let
                               val n = Graph.newNode G
                             in
                               setNodeInfo (n, FuncNode f);
                               node := SOME n;
                               n
                             end
              end

          fun getContNode c
            = let
                val node = ContData.node' (getContData c)
              in
                case !node
                  of SOME n => n
                   | NONE => let
                               val n = Graph.newNode G
                             in
                               setNodeInfo (n, ContNode c);
                               node := SOME n;
                               n
                             end
              end

          fun reset p
            = Graph.foreachNode
              (G,
               fn n => if p n
                         then case getNodeInfo n
                                of ContNode c
                                 => ContData.nodeReset (getContData c)
                                 | FuncNode f 
                                 => FuncData.nodeReset (getFuncData f)
                         else ())
        in
          {G = G, 
           addEdge = addEdge, 
           getNodeInfo = getNodeInfo, 
           getContNode = getContNode, 
           getFuncNode = getFuncNode,
           reset = reset}
        end
    fun newFuncGraph {getFuncData: Func.t -> FuncData.t}
      = let
          val {G, addEdge, getNodeInfo, getFuncNode, reset, ...}
            = newContFuncGraph {getContData = fn _ => Error.bug "Contify.ContFuncGraph.newFuncGraph",
                                getFuncData = getFuncData}
        in
          {G = G,
           addEdge = addEdge,
           getNodeInfo = fn n => case getNodeInfo n
                                   of FuncNode f => f
                                    | ContNode _ => Error.bug "Contify.ContFuncGraph.newFuncGraph",
           getFuncNode = getFuncNode,
           reset = reset}
        end
  end

structure InitReachCallersCallees =
  struct
    structure Graph = DirectedGraph
    structure DfsParam = Graph.DfsParam

    (* Define Reach: Func -> Bool as follows:
     *  Reach (f) iff there is a path of calls from fm to f.
     *
     * Define NontailCallers: Func -> P (Func x Cont) as follows:
     *  NontailCallers (f) = {(g, c) | (g, f, c) in N}
     * Define TailCallers: Func -> P (Func) as follows:
     *  Callers (f) = {g | (g, f) in T}
     * Define NontailCallees: Func -> P (Func x Cont) as follows:
     *  NontailCallers (f) = {(g, c) | (f, g, c) in N}
     * Define TailCallees: Func -> P (Func) as follows:
     *  Callers (f) = {g | (f, g) in T}
     *
     * Precondition: forall f in Func. (FuncData.node o getFuncData) f = NONE
     *               forall f in Func. (FuncData.callers o getFuncData) f 
     *                                 = {nontail = [], tail = []}
     *               forall f in Func. (FuncData.callees o getFuncData) f
     *                                 = {nontail = [], tail = []}
     * Postcondition: FuncData.reach o getFuncData = Reach
     *                #nontail (FuncData.callers o getFuncData)
     *                = NontailCallers
     *                #tail (FuncData.callers o getFuncData)
     *                = TailCallers
     *                #nontail (FuncData.callees o getFuncData)
     *                = NontailCallees
     *                #tail (FuncData.callees o getFuncData)
     *                = TailCallees
     *)
    fun initReachCallersCallees 
        {program = Program.T {functions, main = fm, ...},
         getFuncData: Func.t -> FuncData.t} : unit
      = let
          val {G, addEdge, getNodeInfo, getFuncNode, reset, ...}
            = ContFuncGraph.newFuncGraph {getFuncData = getFuncData}

          val _ 
            = List.foreach
              (functions,
               fn func
                => let
                     val {name = f, blocks, ...} = Function.dest func
                     val callees = FuncData.callees' (getFuncData f)
                     val f_node = getFuncNode f
                   in
                     Vector.foreach
                     (blocks,
                      fn Block.T {transfer = Call {func = g, return, ...}, ...}
                       => let
                            val callers = FuncData.callers' (getFuncData g)
                            val g_node = getFuncNode g
                            val _ =
                               case return of
                                  Return.NonTail c =>
                                     (List.push (#nontail callees, (g, c));
                                      List.push (#nontail callers, (f, c)))
                                | _ => (List.push (#tail callees, g);
                                        List.push (#tail callers, f))
                          in
                            addEdge {from = f_node,
                                     to = g_node}
                          end
                       | _ => ())
                   end)

          val dfs_param
            = DfsParam.finishNode
              (fn n => FuncData.reach' (getFuncData (getNodeInfo n)) := true)
          val fm_node = getFuncNode fm
        in
          Graph.dfsNodes (G, [fm_node], dfs_param);
          reset (fn _ => true)
        end
    val initReachCallersCallees
       = Control.trace (Control.Detail, "initReachCallerCallees") 
                       initReachCallersCallees 
  end

structure AnalyzeDom =
  struct
    structure Graph = DirectedGraph
    structure Node = Graph.Node

    (* Now define a directed graph G = (Node, Edge) where
     *      Node = Cont U Fun U {Root}
     *      Edge = {(Root, fm)}
     *             U {(Root, c) | c in Cont}
     *             U {(Root, f) | not (Reach (f))}
     *             U {(f, g) | (f, g) in T and Reach (f)}
     *             U {(c, g) | (f, g, c) in N and Reach (f)}
     *
     * Let D be the dominator tree of G rooted at Root.
     * For f in Fun, let idom (f) be the parent of f in D.
     *
     * Define an analysis, A_Dom, based on D as follows:
     *      A_Dom (f) = 
     *           if idom (f) = Root
     *             then if Reach (f) then Unknown else Uncalled
     *             else the ancestor g of f in D such that idom (g) = Root
     *
     * Precondition: forall c in Cont. (ContData.node o getContData) c = NONE
     *               forall c in Cont. (ContData.rootEdge o getContData) c = false
     *               forall f in Func. (FuncData.node o getFuncData) f = NONE
     *               forall f in Func. (FuncData.reach o getFuncData) f = Reach
     * Postcondition: FuncData.ADom o getFuncData = A_Dom
     *                forall c in Cont. (ContData.node o getContData) c = NONE
     *                forall f in Func. (FuncData.node o getFuncData) f = NONE
     *)
    fun analyzeDom {program as Program.T {functions, main = fm, ...},
                    getContData: Cont.t -> ContData.t,
                    getFuncData: Func.t -> FuncData.t} : unit
      = let
          datatype z = datatype Areturn.t

          val {G, addEdge, getNodeInfo, getContNode, getFuncNode, reset, ...}
            = ContFuncGraph.newContFuncGraph {getContData = getContData,
                                              getFuncData = getFuncData}
          val Root = DirectedGraph.newNode G

          fun buildGraph () = let
          val fm_node = getFuncNode fm
          (* {(Root, fm)} *)
          val _ = addEdge {from = Root, to = fm_node}
          (* { (Root, f) | fm calls f } *)
          val () =
             if !Control.contifyIntoMain
                then ()
             else
                let
                   val {blocks, ...} =
                      Function.dest (Program.mainFunction program)
                in
                   Vector.foreach
                   (blocks, fn Block.T {transfer, ...} =>
                    case transfer of
                       Call {func, ...} =>
                          addEdge {from = Root, to = getFuncNode func}
                     | _ => ())
                end
          val _
            = List.foreach
              (functions,
               fn func
                => let
                     val {name = f, blocks, ...} = Function.dest func
                     val f_reach = FuncData.reach (getFuncData f)
                     val f_node = getFuncNode f
                   in
                     if f_reach
                       then Vector.foreach
                            (blocks,
                             fn Block.T {transfer = Call {func = g, return, ...}, ...}
                              => if FuncData.reach (getFuncData g)
                                   then let
                                          val g_node = getFuncNode g
                                        in
                                          case return of
                                             Return.Dead =>
                                                (* When compiling with profiling,
                                                 * Dead returns are allowed to
                                                 * have nonempty source stacks
                                                 * (see type-check.fun).  So, we
                                                 * can't contify functions that
                                                 * are called with a Dead cont.
                                                 *)
                                                addEdge {from = Root,
                                                         to = g_node}
                                           | Return.NonTail c =>
                                                let
                                                   val c_node = getContNode c
                                                   val rootEdge 
                                                      = ContData.rootEdge'
                                                      (getContData c)
                                                in
                                                   if !rootEdge
                                                      then ()
                                                   else ((* {(Root, c) | c in Cont} *)
                                                         addEdge {from = Root,
                                                                  to = c_node};
                                                         rootEdge := true);
                                                      (* {(c, g) | (f, g, c) in N
                                                       *       and Reach (f)} *)
                                                      addEdge {from = c_node,
                                                               to = g_node}
                                                end
                                           | _ =>
                                                (* {(f, g) | (f, g) in T 
                                                 *       and Reach (f)} *)
                                                addEdge {from = f_node,
                                                         to = g_node}
                                        end
                                   else ()
                              | _ => ())
                       else (* {(Root, f) | not (Reach (f))} *)
                            addEdge {from = Root,
                                     to = f_node}
                   end)
          in () end
          val buildGraph 
            = Control.trace (Control.Detail, "buildGraph") buildGraph
          val _ = buildGraph ()

          fun computeDominators () = let
          val {idom} = Graph.dominators (G, {root = Root})
          in idom end
          val computeDominators 
            = Control.trace (Control.Detail, "computeDominators") computeDominators
          val idom = computeDominators ()

          fun computeADom () = let
          fun ancestor node =
             case idom node of
                Graph.Idom parent =>
                   if Node.equals (parent, Root)
                      then node
                   else ancestor parent
              | Graph.Root => node
              | Graph.Unreachable => Error.bug "Contify.AnalyzeDom.ancestor: unreachable"

          val _
            = List.foreach
              (functions,
               fn func
                => let
                     val {name = f, ...} = Function.dest func
                     val FuncData.T {A, reach, node, ...} = getFuncData f
                     val f_ADom = A
                     val f_reach = !reach
                     val f_node = valOf (!node)
                     datatype z = datatype ContFuncGraph.t
                   in
                     if (case idom f_node of
                            Graph.Idom n => Node.equals (n, Root)
                          | Graph.Root => true
                          | Graph.Unreachable => Error.bug "Contify.AnalyzeDom.idom: unreachable")
                       then if f_reach
                              then f_ADom := Unknown
                              else f_ADom := Uncalled
                       else let
                              (* Use this for the ancestor version *)
                              val l_node = ancestor f_node
                              (* Use this for the parent version *)
                              (* val l_node = idom f_node *)
                            in
                              case getNodeInfo l_node
                                of FuncNode g => f_ADom := Func g
                                 | ContNode c => f_ADom := Cont c
                            end
                   end)
          in () end
          val computeADom 
            = Control.trace (Control.Detail, "compute ADom") computeADom
          val _ = computeADom ()

          val _ = reset (fn n => not (Node.equals (n, Root)))
        in
          ()
        end
    val analyzeDom 
      = Control.trace (Control.Detail, "analyzeDom") analyzeDom
end

structure Transform =
  struct
    (*
     * Precondition: forall c in Cont. (ContData.node o getContData) c = NONE
     *               forall c in Cont. (ContData.prefixes o getContData) c = []
     *               forall f in Func. (FuncData.node o getFuncData) f = NONE
     *               FuncData.A o getFuncData = A
     *                where A is a safe analysis
     *               FuncData.callers o getFuncData
     *               = {nontail = NontailCallers, tail = TailCallers}
     *               FuncData.callees o getFuncData
     *               = {nontail = NontailCallees, tail = TailCallees}
     *               forall f in Func. (FuncData.prefixes o getFuncData) f = []
     *               forall f in Func. (FuncData.finished o getFuncData) f = false
     *               forall f in Func. (FuncData.replace o getFuncData) f = NONE
     * Postcondition: forall c in Cont. (ContData.node o getContData) c = NONE
     *                forall f in Func. (FuncData.node o getFuncData) f = NONE
     *)
    fun transform {program = Program.T {datatypes, globals, functions, main},
                   getFuncData: Func.t -> FuncData.t,
                   getContData: Cont.t -> ContData.t} : Program.t
      = let
          datatype z = datatype Areturn.t

          (* For functions turned into continuations,
           *  record their args, blocks, and new name.
           *)
          val _ 
            = List.foreach
              (functions,
               fn func
                => let
                     val {name = f, 
                          args = f_args, 
                          blocks = f_blocks, 
                          start = f_start,
                          ...} = Function.dest func
                     val FuncData.T {A, replace, ...} = getFuncData f

                     val _ = Control.diagnostics
                             (fn display
                               => let open Layout
                                  in display (seq [str "A(", 
                                                   Func.layout f,
                                                   str ") = ",
                                                   Areturn.layout (!A)])
                                  end)


                     fun contify prefixes
                       = let
                           val f_label = Label.newString (Func.originalName f)
                           val _ = Control.diagnostics
                                   (fn display 
                                     => let open Layout
                                        in display (seq [Func.layout f,
                                                         str " -> ",
                                                         Label.layout f_label])
                                        end)
                           val f_blocks 
                             = (Block.T {label = f_label,
                                         args = f_args,
                                         statements = Vector.new0 (),
                                         transfer = Goto {dst = f_start,
                                                          args = Vector.new0 ()}})::
                               (Vector.toList f_blocks)
                         in 
                           replace := SOME {label = f_label,
                                            blocks = f_blocks} ;
                           List.push(prefixes, f)
                         end
                   in
                     case !A
                       of Uncalled => ()
                        | Unknown => ()
                        | Cont c => contify (ContData.prefixes' (getContData c))
                        | Func g => contify (FuncData.prefixes' (getFuncData g))
                   end)

          val traceAddFuncs =
             Trace.trace3 ("Contify.Transform.addFuncs",
                           Func.layout,
                           List.layout Func.layout,
                           Return.layout,
                           Unit.layout)
          val traceTransBlock =
             Trace.trace3 ("Contify.Transform.transBlock",
                           Func.layout,
                           Label.layout o Block.label,
                           Return.layout,
                           Layout.ignore)
          (* Walk over all functions, removing those that aren't top level,
           *  and descening those that are, inserting local functions
           *  where necessary.
           * - turn tail calls into nontail calls
           * - turn returns into gotos
           * - turn raises into gotos
           *)
          fun addFuncPrefixes (f: Func.t,
                               g: Func.t,
                               c: Return.t) : unit
            = let
                val prefixes = FuncData.prefixes (getFuncData g)
                val _ = Control.diagnostics
                        (fn display
                          => let open Layout
                             in display (seq [str "addFuncPrefixes: ",
                                              Func.layout f,
                                              str " ",
                                              Func.layout g,
                                              str " ",
                                              List.layout Func.layout prefixes])
                             end)
              in 
                addFuncs (f, prefixes, c)
              end
          and addContPrefixes (f: Func.t,
                               r: Cont.t,
                               c: Return.t): unit
            = let
                val prefixes = ContData.prefixes (getContData r)
                val _ = Control.diagnostics
                        (fn display
                          => let open Layout
                             in display (seq [str "addContPrefixes: ",
                                              Func.layout f,
                                              str " ",
                                              Cont.layout r,
                                              str " ",
                                              List.layout Func.layout prefixes])
                             end)

              in 
                addFuncs (f, prefixes, Return.compose (c, Return.NonTail r))
              end
          and addFuncs arg : unit =
             traceAddFuncs
             (fn (f: Func.t,
                  gs: Func.t list,
                  c: Return.t) =>
             List.foreach
              (gs,
               fn g => let
                         val finished = FuncData.finished' (getFuncData g)
                       in
                         if !finished
                           then ()
                           else (addFuncPrefixes(f, g, c);
                                 addBlocks
                                 (f,
                                  #blocks (valOf (FuncData.replace (getFuncData g))),
                                  c);
                                 finished := true)
                       end)
              ) arg
          and addBlocks (f: Func.t,
                         blocks: Block.t list,
                         c: Return.t) : unit
            = let
                val contified' = List.map(blocks, 
                                          fn block => transBlock (f, block, c))
                val contified = FuncData.contified' (getFuncData f)
              in
                List.push(contified, contified')
              end
          and transBlock arg: Block.t =
             traceTransBlock
             (fn (f: Func.t, 
                  Block.T {label, args, statements, transfer},
                  c: Return.t) =>
             let
                val transfer
                  = case transfer
                      of Call {func, args, return}
                       => ((case return of
                               Return.NonTail r => addContPrefixes (f, r, c)
                             | _ => ());
                           case FuncData.replace (getFuncData func) of
                              NONE => Call {func = func,
                                            args = args,
                                            return = Return.compose (c, return)}
                            | SOME {label, ...} =>
                                 Goto {dst = label, args = args})
                       | Return xs
                       => (case c
                             of Return.NonTail {cont, ...}
                              => Goto {dst = cont, args = xs}
                              | _ => transfer)
                       | Raise xs
                       => (case c
                             of Return.NonTail {handler = Handler.Handle handler, ...} 
                              => Goto {dst = handler, args = xs}
                              | _ => transfer)
                       | _ => transfer
              in
                Block.T {label = label,
                         args = args,
                         statements = statements,
                         transfer = transfer}
              end) arg

          val shrink = shrinkFunction {globals = globals}

          val functions
            = List.fold
              (functions, [], fn (func, ac) =>
               let
                  val {args = f_args, 
                       blocks = f_blocks,
                       mayInline = f_mayInline,
                       name = f, 
                       raises = f_raises,
                       returns = f_returns,
                       start = f_start} = Function.dest func
               in
                  case FuncData.A (getFuncData f)
                     of Unknown
                        => let
                              val _ = addFuncPrefixes (f, f, Return.Tail)
                              val f_blocks =
                                 Vector.toListMap
                                 (f_blocks, fn block =>
                                  transBlock (f, block, Return.Tail))
                              val f_blocks
                                 = f_blocks::
                                 (FuncData.contified (getFuncData f))
                              val f_blocks
                                 = Vector.fromList (List.concat f_blocks)
                           in 
                              shrink (Function.new {args = f_args,
                                                    blocks = f_blocks,
                                                    mayInline = f_mayInline,
                                                    name = f,
                                                    raises = f_raises,
                                                    returns = f_returns,
                                                    start = f_start})
                              :: ac
                           end
                      | _ => ac
               end)

          val program 
            = Program.T {datatypes = datatypes,
                         globals = globals,
                         functions = functions,
                         main = main}
        in
          program
        end
    val transform 
      = Control.trace (Control.Detail, "transform") transform
  end

fun transform (program as Program.T _)
  = let
      val {get = getLabelInfo : Label.t -> (Handler.t * ContData.t) list ref,
           ...}
        = Property.get 
          (Label.plist, Property.initFun (fn _ => ref []))
      val getContData : Cont.t -> ContData.t
        = fn {cont, handler}
           => let 
                val l = getLabelInfo cont
              in 
                case List.peek (!l, fn (handler', _) =>
                                Handler.equals (handler, handler'))
                  of SOME (_, cd) => cd
                   | NONE => let
                               val cd = ContData.new ()
                               val _ = List.push(l, (handler, cd))
                             in
                               cd
                             end
              end
      val {get = getFuncData : Func.t -> FuncData.t, ...}
        = Property.get (Func.plist,
                        Property.initFun
                        (fn _ => FuncData.new ()))

      val _ = InitReachCallersCallees.initReachCallersCallees 
              {program = program,
               getFuncData = getFuncData}
      val _ = AnalyzeDom.analyzeDom 
              {program = program,
               getContData = getContData,
               getFuncData = getFuncData}
      val program = Transform.transform 
                    {program = program,
                     getContData = getContData,
                     getFuncData = getFuncData}
      val _ = Program.clearTop program
    in
      program
    end
end
