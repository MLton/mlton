(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
Defn:
A label is exactly-multi-threaded if it may be executed by two
different threads during some run of the program.  The initial call to
main counts as one thread.

Defn:
A label is actually-multi-used if it may be executed more than once
during the execution of the program.
*)

functor Multi (S: MULTI_STRUCTS): MULTI =
struct

open S
open Exp Transfer

structure Graph = DirectedGraph
local open Graph
in
  structure Node = Node
end

structure Calls =
  struct
    datatype t = T of value ref
    and value = Zero | One | Many
    fun new (): t = T (ref Zero)
    fun inc (T r)
      = case !r 
          of Zero => r := One
           | _ => r := Many
    val isMany
      = fn (T (ref Many)) => true
         | _ => false
  end

structure ThreadCopyCurrent =
  struct
    structure L = TwoPointLattice (val bottom = "false"
                                   val top = "true")
    open L
    val force = makeTop
    val does = isTop
    val when = addHandler
  end

structure MultiThreaded =
  struct
    structure L = TwoPointLattice (val bottom = "false"
                                   val top = "true")
    open L
    val force = makeTop
    val is = isTop
    val when = addHandler
  end

structure MultiUsed =
  struct
    structure L = TwoPointLattice (val bottom = "false"
                                   val top = "true")
    open L
    val force = makeTop
    val is = isTop
    val when = addHandler
  end


structure FuncInfo =
  struct
    datatype t = T of {calls: Calls.t,
                       threadCopyCurrent: ThreadCopyCurrent.t,
                       multiThreaded: MultiThreaded.t,
                       multiUsed: MultiUsed.t}

    local
       fun make f (T r) = f r
    in
      val calls = make #calls
      val threadCopyCurrent = make #threadCopyCurrent
      val multiThreaded = make #multiThreaded
      val multiUsed = make #multiUsed
    end

    fun new (): t = T {calls = Calls.new (),
                       threadCopyCurrent = ThreadCopyCurrent.new (),
                       multiUsed = MultiUsed.new (),
                       multiThreaded = MultiThreaded.new ()}
  end

structure LabelInfo =
  struct
    datatype t = T of {threadCopyCurrent: ThreadCopyCurrent.t,
                       multiThreaded: MultiThreaded.t,
                       multiUsed: MultiUsed.t}

    local
       fun make f (T r) = f r
     in
       val threadCopyCurrent = make #threadCopyCurrent
       val multiThreaded = make #multiThreaded
       val multiUsed = make #multiUsed
     end

     fun new (): t = T {threadCopyCurrent = ThreadCopyCurrent.new (),
                        multiThreaded = MultiThreaded.new (),
                        multiUsed = MultiUsed.new ()}
   end

structure VarInfo =
  struct
    datatype t = T of {multiThreaded: MultiThreaded.t,
                       multiUsed: MultiUsed.t}

    local
       fun make f (T r) = f r
     in
       val multiThreaded = make #multiThreaded
       val multiUsed = make #multiUsed
     end

     fun new (): t = T {multiThreaded = MultiThreaded.new (),
                        multiUsed = MultiUsed.new ()}
   end

fun multi (p as Program.T {functions, main, ...})
  = let
      val usesThreadsOrConts 
        = Program.hasPrim (p, fn p =>
                           case Prim.name p of
                              Prim.Name.Thread_switchTo => true
                            | _ => false)

      (* funcNode *)
      val {get = funcNode: Func.t -> unit Node.t,
           set = setFuncNode, 
           rem = remFuncNode, ...}
        = Property.getSetOnce
          (Func.plist, Property.initRaise ("Multi.funcNode", Func.layout))

      (* nodeFunction *)
      val {get = nodeFunction: unit Node.t -> Function.t,
           set = setNodeFunction, ...}
        = Property.getSetOnce 
          (Node.plist, Property.initRaise ("Multi.nodeFunc", Node.layout))

      (* funcInfo *)
      val {get = funcInfo: Func.t -> FuncInfo.t, ...}
        = Property.get
          (Func.plist, Property.initFun (fn _ => FuncInfo.new ()))

      (* labelInfo *)
      val {get = labelInfo: Label.t -> LabelInfo.t, ...}
        = Property.get
          (Label.plist, Property.initFun (fn _ => LabelInfo.new ()))

      (* varInfo *)
      val {get = varInfo: Var.t -> VarInfo.t, ...}
        = Property.get
          (Var.plist, Property.initFun (fn _ => VarInfo.new ()))

      (* construct call graph
       * compute calls
       * compute threadCopyCurrent
       *)
      val G = Graph.new ()
      fun newNode () = Graph.newNode G
      fun addEdge edge = ignore (Graph.addEdge (G, edge))
      val _ = List.foreach
              (functions, fn f =>
               let
                 val n = newNode ()
               in 
                 setFuncNode (Function.name f, n) ;
                 setNodeFunction (n, f)
               end)
      val _ = Calls.inc (FuncInfo.calls (funcInfo main))
      val _ = List.foreach
              (functions, fn f =>
               let
                 val {name = f, blocks, ...} = Function.dest f
                 val fi = funcInfo f
               in
                 Vector.foreach
                 (blocks, fn Block.T {label, transfer, ...} =>
                  let
                    val li = labelInfo label
                  in
                    case transfer
                      of Call {func = g, ...}
                       => let
                            val gi = funcInfo g
                          in 
                            Calls.inc (FuncInfo.calls gi) ;
                            addEdge {from = funcNode f,
                                     to = funcNode g} ;
                            if usesThreadsOrConts
                              then ThreadCopyCurrent.when
                                   (FuncInfo.threadCopyCurrent gi,
                                    fn () =>
                                    (ThreadCopyCurrent.force
                                     (LabelInfo.threadCopyCurrent li) ;
                                     ThreadCopyCurrent.force
                                     (FuncInfo.threadCopyCurrent fi)))
                              else ()
                          end
                      | Runtime {prim, ...}
                      => if usesThreadsOrConts
                            andalso
                            (case Prim.name prim of
                                Prim.Name.Thread_copyCurrent => true
                              | _ => false)
                           then (ThreadCopyCurrent.force
                                 (LabelInfo.threadCopyCurrent li) ;
                                 ThreadCopyCurrent.force
                                 (FuncInfo.threadCopyCurrent fi))
                           else ()
                      | _ => ()
                  end)
               end)
      val () = Graph.removeDuplicateEdges G
      val rec forceMultiThreadedVar
        = fn x =>
          let
            val vi = varInfo x
          in
            MultiThreaded.force (VarInfo.multiThreaded vi) ;
            MultiUsed.force (VarInfo.multiUsed vi)
          end
      val rec forceMultiUsedVar
        = fn x =>
          let
            val vi = varInfo x
          in
            MultiUsed.force (VarInfo.multiUsed vi)
          end
      val rec forceMultiThreadedFunc
        = fn f =>
          let
            val fi = funcInfo f
          in
            MultiThreaded.force (FuncInfo.multiThreaded fi) ;
            MultiUsed.force (FuncInfo.multiUsed fi)
          end
      val rec forceMultiUsedFunc
        = fn f =>
          let
            val fi = funcInfo f
          in
            MultiUsed.force (FuncInfo.multiUsed fi)
          end
      val rec forceMultiThreadedBlock
        = fn Block.T {label, args, statements, transfer} =>
          let
            val li = labelInfo label
          in
            if MultiThreaded.is (LabelInfo.multiThreaded li)
              then ()
              else (MultiThreaded.force (LabelInfo.multiThreaded li) ;
                    MultiUsed.force (LabelInfo.multiUsed li) ;
                    Vector.foreach (args, forceMultiThreadedVar o #1) ;
                    Vector.foreach 
                    (statements, fn Statement.T {var, ...} =>
                     Option.app (var, forceMultiThreadedVar)) ;
                    Transfer.foreachFunc
                    (transfer, forceMultiThreadedFunc))
          end
      val rec forceMultiThreadedBlockDFS
        = fn controlFlow as {graph = _, labelNode, nodeBlock} =>
          fn block as Block.T {label, transfer, ...} =>
          let
            val li = labelInfo label
          in
            if MultiThreaded.is (LabelInfo.multiThreaded li)
              then ()
              else (forceMultiThreadedBlock block ;
                    Transfer.foreachLabel
                    (transfer, fn l =>
                     forceMultiThreadedBlockDFS controlFlow 
                     (nodeBlock (labelNode l))))
          end
      val rec forceMultiUsedBlock
        = fn Block.T {label, args, statements, transfer} =>
          let
            val li = labelInfo label
          in
            if MultiUsed.is (LabelInfo.multiUsed li)
              then ()
              else (MultiUsed.force (LabelInfo.multiUsed li) ;
                    Vector.foreach (args, forceMultiUsedVar o #1) ;
                    Vector.foreach 
                    (statements, fn Statement.T {var, ...} =>
                     Option.app (var, forceMultiUsedVar)) ;
                    Transfer.foreachFunc
                    (transfer, forceMultiUsedFunc))
          end
      val rec visitBlock
        = fn controlFlow as {graph = _, labelNode, nodeBlock} =>
          fn Block.T {label, transfer, ...} =>
          if ThreadCopyCurrent.does (LabelInfo.threadCopyCurrent (labelInfo label))
            then Transfer.foreachLabel
                 (transfer, fn l =>
                  forceMultiThreadedBlockDFS controlFlow 
                  (nodeBlock (labelNode l)))
            else ()
      val rec visitForceMultiUsedBlock
        = fn controlFlow =>
          fn block =>
          (forceMultiUsedBlock block ;
           visitBlock controlFlow block)

      val rec forceMultiThreadedFunc
        = fn f =>
          let
            val {args, blocks, ...} = Function.dest f
          in
            Vector.foreach
            (args, forceMultiThreadedVar o #1) ;
            Vector.foreach
            (blocks, forceMultiThreadedBlock)
          end
      val rec forceMultiUsedFunc
        = fn f =>
          let
            val {args, blocks, ...} = Function.dest f
          in
            Vector.foreach
            (args, forceMultiUsedVar o #1) ;
            Vector.foreach
            (blocks, forceMultiUsedBlock)
          end

      fun visitFunc multiUsed f
        = let
            val _ = remFuncNode (Function.name f)

            val fi = funcInfo (Function.name f)
            val _ = if multiUsed
                       orelse
                       Calls.isMany (FuncInfo.calls fi)
                      then MultiUsed.force (FuncInfo.multiUsed fi)
                      else ()
          in
            if MultiThreaded.is (FuncInfo.multiThreaded fi)
              then forceMultiThreadedFunc f
            else if MultiUsed.is (FuncInfo.multiUsed fi)
              then (forceMultiUsedFunc f ;
                    if usesThreadsOrConts
                      then let
                             val _ = MultiThreaded.when
                                     (FuncInfo.multiThreaded fi,
                                      fn () => forceMultiThreadedFunc f)
                             val controlFlow = Function.controlFlow f
                           in
                             Vector.foreach
                             (Function.blocks f, visitBlock controlFlow)
                           end
                      else ())
              else if usesThreadsOrConts
                     then let   
                            val _ = MultiThreaded.when
                                    (FuncInfo.multiThreaded fi,
                                     fn () => forceMultiThreadedFunc f)
                            val _ = MultiUsed.when
                                    (FuncInfo.multiUsed fi,
                                     fn () => forceMultiUsedFunc f)
                            val controlFlow as {graph, nodeBlock, ...}
                              = Function.controlFlow f
                          in
                            List.foreach
                            (Graph.stronglyConnectedComponents graph,
                             fn [] => ()
                              | [n] => if Node.hasEdge {from = n, to = n}
                                         then visitForceMultiUsedBlock controlFlow
                                              (nodeBlock n)
                                         else visitBlock controlFlow
                                              (nodeBlock n)
                              | ns => List.foreach
                                      (ns, fn n =>
                                       visitForceMultiUsedBlock controlFlow
                                       (nodeBlock n)))
                          end
                     else let   
                            val _ = MultiUsed.when
                                    (FuncInfo.multiUsed fi,
                                     fn () => forceMultiUsedFunc f)
                            val {graph, nodeBlock, ...} = Function.controlFlow f
                          in
                            List.foreach
                            (Graph.stronglyConnectedComponents graph,
                             fn [] => ()
                              | [n] => if Node.hasEdge {from = n, to = n}
                                         then forceMultiUsedBlock (nodeBlock n)
                                         else ()
                              | ns => List.foreach 
                                      (ns, fn n =>
                                       forceMultiUsedBlock (nodeBlock n)))
                          end
          end

      val _ = List.foreach
              (Graph.stronglyConnectedComponents G,
               fn [] => ()
                | [n] =>
                     visitFunc (Node.hasEdge {from = n, to = n}) (nodeFunction n)
                | ns => List.foreach 
                        (ns, fn n =>
                         visitFunc true (nodeFunction n)))

(*
      val _ = Control.diagnostics
              (fn display =>
               let open Layout
               in 
                 display (Layout.str "\n\nMulti:") ;
                 display (seq [Layout.str "usesThreadsOrConts: ",
                               Bool.layout usesThreadsOrConts]) ;
                 List.foreach
                 (functions, fn f =>
                  let 
                    val {name = f, blocks, ...} = Function.dest f
                  in 
                    display (seq [Func.layout f,
                                  str ": ",
                                  FuncInfo.layout (funcInfo f)]) ;
                    Vector.foreach
                    (blocks, fn Block.T {label, ...} =>
                     display (seq [Label.layout label,
                                   str ": ",
                                   LabelInfo.layout (labelInfo label)]))
                  end)
               end)
*)
    in
      {
       usesThreadsOrConts = usesThreadsOrConts,

       funcDoesThreadCopyCurrent 
       = ThreadCopyCurrent.does o FuncInfo.threadCopyCurrent o funcInfo,
       funcIsMultiThreaded 
       = MultiThreaded.is o FuncInfo.multiThreaded o funcInfo,
       funcIsMultiUsed 
       = MultiUsed.is o FuncInfo.multiUsed o funcInfo,

       labelDoesThreadCopyCurrent 
       = ThreadCopyCurrent.does o LabelInfo.threadCopyCurrent o labelInfo,
       labelIsMultiThreaded 
       = MultiThreaded.is o LabelInfo.multiThreaded o labelInfo,
       labelIsMultiUsed 
       = MultiUsed.is o LabelInfo.multiUsed o labelInfo,

       varIsMultiDefed
       = MultiUsed.is o VarInfo.multiUsed o varInfo
       }
    end
end
