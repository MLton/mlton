(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CommonArg (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S
open Exp Transfer

structure Graph = DirectedGraph
structure Node = Graph.Node

structure VarInfo =
  struct
    datatype t = T of {node: unit DirectedGraph.Node.t}

    fun layout lNode (T {node, ...}) =
      let open Layout
      in record [("node", lNode node)]
      end

    local
      fun make f (T r) = f r
    in
      val node = make #node
    end

    fun new node = T {node = node}
  end

structure NodeInfo =
  struct
    datatype t = T of {var: Var.t}

    local
      fun make f (T r) = f r
    in
      val var = make #var
    end

    fun new var = T {var = var}
  end

fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = nodeInfo: unit Node.t -> NodeInfo.t, 
           set = setNodeInfo, ...} =
         Property.getSetOnce
         (Node.plist,
          Property.initRaise ("CommonArg.nodeInfo", Node.layout))
      val nodeInfo =
         Trace.trace ("CommonArg.nodeInfo", Layout.ignore, Layout.ignore)
         nodeInfo
      val {get = labelArgs: Label.t -> (Var.t * Type.t) vector, 
           set = setLabelArgs, ...} =
         Property.getSetOnce
         (Label.plist, 
          Property.initRaise ("CommonArg.labelArgs", Label.layout))
      val labelArgs =
         Trace.trace ("CommonArg.labelArgs", Layout.ignore, Layout.ignore)
         labelArgs
      (* Argument flow graph. *)
      val G = Graph.new ()
      val root = Graph.newNode G
      fun newNode (v: Var.t): unit Node.t =
         let
            val node = Graph.newNode G
            val () = setNodeInfo (node, NodeInfo.new v)
         in
            node
         end
      fun newRootedNode v =
         let
            val node = newNode v
            val _ = Graph.addEdge (G, {from = root, to = node})
         in
            node
         end
      val {get = varInfo: Var.t -> VarInfo.t, 
           set = setVarInfo, ...} =
         Property.getSetOnce (Var.plist,
                              Property.initFun (VarInfo.new o newRootedNode))
      val varInfo =
         Trace.trace ("CommonArg.varInfo", Layout.ignore, Layout.ignore)
         varInfo
      val varNode = VarInfo.node o varInfo
      (* Analyze *)
      val () =
         List.foreach
         (functions, fn f =>
          let
             val {blocks, ...} = Function.dest f
             val () = 
                Vector.foreach
                (blocks, fn Block.T {label, args, ...} =>
                 (setLabelArgs (label, args)
                  ; Vector.foreach (args, fn (v, _) =>
                                    setVarInfo (v, VarInfo.new (newNode v)))))
            (* Flow Transfer.Goto arguments. *)
            fun flowVarVar (v, v'): unit = 
               ignore (Graph.addEdge (G, {from = varNode v, to = varNode v'}))
            fun flowVarVarTy (v, (v', _)) = flowVarVar (v, v')
            fun flowVarsVarTys (vs, vts') =
               Vector.foreach2 (vs, vts', flowVarVarTy)
            fun flowVarsLabelArgs (vs, l) = flowVarsVarTys (vs, labelArgs l)
            (* Visit in unknown contexts. *)
            fun visitVar v =
               ignore (Graph.addEdge (G, {from = root, to = varNode v}))
            fun visitVarTy (v, _) = visitVar v
            fun visitArgs args = Vector.foreach (args, visitVarTy)
            fun visitLabelArgs l = visitArgs (labelArgs l)
          in
             Vector.foreach
             (blocks, fn Block.T {transfer, ...} =>
              case transfer of
                 Arith {overflow, success, ...} =>
                    (visitLabelArgs overflow; visitLabelArgs success)
               | Bug => ()
               | Call {return, ...} =>
                    (case return of
                        Return.NonTail {cont, handler} =>
                           (visitLabelArgs cont
                            ; (case handler of
                                  Handler.Handle hand => visitLabelArgs hand
                                | _ => ()))
                      | _ => ())
               | Case {cases, default, ...} =>
                    (Cases.foreach (cases, visitLabelArgs)
                     ; Option.app (default, visitLabelArgs))
               | Goto {dst, args} => flowVarsLabelArgs (args, dst)
               | Raise _ => ()
               | Return _ => ()
               | Runtime {return, ...} => visitLabelArgs return)
          end)
      val () = Graph.removeDuplicateEdges G
      val {idom} = Graph.dominators (G, {root = root})
      fun getVar (v: Var.t): Var.t =
         case idom (varNode v) of
            Graph.Idom parent => if Node.equals (parent, root)
                                    then v
                                 else NodeInfo.var (nodeInfo parent)
          | Graph.Unreachable => v
          | Graph.Root => v
      fun keepVar v = Var.equals (v, getVar v)
      (* Diagnostics *)
      val () = 
         Control.diagnostics
         (fn display =>
          List.foreach
          (functions, fn f =>
           let
              val {blocks, name, ...} = Function.dest f
              open Layout
              fun lNode n = 
                 record [("idom", case idom n of
                          Graph.Idom parent => 
                             if Node.equals (parent, root)
                                then str "root"
                             else Var.layout (NodeInfo.var (nodeInfo parent))
                        | _ => str "???")]
           in
              display (seq [str "\n", Func.layout name])
              ; (Vector.foreach
                 (blocks, fn Block.T {args, label, ...} =>
                  if Vector.exists (args, not o keepVar o #1)
                     then
                        display
                        (seq [Label.layout label,
                              str " ",
                              Vector.layout
                              (fn (v, _) =>
                               seq [Var.layout v,
                                    str ": ",
                                    VarInfo.layout lNode (varInfo v)])
                              args])
                  else ()))
           end))
      (* Transform *)
      val shrink = shrinkFunction {globals = globals}
      val functions =
         List.revMap
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, start, raises, returns} =
                Function.dest f
             val blocks = 
                Vector.map
                (blocks, fn Block.T {args, label, statements, transfer} =>
                 let
                    val {yes = args, no = rems} = 
                       Vector.partition (args, keepVar o #1)
                    val statements =
                       if Vector.isEmpty rems
                          then statements
                       else Vector.concat [Vector.map
                                           (rems, fn (v, ty) =>
                                            Statement.T {var = SOME v,
                                                         ty = ty,
                                                         exp = Var (getVar v)}),
                                           statements]
                    val transfer =
                       case transfer of
                          Goto {args, dst} => 
                             let
                                val args =
                                   Vector.keepAllMap2
                                   (args, labelArgs dst, fn (arg, (v, _)) =>
                                    if keepVar v
                                       then SOME arg
                                    else NONE)
                             in
                                Goto {args = args, dst = dst}
                             end
                        | _ => transfer 
                 in
                    Block.T {args = args,
                             label = label, 
                             statements = statements,
                             transfer = transfer}
                 end)
          in
             shrink (Function.new {args = args,
                                   blocks = blocks,
                                   mayInline = mayInline,
                                   name = name,
                                   start = start,
                                   raises = raises,
                                   returns = returns})
          end)
      val program =
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = functions,
                    main = main}
      val () = Program.clearTop program
   in
      program
   end

end
