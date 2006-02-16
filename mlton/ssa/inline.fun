(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Inline (S: INLINE_STRUCTS): INLINE = 
struct

open S
open Exp Transfer

type int = Int.t

structure Size =
   struct
      val check : (int * int option) -> bool =
         fn (_, NONE) => false
          | (size, SOME size') => size > size'

      val defaultExpSize : Exp.t -> int = 
         fn ConApp {args, ...} => 1 + Vector.length args
          | Const _ => 0
          | PrimApp {args, ...} => 1 + Vector.length args
          | Profile _ => 0
          | Select _ => 1 + 1
          | Tuple xs => 1 + Vector.length xs
          | Var _ => 0
      fun expSize (size, max) (doExp, _) exp =
         let
            val size' = doExp exp
            val size = size + size'
         in
            (size, check (size, max))
         end
      fun statementSize (size, max) (doExp, doTransfer) =
         fn Statement.T {exp, ...} => expSize (size, max) (doExp, doTransfer) exp
      fun statementsSize (size, max) (doExp, doTransfer) statements =
         Exn.withEscape
         (fn escape =>
          Vector.fold
          (statements, (size, false), fn (statement, (size, check)) =>
           if check
              then escape (size, check)
           else statementSize (size, max) (doExp, doTransfer) statement))
      val defaultTransferSize =
         fn Arith {args, ...} => 1 + Vector.length args
          | Bug => 1
          | Call {args, ...} => 1 + Vector.length args
          | Case {cases, ...} => 1 + Cases.length cases
          | Goto {args, ...} => 1 + Vector.length args
          | Raise xs => 1 + Vector.length xs
          | Return xs => 1 + Vector.length xs
          | Runtime {args, ...} => 1 + Vector.length args
      fun transferSize (size, max) (_, doTransfer) transfer =
         let
            val size' = doTransfer transfer
            val size = size + size'
         in
            (size, check (size, max))
         end
      fun blockSize (size, max) (doExp, doTransfer) =
         fn Block.T {statements, transfer, ...} =>
         case statementsSize (size, max) (doExp, doTransfer) statements of
            (size, true) => (size, true)
          | (size, false) => transferSize (size, max) (doExp, doTransfer) transfer
      fun blocksSize (size, max) (doExp, doTransfer) blocks =
         Exn.withEscape
         (fn escape =>
          Vector.fold
          (blocks, (size, false), fn (block, (size, check)) =>
           if check
              then escape (size, check)
           else blockSize (size, max) (doExp, doTransfer) block))
      fun functionSize (size, max) (doExp, doTransfer) f =
         blocksSize (size, max) (doExp, doTransfer) (#blocks (Function.dest f))

      val default = (defaultExpSize, defaultTransferSize)
      fun functionGT max = #2 o (functionSize (0, max) default)
   end

local
   fun 'a make (dontInlineFunc: Function.t * 'a -> bool)
      (Program.T {functions, ...}, a: 'a): Func.t -> bool =
      let
         val {get = shouldInline: Func.t -> bool, 
              set = setShouldInline, ...} =
            Property.getSetOnce (Func.plist, Property.initConst false)
      in
         List.foreach
         (functions, fn f =>
          if not (Function.mayInline f) orelse dontInlineFunc (f, a)
             then ()
          else setShouldInline (Function.name f, true))
         ; Control.diagnostics
           (fn display =>
            let open Layout
            in List.foreach
               (functions, fn f => 
                let 
                   val name = Function.name f
                   val shouldInline = shouldInline name
                in 
                   display
                   (seq [Func.layout name, str ": ",
                         record [("shouldInline", Bool.layout shouldInline)]])
                end)
            end)
         ; shouldInline
      end
   fun containsCall (f: Function.t): bool =
      Exn.withEscape
      (fn escape =>
       (Vector.foreach
        (Function.blocks f, fn Block.T {transfer, ...} =>
         case transfer of
            Call _ => escape true
          | _ => ())
        ; false))
   fun containsLoop (f: Function.t): bool =
      let
         val {get, set, destroy} =
            Property.destGetSet (Label.plist, Property.initConst false)
      in
         Exn.withEscape
         (fn escape =>
          let
             val _ =
                Function.dfs
                (f, fn (Block.T {label, transfer, ...}) =>
                 (set (label, true)
                  ; (case transfer of
                        Goto {dst, ...} => if get dst then escape true else ()
                      | _ => ())
                  ; fn () => set (label, false)))
          in
             false
          end)
         before (destroy ())
      end
in
   val leaf = make (fn (f, {size}) =>
                    Size.functionGT size f
                    orelse containsCall f)
   val leafNoLoop = make (fn (f, {size}) =>
                          Size.functionGT size f
                          orelse containsCall f
                          orelse containsLoop f)
end

structure Graph = DirectedGraph
structure Node = Graph.Node

fun product (Program.T {functions, ...}, {small: int, product: int}) =
   let
      type info = {doesCallSelf: bool ref,
                   function: Function.t,
                   node: unit Node.t,
                   numCalls: int ref,
                   shouldInline: bool ref,
                   size: int ref}
      val {get = funcInfo: Func.t -> info,
           set = setFuncInfo, ...} =
         Property.getSetOnce
         (Func.plist, Property.initRaise ("funcInfo", Func.layout))
      val {get = nodeFunc: unit Node.t -> Func.t,
           set = setNodeFunc, ...} = 
         Property.getSetOnce 
         (Node.plist, Property.initRaise ("nodeFunc", Node.layout))
      val graph = Graph.new ()
      (* initialize the info for each func *)
      val _ = 
         List.foreach
         (functions, fn f =>
          let 
             val name = Function.name f
             val n = Graph.newNode graph
          in
             setNodeFunc (n, name)
             ; setFuncInfo (name, {doesCallSelf = ref false,
                                   function = f,
                                   node = n,
                                   numCalls = ref 0,
                                   shouldInline = ref false,
                                   size = ref 0})
          end)
      (* Update call counts. *)
      val _ = 
         List.foreach
         (functions, fn f =>
          let 
             val {name, blocks, ...} = Function.dest f
             val {doesCallSelf, ...} = funcInfo name
          in
             Vector.foreach
             (blocks, fn Block.T {transfer, ...} =>
              case transfer of
                 Call {func, ...} =>
                    let
                       val {numCalls, ...} = funcInfo func
                    in
                       if Func.equals (name, func)
                          then doesCallSelf := true
                       else Int.inc numCalls
                    end
               | _ => ())
          end)
      fun mayInline (setSize: bool,
                     {function, doesCallSelf, numCalls, size, ...}: info): bool =
         Function.mayInline function
         andalso not (!doesCallSelf)
         andalso let
                    val (n, _) = 
                       Size.functionSize
                       (0, NONE)
                       (Size.defaultExpSize,
                        fn t as Call {func, ...} =>
                              let
                                val {shouldInline, size, ...} = funcInfo func
                              in
                                if !shouldInline
                                   then !size
                                else Size.defaultTransferSize t
                              end
                         | t => Size.defaultTransferSize t)
                       function
                 in
                    if setSize
                       then size := n
                    else ()
                    ; (!numCalls - 1) * (n - small) <= product
                 end
      (* Build the call graph.  Do not include functions that we already know
       * will not be inlined.
       *)
      val _ =
         List.foreach
         (functions, fn f => 
          let 
             val {name, blocks, ...} = Function.dest f
             val info as {node, ...} = funcInfo name
          in
             if mayInline (false, info)
                then Vector.foreach
                     (blocks, fn Block.T {transfer, ...} =>
                      case transfer of
                         Call {func, ...} =>
                            if Func.equals (name, func)
                               then ()
                            else (ignore o Graph.addEdge)
                                 (graph, {from = node, to = #node (funcInfo func)})
                       | _ => ())
             else ()
          end)
      (* Compute strongly-connected components.
       * Then start at the leaves of the call graph and work up.
       *)
      val _ = 
         List.foreach
         (rev (Graph.stronglyConnectedComponents graph),
          fn [n] => let val info as {shouldInline, ...} = funcInfo (nodeFunc n)
                    in shouldInline := mayInline (true, info)
                    end
           | _ => ())
      val _ =
         Control.diagnostics
         (fn display =>
          let open Layout
          in List.foreach
             (functions, fn f => 
              let 
                 val name = Function.name f
                 val {numCalls, shouldInline, size, ...} = funcInfo name
                 val numCalls = !numCalls
                 val shouldInline = !shouldInline
                 val size = !size
              in 
                 display
                 (seq [Func.layout name, str ": ",
                       record [("numCalls", Int.layout numCalls),
                               ("size", Int.layout size),
                               ("shouldInline", Bool.layout shouldInline)]])
              end)
          end)
   in
      ! o #shouldInline o funcInfo
   end

fun inline (program as Program.T {datatypes, globals, functions, main}) =
   let
      val shouldInline: Func.t -> bool =
         let open Control
         in case !inline of
            NonRecursive r => product (program, r)
          | Leaf r => leaf (program, r)
          | LeafNoLoop r => leafNoLoop (program, r)
         end
      val {get = funcInfo: Func.t -> {function: Function.t,
                                      isCalledByMain: bool ref},
           set = setFuncInfo, ...} =
         Property.getSetOnce
         (Func.plist, Property.initRaise ("Inline.funcInfo", Func.layout))
      val () = List.foreach (functions, fn f =>
                             setFuncInfo (Function.name f,
                                          {function = f,
                                           isCalledByMain = ref false}))
      val () =
         Vector.foreach (#blocks (Function.dest (Program.mainFunction program)),
                         fn Block.T {transfer, ...} =>
                         case transfer of
                            Transfer.Call {func, ...} =>
                               #isCalledByMain (funcInfo func) := true
                          | _ => ())
      fun doit (blocks: Block.t vector,
                return: Return.t) : Block.t vector =
         let
            val newBlocks = ref []
            val blocks =
               Vector.map
               (blocks,
                fn block as Block.T {label, args, statements, transfer} =>
                let
                   fun new transfer =
                      Block.T {label = label,
                               args = args,
                               statements = statements,
                               transfer = transfer}
                in
                  case transfer of
                     Call {func, args, return = return'} =>
                        let
                           val return = Return.compose (return, return')
                        in
                           if shouldInline func
                              then 
                              let
                                 local
                                    val {name, args, start, blocks, ...} =
                                       (Function.dest o Function.alphaRename) 
                                       (#function (funcInfo func))
                                    val blocks = doit (blocks, return)
                                    val _ = List.push (newBlocks, blocks)
                                    val name =
                                       Label.newString (Func.originalName name)
                                    val _ = 
                                       List.push 
                                       (newBlocks,
                                        Vector.new1
                                        (Block.T
                                         {label = name,
                                          args = args,
                                          statements = Vector.new0 (),
                                          transfer = Goto {dst = start,
                                                           args = Vector.new0 ()}}))
                                 in
                                    val name = name
                                 end
                              in
                                 new (Goto {dst = name, 
                                            args = args})
                              end
                           else new (Call {func = func,
                                           args = args,
                                           return = return})
                        end
                   | Raise xs =>
                        (case return of
                            Return.NonTail
                            {handler = Handler.Handle handler, ...} =>
                               new (Goto {dst = handler,
                                          args = xs})
                          | _ => block)
                   | Return xs =>
                        (case return of
                            Return.NonTail {cont, ...} =>
                               new (Goto {dst = cont, args = xs})
                          | _ => block)
                   | _ => block
                end)
         in
            Vector.concat (blocks::(!newBlocks))
         end
      val shrink = shrinkFunction {globals = globals}
      val inlineIntoMain = !Control.inlineIntoMain
      val functions =
         List.fold
         (functions, [], fn (f, ac) =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             fun keep () =
                let
                   val blocks = doit (blocks, Return.Tail)
                in
                   shrink (Function.new {args = args,
                                         blocks = blocks,
                                         mayInline = mayInline,
                                         name = name,
                                         raises = raises,
                                         returns = returns,
                                         start = start})
                   :: ac
                end
          in
             if Func.equals (name, main)
                then if inlineIntoMain
                        then keep ()
                     else f :: ac
             else
                if shouldInline name
                   then
                      if inlineIntoMain
                         orelse not (! (#isCalledByMain (funcInfo name)))
                         then ac
                      else keep ()
                else keep ()
          end)
      val program =
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = functions,
                    main = main}
      val _ = Program.clearTop program
   in
      program
   end

end
