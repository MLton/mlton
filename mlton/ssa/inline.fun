(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Inline (S: INLINE_STRUCTS): INLINE = 
struct

open S
open Exp Transfer

structure Function =
   struct
      open Function

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
in
   val leafOnce = make (fn (f, {size}) =>
                        Option.isNone (Function.sizeMax (f, {max = size,
                                                             sizeExp = Exp.size,
                                                             sizeTransfer =Transfer.size}))
                        orelse Function.containsCall f)
   val leafOnceNoLoop = make (fn (f, {size}) =>
                              Option.isNone (Function.sizeMax (f, {max = size,
                                                                   sizeExp = Exp.size,
                                                                   sizeTransfer =Transfer.size}))
                              orelse Function.containsCall f
                              orelse Function.containsLoop f)
end

structure Graph = DirectedGraph
structure Node = Graph.Node

local
   fun make (dontInline: Function.t -> bool)
      (Program.T {functions, ...}, {size: int option}) =
      let
         val max = size
         type info = {function: Function.t,
                      node: unit Node.t,
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
                ; setFuncInfo (name, {function = f,
                                      node = n,
                                      shouldInline = ref false,
                                      size = ref 0})
             end)
         (* Build the call graph. *)
         val _ =
            List.foreach
            (functions, fn f => 
             let 
                val {name, blocks, ...} = Function.dest f
                val {node, ...} = funcInfo name
             in
                Vector.foreach
                (blocks, fn Block.T {transfer, ...} =>
                 case transfer of
                    Call {func, ...} =>
                       (ignore o Graph.addEdge)
                       (graph, {from = node, to = #node (funcInfo func)})
                  | _ => ())
             end)
         (* Compute strongly-connected components.
          * Then start at the leaves of the call graph and work up.
          *)
         val _ = 
            List.foreach
            (rev (Graph.stronglyConnectedComponents graph),
             fn scc =>
             case scc of 
                [n] =>
                   let 
                      val {function, shouldInline, size, ...} = 
                         funcInfo (nodeFunc n)
                   in 
                      if Function.mayInline function
                         andalso not (dontInline function)
                         then Exn.withEscape
                              (fn escape =>
                               let
                                  val res =
                                     Function.sizeMax
                                     (function,
                                      {max = max,
                                       sizeExp = Exp.size,
                                       sizeTransfer =
                                       fn t =>
                                       case t of
                                          Call {func, ...} =>
                                             let
                                                val {shouldInline, size, ...} =
                                                   funcInfo func
                                             in
                                                if !shouldInline
                                                   then !size
                                                   else escape ()
                                             end
                                        | _ => Transfer.size t})
                               in
                                  case res of
                                     NONE => ()
                                   | SOME n => (shouldInline := true
                                                ; size := n)
                               end)
                      else ()
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
                    val {shouldInline, size, ...} = funcInfo name
                    val shouldInline = !shouldInline
                    val size = !size
                 in 
                    display
                    (seq [Func.layout name, str ": ",
                          record [("shouldInline", Bool.layout shouldInline),
                                  ("size", Int.layout size)]])
                 end)
             end)
      in
         ! o #shouldInline o funcInfo
      end
in
   val leafRepeat = make (fn _ => false)
   val leafRepeatNoLoop = make (fn f => Function.containsLoop f)
end

fun nonRecursive (Program.T {functions, ...}, {small: int, product: int}) =
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
                    val n =
                       Function.size
                       (function,
                        {sizeExp = Exp.size,
                         sizeTransfer =
                         fn t as Call {func, ...} =>
                               let
                                  val {shouldInline, size, ...} = funcInfo func
                               in
                                  if !shouldInline
                                     then !size
                                     else Transfer.size t
                               end
                          | t => Transfer.size t})
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
                               ("shouldInline", Bool.layout shouldInline),
                               ("size", Int.layout size)]])
              end)
          end)
   in
      ! o #shouldInline o funcInfo
   end

fun transform {program as Program.T {datatypes, globals, functions, main},
               shouldInline: Func.t -> bool,
               inlineIntoMain: bool} =
   let
      val {get = funcInfo: Func.t -> {function: Function.t,
                                      isCalledByMain: bool ref},
           set = setFuncInfo, ...} =
         Property.getSetOnce
         (Func.plist, Property.initRaise ("Inline.funcInfo", Func.layout))
      val isCalledByMain: Func.t -> bool =
         ! o #isCalledByMain o funcInfo
      val () = List.foreach (functions, fn f =>
                             setFuncInfo (Function.name f,
                                          {function = f,
                                           isCalledByMain = ref false}))
      val () =
         Vector.foreach 
         (#blocks (Function.dest (Program.mainFunction program)),
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
                         orelse not (isCalledByMain name)
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

fun inlineLeaf (p, {loops, repeat, size}) =
   if size = SOME 0
      then p
   else transform {program = p,
                   shouldInline = 
                   case (loops, repeat) of
                      (false, false) => leafOnce (p, {size = size})
                    | (false, true) => leafRepeat (p, {size = size})
                    | (true, false) => leafOnceNoLoop (p, {size = size})
                    | (true, true) => leafRepeatNoLoop (p, {size = size}),
                   inlineIntoMain = true}
fun inlineNonRecursive (p, arg) =
   transform {program = p,
              shouldInline = nonRecursive (p, arg),
              inlineIntoMain = !Control.inlineIntoMain}

end
