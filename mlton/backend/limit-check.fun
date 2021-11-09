(* Copyright (C) 2009,2019-2021 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * The goal of limit check insertion is to ensure that
 *      1. At any allocation of b bytes, frontier + b <= base + heapSize
 *      2. At entry to each function, stackTop <= stackLimit
 * 
 * It assumes that runtime provides several operands to help with this.
 *      Frontier
 *      Limit
 *      LimitPlusSlop
 *      StackLimit
 *      StackTop
 * 
 * There are three different kinds of checks inserted, depending on the
 * amount being allocated and whether or not the program uses signal
 * handlers.
 * 
 * 1. If b <= LIMIT_SLOP, then continue (don't GC) if
 * 
 *      frontier <= limit
 * 
 *    The reason this works is that if frontier <= limit and b <=
 *    LIMIT_SLOP, then 
 *      frontier + b <= limit + LIMIT_SLOP 
 *                       = limitPlusSlop
 *                       = base + heapSize
 *    This works even if the program uses signal handlers, which set
 *    limit to zero, since frontier <= 0 will always be false.
 * 
 * 2. If b > LIMIT_SLOP and if the program doesn't use signal handlers,
 *    then continue (don't GC) if
 * 
 *      b <= limitPlusSlop - frontier
 * 
 *    The reason this works is that the condition is equivalent to
 *      
 *      b + frontier <= limitPlusSlop = base + heapSize
 * 
 *    We write the condition the way we do instead of the more obvious way
 *    because "b + frontier" may overflow, while limitPlusSlop - frontier
 *    can not, unless the program uses signal handlers.
 * 
 * 3. If b > LIMIT_SLOP and if the program uses signal handlers, then
 *    continue (don't GC) if
 * 
 *      limit > 0
 *      and b <= limitPlusSlop - frontier
 * 
 *    This is like case (2), except that because the program uses signal
 *    handlers, the runtime may have set limit to zero to indicate that a
 *    signal needs to be handled.  So, we first check that this is not
 *    the case before continuing as in case (2).
 *
 * Stack limit checks are completely orthogonal to heap checks, and are simply
 * inserted at the start of each function.
 *)
functor LimitCheck (S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM =
struct

open S

structure LimitCheck =
   struct
      datatype t = 
         PerBlock
       | ExtBasicBlocks
       | LoopHeaders of {fullCFG: bool,
                         loopExits: bool}
   end

structure Control =
   struct
      open Control

      datatype limitCheck = datatype LimitCheck.t

      val limitCheck =
         ref (LoopHeaders {fullCFG = false,
                           loopExits = true})
   end

datatype z = datatype Transfer.t

structure CFunction =
   struct
      open CFunction Type.BuiltInCFunction
   end

structure Statement =
   struct
      open Statement

      fun bytesAllocated (s: t, {tyconTy}): Bytes.t =
         case s of
            Object {obj, ...} => Object.size (obj, {tyconTy = tyconTy})
          | _ => Bytes.zero
   end

structure Transfer =
   struct
      open Transfer

      datatype bytesAllocated =
         Big of Operand.t
       | Small of Bytes.t

      fun bytesAllocated (t: t): bytesAllocated =
         case t of
            CCall {args, func, ...} =>
               (case CFunction.bytesNeeded func of
                   NONE => Small Bytes.zero
                 | SOME i =>
                      let
                         val z = Vector.sub (args, i)
                      in
                         case z of
                            Operand.Const c =>
                               (case c of
                                   Const.Word w =>
                                      let
                                         val w = WordX.toIntInf w
                                      in
                                         (* 512 is small and arbitrary *)
                                         if w <= 512 
                                            then Small (Bytes.fromIntInf w)
                                         else Big z
                                      end
                                 | _ => Error.bug "LimitCheck.Transfer.bytesAllocated: strange numBytes")
                          | _ => Big z
                      end)
          | _ => Small Bytes.zero
   end

structure Block =
   struct
      open Block

      fun objectBytesAllocated (T {statements, transfer, ...}, {tyconTy}): Bytes.t =
         Bytes.+
         (Vector.fold (statements, Bytes.zero, fn (s, ac) =>
                       let
                          val b = Statement.bytesAllocated (s, {tyconTy = tyconTy})
                       in
                          Bytes.+ (ac, b)
                       end),
          case Transfer.bytesAllocated transfer of
             Transfer.Big _ => Bytes.zero
           | Transfer.Small b => b)
   end

fun insertFunction (f: Function.t,
                    handlesSignals: bool,
                    newFlag: unit -> Operand.t,
                    blockCheckAmount: {blockIndex: int} -> Bytes.t,
                    ensureFree: Label.t -> Bytes.t) =
   let
      val {args, blocks, name, raises, returns, start} = Function.dest f
      val newBlocks = ref []
      local
         val r: Label.t option ref = ref NONE
      in
         fun heapCheckTooLarge () =
            case !r of
               SOME l => l
             | NONE =>
                  let
                     val l = Label.newNoname ()
                     val _ = r := SOME l
                     val cfunc =
                        CFunction.T {args = Vector.new0 (),
                                     convention = CFunction.Convention.Cdecl,
                                     inline = false,
                                     kind = CFunction.Kind.Runtime {bytesNeeded = NONE,
                                                                    ensuresBytesFree = NONE,
                                                                    mayGC = false,
                                                                    maySwitchThreadsFrom = false,
                                                                    maySwitchThreadsTo = false,
                                                                    modifiesFrontier = false,
                                                                    readsStackTop = false,
                                                                    writesStackTop = false},
                                     prototype = (Vector.new0 (), NONE),
                                     return = Type.unit,
                                     symbolScope = CFunction.SymbolScope.Private,
                                     target = CFunction.Target.Direct "MLton_heapCheckTooLarge"}
                     val _ =
                        List.push
                        (newBlocks,
                         Block.T {args = Vector.new0 (),
                                  kind = Kind.Jump,
                                  label = l,
                                  statements = Vector.new0 (),
                                  transfer =
                                  Transfer.CCall {args = Vector.new0 (),
                                                  func = cfunc,
                                                  return = NONE}})
                  in
                     l
                  end
      end
      val _ =
         Vector.foreachi
         (blocks, fn (i, Block.T {args, kind, label, statements, transfer}) =>
          let
             val transfer = 
                case transfer of
                   Transfer.CCall {args, func, return} =>
                      (case CFunction.ensuresBytesFree func of
                          NONE => transfer
                        | SOME i =>
                             Transfer.CCall
                             {args = Vector.mapi
                                     (args, fn (j, arg) =>
                                      if i = j
                                         then Operand.word
                                              (WordX.fromBytes
                                               (ensureFree (valOf return),
                                                WordSize.csize ()))
                                         else arg),
                              func = func,
                              return = return})
                 | _ => transfer
             val stack = Label.equals (start, label)
             fun insert (amount: Operand.t (* of type word *)) =
                let
                   val collect = Label.newNoname ()
                   val collectReturn = Label.newNoname ()
                   val doneCollect = Label.newNoname ()
                   val (dontCollect, collectReturnStatements, force) =
                      case !Control.gcCheck of
                         Control.First =>
                            let
                               val flag = newFlag ()
                               val dontCollect = Label.newNoname ()
                               val _ =
                                  List.push
                                  (newBlocks,
                                   Block.T
                                   {args = Vector.new0 (),
                                    kind = Kind.Jump,
                                    label = dontCollect,
                                    statements = Vector.new0 (),
                                    transfer =
                                    Transfer.ifBoolE
                                    (flag,
                                     !Control.gcExpect,
                                     {falsee = doneCollect,
                                      truee = collect})})
                            in
                               (dontCollect,
                                Vector.new1
                                (Statement.Move {dst = flag,
                                                 src = Operand.bool false}),
                                flag)
                            end
                       | Control.Limit =>
                            (doneCollect, Vector.new0 (), Operand.bool false)
                       | Control.Every =>
                            (collect, Vector.new0 (), Operand.bool true)
                   val func = CFunction.gc {maySwitchThreads = handlesSignals}
                   val _ =
                      newBlocks :=
                      Block.T {args = Vector.new0 (),
                               kind = Kind.Jump,
                               label = collect,
                               statements = Vector.new0 (),
                               transfer = (Transfer.CCall
                                           {args = Vector.new3 (Operand.GCState,
                                                                amount,
                                                                force),
                                            func = func,
                                            return = SOME collectReturn})}
                      :: Block.T {args = Vector.new0 (),
                                  kind = Kind.CReturn {func = func},
                                  label = collectReturn,
                                  statements = collectReturnStatements,
                                  transfer = Transfer.Goto {dst = doneCollect,
                                                            args = Vector.new0 ()}}
                      :: Block.T {args = Vector.new0 (),
                                  kind = Kind.Jump,
                                  label = doneCollect,
                                  statements = statements,
                                  transfer = transfer}
                      :: !newBlocks
                in
                   {collect = collect,
                    dontCollect = dontCollect}
                end
             fun newBlock (isFirst, statements, transfer) =
                let
                   val (args, kind, label) =
                      if isFirst
                         then (args, kind, label)
                      else (Vector.new0 (), Kind.Jump, Label.newNoname ())
                   val _ =
                      List.push
                      (newBlocks,
                       Block.T {args = args,
                                kind = kind,
                                label = label,
                                statements = statements,
                                transfer = transfer})
                in
                   label
                end
             fun gotoHeapCheckTooLarge () =
                newBlock
                (true,
                 Vector.new0 (),
                 Transfer.Goto {args = Vector.new0 (),
                                dst = heapCheckTooLarge ()})
             fun primApp (prim, op1, op2, {collect, dontCollect}) =
                let
                   val res = Var.newNoname ()
                   val s =
                      Statement.PrimApp {args = Vector.new2 (op1, op2),
                                         dst = SOME (res, Type.bool),
                                         prim = prim}
                   val transfer =
                      Transfer.ifBoolE
                      (Operand.Var {var = res, ty = Type.bool},
                       !Control.gcExpect,
                       {falsee = dontCollect,
                        truee = collect})
                in
                   (Vector.new1 s, transfer)
                end
             datatype z = datatype Runtime.GCField.t
             fun stackCheck (maybeFirst, z): Label.t =
                let
                   val (statements, transfer) =
                      primApp (Prim.CPointer_lt,
                               Operand.Runtime StackLimit,
                               Operand.Runtime StackTop,
                               z)
                in
                   newBlock (maybeFirst, statements, transfer)
                end
             fun maybeStack (): unit =
                if stack
                   then ignore (stackCheck
                                (true,
                                 insert (Operand.zero (WordSize.csize ()))))
                else
                   (* No limit check, just keep the block around. *)
                   List.push (newBlocks,
                              Block.T {args = args,
                                       kind = kind,
                                       label = label,
                                       statements = statements,
                                       transfer = transfer})
             fun frontierCheck (isFirst,
                                prim, op1, op2,
                                z as {collect, dontCollect = _}): Label.t =
                let
                   val (statements, transfer) = primApp (prim, op1, op2, z)
                   val l = newBlock (isFirst andalso not stack,
                                     statements, transfer)
                in
                   if stack
                      then stackCheck (isFirst, {collect = collect,
                                                 dontCollect = l})
                   else l
                end
             fun heapCheck (isFirst: bool,
                            amount: Operand.t (* of type word *)): Label.t =
                let
                   val z as {collect, ...} = insert amount
                   val res = Var.newNoname ()
                   val s =
                      (* Can't do Limit - Frontier, because don't know that
                       * Frontier < Limit.
                       *)
                      Statement.PrimApp
                      {args = Vector.new2 (Operand.Runtime LimitPlusSlop,
                                           Operand.Runtime Frontier),
                       dst = SOME (res, Type.csize ()),
                       prim = Prim.CPointer_diff}
                   val (statements, transfer) =
                      primApp (Prim.Word_lt (WordSize.csize (), {signed = false}),
                               Operand.Var {var = res, ty = Type.csize ()},
                               amount,
                               z)
                   val statements = Vector.concat [Vector.new1 s, statements]
                in
                   if handlesSignals
                      then
                         frontierCheck (isFirst,
                                        Prim.CPointer_equal,
                                        Operand.Runtime Limit,
                                        Operand.null,
                                        {collect = collect,
                                         dontCollect = newBlock (false,
                                                                 statements,
                                                                 transfer)})
                   else if stack
                           then
                              stackCheck
                              (isFirst,
                               {collect = collect,
                                dontCollect =
                                newBlock (false, statements, transfer)})
                        else newBlock (isFirst, statements, transfer)
                end
             fun heapCheckNonZero (bytes: Bytes.t): unit =
                ignore
                (if Bytes.<= (bytes, Runtime.limitSlop)
                    then frontierCheck (true,
                                        Prim.CPointer_lt,
                                        Operand.Runtime Limit,
                                        Operand.Runtime Frontier,
                                        insert (Operand.zero (WordSize.csize ())))
                 else
                    let
                       val bytes =
                          SOME (WordX.fromBytes (bytes, WordSize.csize ()))
                          handle Overflow => NONE
                    in
                       case bytes of
                          NONE => gotoHeapCheckTooLarge ()
                        | SOME bytes => heapCheck (true, Operand.word bytes)
                    end)
             fun smallAllocation (): unit =
                let
                   val b = blockCheckAmount {blockIndex = i}
                in
                   if Bytes.isZero b
                      then maybeStack ()
                   else heapCheckNonZero b
                end
             fun bigAllocation (bytesNeeded: Operand.t): unit =
                let
                   val extraBytes = blockCheckAmount {blockIndex = i}
                in
                   case bytesNeeded of
                      Operand.Const c =>
                         (case c of
                             Const.Word w =>
                                heapCheckNonZero
                                (Bytes.+
                                 (Bytes.fromIntInf (WordX.toIntInf w),
                                  extraBytes))
                           | _ => Error.bug "LimitCheck.bigAllocation: strange constant bytesNeeded")
                    | _ =>
                         let
                            val bytes = Var.newNoname ()
                            val test = Var.newNoname ()
                            val extraBytes =
                               let
                                  val extraBytes =
                                     WordX.fromBytes (extraBytes, WordSize.csize ())
                               in
                                  SOME extraBytes
                               end handle Overflow => NONE
                         in
                            case extraBytes of
                               NONE => ignore (gotoHeapCheckTooLarge ())
                             | SOME extraBytes =>
                                  (ignore o newBlock)
                                  (true,
                                   Vector.new2
                                   (Statement.PrimApp
                                    {args = Vector.new2
                                            (Operand.word extraBytes,
                                             bytesNeeded),
                                     dst = SOME (bytes, Type.csize ()),
                                     prim = Prim.Word_add (WordSize.csize ())},
                                    Statement.PrimApp
                                    {args = Vector.new2
                                            (Operand.word extraBytes,
                                             bytesNeeded),
                                     dst = SOME (test, Type.bool),
                                     prim = Prim.Word_addCheckP
                                            (WordSize.csize (),
                                             {signed = false})}),
                                   Transfer.ifBoolE
                                   (Operand.Var {var = test, ty = Type.bool},
                                    !Control.gcExpect,
                                    {falsee = heapCheck (false,
                                                         Operand.Var
                                                         {var = bytes,
                                                          ty = Type.csize ()}),
                                     truee = heapCheckTooLarge ()}))
                         end
                end
          in
             case Transfer.bytesAllocated transfer of
                Transfer.Big z => bigAllocation z
              | Transfer.Small _ => smallAllocation ()
          end)
   in
      Function.new {args = args,
                    blocks = Vector.fromList (!newBlocks),
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun insertPerBlock (f: Function.t, handlesSignals, newFlag, tyconTy) =
   let
      val {blocks, ...} = Function.dest f
      fun blockCheckAmount {blockIndex} =
         Block.objectBytesAllocated
         (Vector.sub (blocks, blockIndex),
          {tyconTy = tyconTy})
   in
      insertFunction (f, handlesSignals, newFlag, blockCheckAmount, fn _ => Bytes.zero)
   end

structure Graph = DirectedGraph
structure Node = Graph.Node
structure Edge = Graph.Edge
structure Forest = Graph.LoopForest

val traceMaxPath = Trace.trace ("LimitCheck.maxPath", Int.layout, Bytes.layout)

fun isolateBigTransfers (f: Function.t): Function.t =
   let
      val {args, blocks, name, raises, returns, start} = Function.dest f
      val newBlocks = ref []
      val () =
         Vector.foreach
         (blocks,
          fn block as Block.T {args, kind, label, statements, transfer} =>
          case Transfer.bytesAllocated transfer of
             Transfer.Big _ =>
                let
                   val l = Label.newNoname ()
                in
                   List.push (newBlocks,
                              Block.T {args = args,
                                       kind = kind,
                                       label = label,
                                       statements = statements,
                                       transfer = Goto {args = Vector.new0 (),
                                                        dst = l}})
                   ; List.push (newBlocks,
                                Block.T {args = Vector.new0 (),
                                         kind = Kind.Jump,
                                         label = l,
                                         statements = Vector.new0 (),
                                         transfer = transfer})
                end
           | Transfer.Small _ => List.push (newBlocks, block))
      val blocks = Vector.fromListRev (!newBlocks)
   in
      Function.new {args = args,
                    blocks = blocks,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun insertCoalesce (f: Function.t, handlesSignals, newFlag, tyconTy) =
   let
      val f = isolateBigTransfers f
      val {blocks, start, ...} = Function.dest f
      val n = Vector.length blocks
      val {get = labelIndex, set = setLabelIndex, ...} =
         Property.getSetOnce
         (Label.plist,
          Property.initRaise ("LimitCheck.labelIndex", Label.layout))
      val {get = nodeIndex, set = setNodeIndex, ...} =
         Property.getSetOnce
         (Node.plist, Property.initRaise ("LimitCheck.nodeIndex", Node.layout))
      val _ =
         Vector.foreachi
         (blocks, fn (i, Block.T {label, ...}) =>
          setLabelIndex (label, i))
      (* Build the graph. *)
      val g = Graph.new ()
      val nodes = 
         Vector.tabulate
         (n, fn i => 
          let
             val n = Graph.newNode g
             val _ = setNodeIndex (n, i)
          in 
             n
          end)
      fun indexNode i = Vector.sub (nodes, i)
      val labelNode = indexNode o labelIndex
      val root = Graph.newNode g
      (* mayHaveCheck == E U D
       *   E = set of entry nodes 
       *     = start, Cont, Handler,
       *         or CReturn that doesn't ensure bytesFree
       *         Jump that calls a cfunction with bytesneeded
       *   D = set of decycling nodes
       *)
      val mayHaveCheck =
         Array.tabulate
         (n, fn i =>
          let
             val Block.T {kind, transfer, ...} = Vector.sub (blocks, i)
             datatype z = datatype Kind.t
             val isBigAlloc =
                case Transfer.bytesAllocated transfer of
                   Transfer.Big _ => true
                 | Transfer.Small _ => false
             val b =
                case kind of
                   Cont _ => true
                 | CReturn {func, ...} =>
                      CFunction.mayGC func
                      andalso not (Option.isSome (CFunction.ensuresBytesFree func))
                 | Handler => true
                 | Jump =>
                      (case transfer of
                          Transfer.CCall {args, func, ...} =>
                             (case CFunction.bytesNeeded func of
                                 NONE => true
                               | SOME i => 
                                    (case Vector.sub (args, i) of
                                        Operand.Const _ => false
                                      | _ => true))
                        | _ => false)
          in
             b orelse isBigAlloc
          end)
      val _ = Array.update (mayHaveCheck, labelIndex start, true)
      (* Build cfg. *)
      val _ = Graph.addEdge (g, {from = root, to = labelNode start})
      datatype z = datatype Control.limitCheck
      val fullCFG = 
         case !Control.limitCheck of
            ExtBasicBlocks => true
          | LoopHeaders {fullCFG, ...} => fullCFG
          | _ => Error.bug "LimitCheck.insertCoalesce: fullCFG"
      val _ =
         Vector.foreachi
         (blocks, fn (i, Block.T {transfer, ...}) =>
          let
             val from = indexNode i
          in
             Transfer.foreachLabel
             (transfer, fn l =>
              let
                 val i' = labelIndex l
                 val to = indexNode i'
                 fun addEdge from =
                    (ignore o Graph.addEdge) 
                    (g, {from = from, to = to})
              in
                 if fullCFG
                    then addEdge from
                 else if Array.sub (mayHaveCheck, i')
                         then addEdge root
                      else addEdge from
              end)
          end)
      val objectBytesAllocated =
         Vector.map (blocks, fn b =>
                     Block.objectBytesAllocated (b, {tyconTy = tyconTy}))
      fun insertCoalesceExtBasicBlocks () =
         let
            val preds = Array.new (n, 0)
            fun incPred i =
               Array.update (preds, i, 1 + (Array.sub (preds, i)))
            val _ = 
               Vector.foreach
               (nodes, fn node => 
                List.foreach
                (Node.successors node, 
                 incPred o nodeIndex o Edge.to))
            val _ =
               Array.foreachi
               (preds, fn (i, n) =>
                if n > 1 then Array.update (mayHaveCheck, i, true) else ())
         in
           ()
         end
      fun insertCoalesceLoopHeaders loopExits =
         let
            (* Set equivalence classes, where two nodes are equivalent if they
             * are in the same loop in the loop forest.
             * Also mark loop headers as mayHaveCheck.
             *)
            val classes = Array.array (n, ~1)
            fun indexClass i = Array.sub (classes, i)
            val c = Counter.new 0
            fun setClass (f: unit Node.t Forest.t) =
               let
                  val {loops, notInLoop} = Forest.dest f
                  val class = Counter.next c
                  val _ =
                     Vector.foreach
                     (notInLoop, fn n =>
                      if Node.equals (n, root)
                         then ()
                      else Array.update (classes, nodeIndex n, class))
                  val _ =
                     Vector.foreach
                     (loops, fn {headers, child} =>
                      (Vector.foreach
                       (headers, fn n =>
                        Array.update (mayHaveCheck, nodeIndex n, true))
                       ; setClass child))
               in
                  ()
               end
            val _ = setClass (Graph.loopForestSteensgaard
                              (g, {root = root, nodeValue = fn x => x}))
            val numClasses = Counter.value c
            datatype z = datatype Control.limitCheck
            val _ =
               if loopExits
                  then let
                          (* Determine which classes allocate. *)
                          val classDoesAllocate =
                             Array.array (numClasses, false)
                          val _ =
                             List.foreach
                             (Graph.nodes g, fn n =>
                              if Node.equals (n, root)
                                 then ()
                              else
                              let
                                 val i = nodeIndex n
                              in
                                 if (Bytes.<
                                     (Bytes.zero,
                                      Vector.sub (objectBytesAllocated, i)))
                                    then Array.update (classDoesAllocate, 
                                                       indexClass i, 
                                                       true)
                                 else ()
                              end)
                          (* Mark nodes that are post-exits of non-allocating 
                           * loops as mayHaveCheck.
                           *)
                          val _ =
                             List.foreach
                             (Graph.nodes g, fn n =>
                              if Node.equals (n, root)
                                 then ()
                              else 
                              let
                                 val i = nodeIndex n
                                 val c = indexClass i
                              in
                                if Array.sub (classDoesAllocate, c)
                                   then ()
                                else List.foreach 
                                     (Node.successors n, fn e =>
                                      let
                                         val i' = nodeIndex (Edge.to e)
                                      in
                                         if c <> indexClass i'
                                            then Array.update 
                                                 (mayHaveCheck, i', true)
                                         else ()
                                      end)
                              end)      
                       in
                         ()
                       end
               else ()
         in
            ()
         end
      datatype z = datatype Control.limitCheck
      val _ = 
         case !Control.limitCheck of
            ExtBasicBlocks => insertCoalesceExtBasicBlocks ()
          | LoopHeaders {loopExits, ...} => insertCoalesceLoopHeaders loopExits
          | _ => Error.bug "LimitCheck.insertCoalesce"
      (* If we remove edges into nodes that are mayHaveCheck, we have an
       * acyclic graph.
       * So, we can compute a function, maxPath, inductively that for each node
       * tells the maximum amount allocated along any path that passes only
       * through nodes that are not mayHaveCheck.
       *)
      local
         val a = Array.array (n, NONE)
      in
         fun maxPath arg : Bytes.t =  (* i is a node index *)
            traceMaxPath
            (fn (i: int) =>
            case Array.sub (a, i) of
               SOME x => x
             | NONE =>
                  let
                     val x = Vector.sub (objectBytesAllocated, i)
                     val max =
                        List.fold
                        (Node.successors (indexNode i), Bytes.zero,
                         fn (e, max) =>
                         let
                            val i' = nodeIndex (Edge.to e)
                         in
                            if Array.sub (mayHaveCheck, i')
                               then max
                            else Bytes.max (max, maxPath i')
                         end)
                     val x = Bytes.+ (x, max)
                     val _ = Array.update (a, i, SOME x)
                  in
                     x
                  end
               ) arg
      end
      fun blockCheckAmount {blockIndex} =
         if Array.sub (mayHaveCheck, blockIndex)
            then maxPath blockIndex
         else Bytes.zero
      val f = insertFunction (f, handlesSignals, newFlag, blockCheckAmount,
                              maxPath o labelIndex)
      val _ =
         Control.diagnostics
         (fn display =>
          Vector.foreach
          (blocks, fn Block.T {label, ...} =>
           display (let open Layout
                    in seq [Label.layout label, str " ",
                            Bytes.layout (maxPath (labelIndex label))]
                    end)))
      val _ = Function.clear f
   in
      f
   end

fun transform (Program.T {functions, handlesSignals, main, objectTypes, profileInfo, statics}) =
   let
      fun tyconTy tycon =
         Vector.sub (objectTypes, ObjptrTycon.index tycon)

      val _ = Control.diagnostic (fn () => Layout.str "Limit Check maxPaths")

      val (newFlag, finishFlags) =
         let
            val flagsTycon = ObjptrTycon.new ()
            val flagsVar = Var.newString "flags"
            val flagsTy = Type.objptr flagsTycon
            val flags = Operand.Var {ty = flagsTy, var = flagsVar}

            val flagWS = WordSize.bool
            val flagScale = valOf (Scale.fromBytes (WordSize.bytes flagWS))
            val flagTy = Type.word flagWS

            val c = Counter.new 0
         in
            (fn () => Operand.SequenceOffset
                      {base = flags,
                       index = Operand.word (WordX.fromInt (Counter.next c,
                                                            WordSize.seqIndex ())),
                       offset = Bytes.zero,
                       scale = flagScale,
                       ty = flagTy},
             fn () =>
             if Counter.value c > 0
                then (ObjptrTycon.setIndex (flagsTycon, Vector.length objectTypes)
                      ; (Vector.concat
                         [objectTypes,
                          Vector.new1 (ObjectType.Sequence
                                       {components = Prod.new1Mutable flagTy,
                                        hasIdentity = true})],
                         Vector.concat
                         [statics,
                          Vector.new1 {dst = (flagsVar, flagsTy),
                                       obj = Object.Sequence
                                             {init = Vector.tabulate
                                                     (Counter.value c, fn _ =>
                                                      Vector.new1
                                                      {offset = Bytes.zero,
                                                       src = Operand.one flagWS}),
                                              tycon = flagsTycon}}]))
                else (objectTypes, statics))
         end

      datatype z = datatype Control.limitCheck
      fun insert f =
         case !Control.limitCheck of
            PerBlock => insertPerBlock (f, handlesSignals, newFlag, tyconTy)
          | _ => insertCoalesce (f, handlesSignals, newFlag, tyconTy)

      val main = insert main
      val functions = List.revMap (functions, insert)

      val (objectTypes, statics) = finishFlags ()
   in
      Program.T {functions = functions,
                 handlesSignals = handlesSignals,
                 main = main,
                 objectTypes = objectTypes,
                 profileInfo = profileInfo,
                 statics = statics}
   end

end
