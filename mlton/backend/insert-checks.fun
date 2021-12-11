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
functor InsertChecks (S: RSSA_TRANSFORM_STRUCTS): RSSA_TRANSFORM =
struct

open S

structure Control =
   struct
      open Control

      structure LimitCheck =
         struct
            datatype t =
               PerBlock
             | ExtBasicBlocks
             | LoopHeaders of {fullCFG: bool,
                               loopExits: bool}
         end
      datatype limitCheck = datatype LimitCheck.t

      val limitCheck =
         ref (LoopHeaders {fullCFG = false,
                           loopExits = true})
   end

structure CFunction =
   struct
      open CFunction Type.BuiltInCFunction
   end

structure BytesAllocated =
   struct
      datatype t =
         Dynamic of Operand.t
       | Static of Bytes.t

      (*
      fun layout ba =
         let
            open Layout
         in
            case ba of
               Dynamic oper => seq [str "Dynamic ", Operand.layout oper]
             | Static bytes => seq [str "Static ", Bytes.layout bytes]
         end
      val toString = Layout.toString o layout
      *)

      val zero = Static Bytes.zero

      fun isBig ba =
         case ba of
            Dynamic _ => true
          | Static bytes =>
               (* 512 is small and arbitrary *)
               if Bytes.<= (bytes, Bytes.fromInt 512)
                  then false
                  else true
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

      fun bytesAllocated (t: t): BytesAllocated.t =
         case t of
            CCall {args, func, ...} =>
               (case CFunction.bytesNeeded func of
                   NONE => BytesAllocated.zero
                 | SOME i =>
                      let
                         val arg = Vector.sub (args, i)
                      in
                         case arg of
                            Operand.Const c =>
                               (case c of
                                   Const.Word w =>
                                      BytesAllocated.Static
                                      (Bytes.fromIntInf (WordX.toIntInf w))
                                 | _ => Error.bug "LimitCheck.Transfer.bytesAllocated: strange bytesNeeded argument")
                          | _ => BytesAllocated.Dynamic arg
                      end)
          | _ => BytesAllocated.zero
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
             BytesAllocated.Dynamic _ => Bytes.zero
           | BytesAllocated.Static b => b)
   end

fun insertFunction (f: Function.t,
                    handlesSignals: bool,
                    newFlag: unit -> Operand.t,
                    blockLimitCheckAmount: Label.t -> Bytes.t,
                    ensureFree: Label.t -> Bytes.t,
                    needsSignalCheck: Label.t -> bool) =
   let
      val {args, blocks, name, raises, returns, start} = Function.dest f
      val newBlocks = ref []
      fun newBlock b = List.push (newBlocks, b)
      local
         val r: Label.t option ref = ref NONE
      in
         fun heapCheckTooLarge () =
            case !r of
               SOME l => l
             | NONE =>
                  let
                     val l = Label.newString "heapCheckTooLarge"
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
                        newBlock
                        (Block.T {args = Vector.new0 (),
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
      fun gotoHeapCheckTooLarge () =
         Transfer.Goto {args = Vector.new0 (), dst = heapCheckTooLarge ()}
      val _ =
         Vector.foreach
         (blocks, fn Block.T {args, kind, label, statements, transfer} =>
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
             fun mkCollect (amount: Operand.t (* of type word *)) =
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
                                  newBlock
                                  (Block.T
                                   {args = Vector.new0 (),
                                    kind = Kind.Jump,
                                    label = dontCollect,
                                    statements = Vector.new0 (),
                                    transfer =
                                    Transfer.ifBoolE
                                    (flag,
                                     NONE,
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
             fun primAppIf' (addPrefix, prim, arg1, arg2, expect, {falsee, truee}) =
                let
                   val res = Var.newNoname ()
                   val s =
                      Statement.PrimApp {args = Vector.new2 (arg1, arg2),
                                         dst = SOME (res, Type.bool),
                                         prim = prim}
                   val transfer =
                      Transfer.ifBoolE
                      (Operand.Var {var = res, ty = Type.bool},
                       expect,
                       {falsee = falsee,
                        truee = truee})
                in
                   (addPrefix s, transfer)
                end
             fun primAppIf (prim, arg1, arg2, expect, falsee_truee) =
                primAppIf' (Vector.new1, prim, arg1, arg2, expect, falsee_truee)
             datatype z = datatype Runtime.GCField.t
             datatype limitCheck = Zero | Small | Large of Operand.t

             val needsStackCheck = Label.equals (start, label)
             val needsSignalCheck = needsSignalCheck label
             val signalCheckAtLimitCheck =
                !Control.signalCheckAtLimitCheck andalso handlesSignals

             fun mkSmallLimitCheck {collect, nextCheck} =
                let
                   val limitCheck = Label.newString "smallLimitCheck"
                   val (statements, transfer) =
                      primAppIf (Prim.CPointer_lt,
                                 Operand.Runtime Limit,
                                 Operand.Runtime Frontier,
                                 !Control.limitCheckExpect,
                                 {truee = collect,
                                  falsee = nextCheck})
                   val _ =
                      newBlock
                      (Block.T {label = limitCheck,
                                kind = Kind.Jump,
                                args = Vector.new0 (),
                                statements = statements,
                                transfer = transfer})
                in
                   limitCheck
                end
             fun mkLargeLimitCheck (amount, {collect, nextCheck}) =
                let
                   val limitCheck = Label.newString "largeLimitCheck"
                   val avail = Var.newNoname ()
                   val calcAvail =
                      Statement.PrimApp
                      {args = Vector.new2 (Operand.Runtime LimitPlusSlop,
                                           Operand.Runtime Frontier),
                       dst = SOME (avail, Type.csize ()),
                       prim = Prim.CPointer_diff}
                   val (statements, transfer) =
                      primAppIf'
                      (fn cmp => Vector.new2 (calcAvail, cmp),
                       Prim.Word_lt (WordSize.csize (), {signed = false}),
                       Operand.Var {var = avail, ty = Type.csize ()},
                       amount,
                       !Control.limitCheckExpect,
                       {truee = collect,
                        falsee = nextCheck})
                   val _ =
                      newBlock
                      (Block.T {label = limitCheck,
                                kind = Kind.Jump,
                                args = Vector.new0 (),
                                statements = statements,
                                transfer = transfer})
                in
                   limitCheck
                end
             fun mkLimitCheck (limitCheck, collect_nextCheck as {nextCheck, ...}) =
                case limitCheck of
                   Zero => nextCheck
                 | Small => mkSmallLimitCheck collect_nextCheck
                 | Large oper => mkLargeLimitCheck (oper, collect_nextCheck)
             fun mkSignalCheck (limitCheck, collect_nextCheck as {collect, nextCheck}) =
                case limitCheck of
                   Zero =>
                      if needsSignalCheck
                         then (* force a limitCheck as signalCheck *)
                              mkSmallLimitCheck collect_nextCheck
                         else nextCheck
                 | Small =>
                      (* signalCheck handled by Small limitCheck *)
                      nextCheck
                 | Large _ =>
                      if needsSignalCheck
                         orelse signalCheckAtLimitCheck
                         then let
                                 val signalCheck = Label.newString "signalCheck"
                                 val (statements, transfer) =
                                    primAppIf (Prim.CPointer_equal,
                                               Operand.Runtime Limit,
                                               Operand.null,
                                               !Control.signalCheckExpect,
                                               {truee = collect,
                                                falsee = nextCheck})
                                 val _ =
                                    newBlock
                                    (Block.T {label = signalCheck,
                                              kind = Kind.Jump,
                                              args = Vector.new0 (),
                                              statements = statements,
                                              transfer = transfer})
                              in
                                 signalCheck
                              end
                         else nextCheck
             fun mkStackCheck {collect, nextCheck} =
                if needsStackCheck
                   then let
                           val stackCheck = Label.newString "stackCheck"
                           val (statements, transfer) =
                              primAppIf (Prim.CPointer_lt,
                                         Operand.Runtime StackLimit,
                                         Operand.Runtime StackTop,
                                         !Control.stackCheckExpect,
                                         {truee = collect,
                                          falsee = nextCheck})
                           val _ =
                              newBlock
                              (Block.T {label = stackCheck,
                                        kind = Kind.Jump,
                                        args = Vector.new0 (),
                                        statements = statements,
                                        transfer = transfer})
                        in
                           stackCheck
                        end
                   else nextCheck
             fun mkChecks limitCheck =
                let
                   val {collect, dontCollect} =
                      mkCollect (case limitCheck of
                                    Zero => Operand.zero (WordSize.csize())
                                  | Small => Operand.zero (WordSize.csize())
                                  | Large oper => oper)
                   val nextCheck = dontCollect
                   val nextCheck =
                      mkSignalCheck (limitCheck,
                                     {collect = collect,
                                      nextCheck = nextCheck})
                   val nextCheck =
                      mkLimitCheck (limitCheck,
                                    {collect = collect,
                                     nextCheck = nextCheck})
                   val nextCheck =
                      mkStackCheck {collect = collect,
                                    nextCheck = nextCheck}
                in
                   nextCheck
                end
             fun gotoChecks limitCheck =
                Transfer.Goto {args = Vector.new0 (), dst = mkChecks limitCheck}
             fun bigAllocation (blockBytesNeeded, transferBytesNeeded) =
                if WordX.isZero blockBytesNeeded
                   then (Vector.new0 (),
                         gotoChecks (Large transferBytesNeeded))
                   else let
                           val bytesNeededVar = Var.newNoname ()
                           val bytesNeededTy = Type.csize ()
                           val bytesNeededDst = (bytesNeededVar, bytesNeededTy)
                           val bytesNeededOper =
                              Operand.Var {var = bytesNeededVar,
                                           ty = bytesNeededTy}
                           val overflowVar = Var.newNoname ()
                           val overflowTy = Type.bool
                           val overflowDst = (overflowVar, overflowTy)
                           val overflowOper =
                              Operand.Var {var = overflowVar,
                                           ty = overflowTy}
                        in
                           (Vector.new2
                            (Statement.PrimApp
                             {args = Vector.new2 (Operand.word blockBytesNeeded,
                                                  transferBytesNeeded),
                              dst = SOME bytesNeededDst,
                              prim = Prim.Word_add (WordSize.csize ())},
                             Statement.PrimApp
                             {args = Vector.new2 (Operand.word blockBytesNeeded,
                                                  transferBytesNeeded),
                              dst = SOME overflowDst,
                              prim = Prim.Word_addCheckP (WordSize.csize (),
                                                          {signed = false})}),
                            Transfer.ifBoolE
                            (overflowOper,
                             NONE,
                             {falsee = mkChecks (Large bytesNeededOper),
                              truee = heapCheckTooLarge ()}))
                        end
             val (statements, transfer) =
                Exn.withEscape
                (fn escape =>
                 let
                    val blockBytesNeeded = blockLimitCheckAmount label
                    val blockBytesNeededAsCSize =
                       WordX.fromBytes (blockBytesNeeded, WordSize.csize ())
                       handle Overflow => escape (Vector.new0 (), gotoHeapCheckTooLarge ())
                 in
                    case Transfer.bytesAllocated transfer of
                       BytesAllocated.Dynamic transferBytesNeeded =>
                          bigAllocation (blockBytesNeededAsCSize, transferBytesNeeded)
                     | BytesAllocated.Static _ =>
                          let
                             val limitCheck =
                                if Bytes.isZero blockBytesNeeded
                                   then Zero
                                else if Bytes.<= (blockBytesNeeded, Runtime.limitSlop)
                                   then Small
                                else Large (Operand.word blockBytesNeededAsCSize)
                          in
                             case (limitCheck, needsSignalCheck, needsStackCheck) of
                                (Zero, false, false) => (statements, transfer)
                              | _ => (Vector.new0 (), gotoChecks limitCheck)
                          end
                 end)
          in
             newBlock
             (Block.T {args = args,
                       kind = kind,
                       label = label,
                       statements = statements,
                       transfer = transfer})
          end)
      val f =
         Function.new {args = args,
                       blocks = Vector.fromList (!newBlocks),
                       name = name,
                       raises = raises,
                       returns = returns,
                       start = start}
      val _ = Function.clear f
   in
      f
   end

fun limitCheckPerBlock (f: Function.t, tyconTy) =
   let
      val {blocks, ...} = Function.dest f
      val {get = blockLimitCheckAmount, set = setBlockLimitCheckAmount, ...} =
         Property.getSetOnce
         (Label.plist,
          Property.initRaise ("LimitCheck.insertPerBlock.blockCheckLimitAmount", Label.layout))
      val _ =
         Vector.foreach
         (blocks, fn block =>
          setBlockLimitCheckAmount
          (Block.label block,
           Block.objectBytesAllocated
           (block, {tyconTy = tyconTy})))
   in
      (blockLimitCheckAmount, fn _ => Bytes.zero)
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
      fun newBlock b = List.push (newBlocks, b)
      val () =
         Vector.foreach
         (blocks,
          fn block as Block.T {args, kind, label, statements, transfer} =>
          if BytesAllocated.isBig (Transfer.bytesAllocated transfer)
             then let
                     val l = Label.newNoname ()
                  in
                     newBlock (Block.T {args = args,
                                        kind = kind,
                                        label = label,
                                        statements = statements,
                                        transfer = Transfer.Goto {args = Vector.new0 (),
                                                                  dst = l}})
                     ; newBlock (Block.T {args = Vector.new0 (),
                                          kind = Kind.Jump,
                                          label = l,
                                          statements = Vector.new0 (),
                                          transfer = transfer})
                  end
             else newBlock block)
      val blocks = Vector.fromListRev (!newBlocks)
   in
      Function.new {args = args,
                    blocks = blocks,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun limitCheckCoalesce (f: Function.t, tyconTy) =
   let
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
             val isBigAlloc = BytesAllocated.isBig (Transfer.bytesAllocated transfer)
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
      fun blockLimitCheckAmount blockIndex =
         if Array.sub (mayHaveCheck, blockIndex)
            then maxPath blockIndex
         else Bytes.zero
      val blockLimitCheckAmount = blockLimitCheckAmount o labelIndex
      val ensureFree = maxPath o labelIndex
      val _ =
         Control.diagnostics
         (fn display =>
          (display (Layout.str "Limit Check maxPaths")
           ; Vector.foreach
             (blocks, fn Block.T {label, ...} =>
              display (let open Layout
                       in seq [Label.layout label, str " ",
                               Bytes.layout (maxPath (labelIndex label))]
                       end))))
   in
     (blockLimitCheckAmount, ensureFree)
   end

fun signalCheck (f: Function.t) =
   let
      val {blocks, start, ...} = Function.dest f
      val {get = labelIndex: Label.t -> int, set = setLabelIndex, ...} =
         Property.getSetOnce
         (Label.plist, Property.initRaise ("index", Label.layout))
      val _ =
         Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
                          setLabelIndex (label, i))
      val g = Graph.new ()
      val n = Vector.length blocks
      val {get = nodeIndex: unit Node.t -> int, set = setNodeIndex, ...} =
         Property.getSetOnce
         (Node.plist, Property.initRaise ("index", Node.layout))
      val nodes =
         Vector.tabulate (n, fn i =>
                          let
                             val n = Graph.newNode g
                             val _ = setNodeIndex (n, i)
                          in
                             n
                          end)
      fun indexNode i = Vector.sub (nodes, i)
      val labelNode = indexNode o labelIndex
      val _ =
         Vector.foreachi
         (blocks, fn (i, Block.T {transfer, ...}) =>
          let
             val from = indexNode i
          in
             if (case transfer of
                    Transfer.CCall {func, ...} =>
                       CFunction.maySwitchThreadsFrom func
                  | _ => false)
                then ()
             else
                Transfer.foreachLabel
                (transfer, fn to =>
                 (ignore o Graph.addEdge)
                 (g, {from = from, to = labelNode to}))
          end)
      val needsSignalCheck = Array.new (n, false)
      (* Add a signal check at each loop header. *)
      fun loop (f: int Forest.t) =
         let
            val {loops, ...} = Forest.dest f
         in
            Vector.foreach
            (loops, fn {headers, child} =>
             let
                val _ =
                   Vector.foreach
                   (headers, fn i =>
                    Array.update (needsSignalCheck, i, true))
             in
                loop child
             end)
         end
      val () = loop (Graph.loopForestSteensgaard
                     (g, {root = labelNode start, nodeValue = nodeIndex}))
      (* Add a signal check at the function entry. *)
      val _ = Array.update (needsSignalCheck, labelIndex start, true)
   in
      fn l => Array.sub (needsSignalCheck, labelIndex l)
   end

fun transform (Program.T {functions, handlesSignals, main, objectTypes, profileInfo, statics}) =
   let
      fun tyconTy tycon =
         Vector.sub (objectTypes, ObjptrTycon.index tycon)

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

      val insertFunction = fn f =>
         let
            datatype z = datatype Control.LimitCheck.t
            val (f, (blockLimitCheckAmount, ensureFree)) =
               case !Control.limitCheck of
                  PerBlock => (f, limitCheckPerBlock (f, tyconTy))
                | _ => let
                          val f = isolateBigTransfers f
                       in
                          (f, limitCheckCoalesce (f, tyconTy))
                       end
            val needsSignalCheck =
               if (case !Control.signalCheck of
                      Control.SignalCheck.Always => true
                    | Control.SignalCheck.IfHandlesSignals => handlesSignals)
                  then signalCheck f
                  else fn (_: Label.t) => false
         in
            insertFunction (f, handlesSignals, newFlag,
                            blockLimitCheckAmount, ensureFree,
                            needsSignalCheck)
         end

      val main = insertFunction main
      val functions = List.revMap (functions, insertFunction)

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
