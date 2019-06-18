(* Copyright (C) 2009,2013-2014,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Backend (S: BACKEND_STRUCTS): BACKEND =
struct

open S

structure M = Machine
local
   open Machine
in
   structure CFunction = CFunction
   structure Global = Global
   structure Label = Label
   structure Live = Live
   structure ObjptrTycon = ObjptrTycon
   structure RealX = RealX
   structure Register = Register
   structure Runtime = Runtime
   structure StackOffset = StackOffset
   structure WordSize = WordSize
   structure WordX = WordX
   structure WordXVector = WordXVector
end
local
   open Runtime
in
   structure GCField = GCField
end

structure R = Rssa
local
   open Rssa
in
   structure CType = CType
   structure Const = Const
   structure Func = Func
   structure Function = Function
   structure Prim = Prim
   structure Type = Type
   structure Var = Var
end 

structure AllocateRegisters = AllocateRegisters (structure Machine = Machine
                                                 structure Rssa = Rssa)
structure Chunkify = Chunkify (Rssa)
structure ParallelMove = ParallelMove ()

structure VarOperand =
   struct
      datatype t =
         Allocate of {operand: M.Operand.t option ref}
       | Const of M.Operand.t

      fun layout i =
         let
            open Layout
         in
            case i of
               Allocate {operand, ...} =>
                  seq [str "Allocate ",
                       record [("operand",
                                Option.layout M.Operand.layout (!operand))]]
             | Const oper => seq [str "Const ", M.Operand.layout oper]
         end

      val operand: t -> M.Operand.t option =
         fn Allocate {operand, ...} => !operand
          | Const oper => SOME oper
   end

structure ByteSet = UniqueSet (val cacheSize: int = 1
                               val bits: int = 14
                               structure Element =
                                  struct
                                     open Bytes
                                  end)

structure Chunk =
   struct
      datatype t = T of {blocks: M.Block.t list ref,
                         chunkLabel: M.ChunkLabel.t}

      fun label (T {chunkLabel, ...}) = chunkLabel

      fun new (): t =
         T {blocks = ref [],
            chunkLabel = M.ChunkLabel.newNoname ()}

      fun newBlock (T {blocks, ...}, z) =
         List.push (blocks, M.Block.T z)
   end

val traceGenBlock =
   Trace.trace ("Backend.genBlock",
                Label.layout o R.Block.label,
                Unit.layout)

fun eliminateDeadCode (f: R.Function.t): R.Function.t =
   let
      val {args, blocks, name, returns, raises, start} = R.Function.dest f
      val {get, rem, set, ...} =
         Property.getSetOnce (Label.plist, Property.initConst false)
      val get = Trace.trace ("Backend.labelIsReachable",
                             Label.layout,
                             Bool.layout) get
      val _ =
         R.Function.dfs (f, fn R.Block.T {label, ...} =>
                         (set (label, true)
                          ; fn () => ()))
      val blocks =
         Vector.keepAll (blocks, fn R.Block.T {label, ...} =>
                         let
                            val res = get label
                            val () = rem label
                         in
                            res
                         end)
   in
      R.Function.new {args = args,
                      blocks = blocks,
                      name = name,
                      returns = returns,
                      raises = raises,
                      start = start}
   end

fun toMachine (rssa: Rssa.Program.t) =
   let
      val R.Program.T {functions, handlesSignals, main, objectTypes, profileInfo} = rssa
      (* Chunk info *)
      val {get = labelChunk, set = setLabelChunk, ...} =
         Property.getSetOnce (Label.plist,
                              Property.initRaise ("labelChunk", Label.layout))
      val {get = funcChunk: Func.t -> Chunk.t, set = setFuncChunk, ...} =
         Property.getSetOnce (Func.plist,
                              Property.initRaise ("funcChunk", Func.layout))
      val chunks = ref []
      fun newChunk () =
         let
            val c = Chunk.new ()
            val _ = List.push (chunks, c)
         in
            c
         end
      (* Set funcChunk and labelChunk. *)
      val _ =
         Vector.foreach
         (Chunkify.chunkify rssa, fn {funcs, labels} =>
          let 
             val c = newChunk ()
             val _ = Vector.foreach (funcs, fn f => setFuncChunk (f, c))
             val _ = Vector.foreach (labels, fn l => setLabelChunk (l, c))
          in
             ()
          end)
      (* Profile info *)
      val (sourceMaps, getFrameSourceSeqIndex) =
         case profileInfo of
            NONE => (NONE, fn _ => NONE)
          | SOME {sourceMaps, getFrameSourceSeqIndex} => (SOME sourceMaps, getFrameSourceSeqIndex)
      (* Frame info *)
      local
         val frameInfos: M.FrameInfo.t list ref = ref []
         val frameInfosCounter = Counter.new 0
         val _ = ByteSet.reset ()
         val table =
            let
               fun equals ({kind = k1, frameOffsets = fo1, size = s1, sourceSeqIndex = ssi1},
                           {kind = k2, frameOffsets = fo2, size = s2, sourceSeqIndex = ssi2}) =
                  M.FrameInfo.Kind.equals (k1, k2)
                  andalso M.FrameOffsets.equals (fo1, fo2)
                  andalso Bytes.equals (s1, s2)
                  andalso Option.equals (ssi1, ssi2, Int.equals)
               fun hash {kind, frameOffsets, size, sourceSeqIndex} =
                  Hash.list [M.FrameInfo.Kind.hash kind,
                             M.FrameOffsets.hash frameOffsets,
                             Bytes.hash size,
                             Hash.optionMap (sourceSeqIndex, Word.fromInt)]
            in
               HashTable.new {equals = equals,
                              hash = hash}
            end
         val frameOffsets: M.FrameOffsets.t list ref = ref []
         val frameOffsetsCounter = Counter.new 0
         val {get = getFrameOffsets: ByteSet.t -> M.FrameOffsets.t, ...} =
            Property.get
            (ByteSet.plist,
             Property.initFun
             (fn offsets =>
              let
                 val index = Counter.next frameOffsetsCounter
                 val offsets =
                    QuickSort.sortVector
                    (Vector.fromList (ByteSet.toList offsets),
                     Bytes.<=)
                 val fo =
                    M.FrameOffsets.new {index = index, offsets = offsets}
                 val _ = List.push (frameOffsets, fo)
              in
                 fo
              end))
      in
         fun allFrameInfo chunks =
            let
               (* Reverse lists because the index is from back of list. *)
               val frameInfos = Vector.fromListRev (!frameInfos)
               val frameOffsets = Vector.fromListRev (!frameOffsets)
               (* If we are using the C or LLVM codegens, then we reindex the
                * frameInfos so that the indices of the entry frames of a chunk
                * are consecutive integers so that gcc will use a jump table.
                *)
               val frameInfos =
                  if !Control.codegen = Control.CCodegen orelse !Control.codegen = Control.LLVMCodegen
                     then let
                             val done =
                                Array.array (Vector.length frameInfos, false)
                             val newFrameInfos = ref []
                             fun newFrameInfo fi =
                                if Array.sub (done, M.FrameInfo.index fi)
                                   then ()
                                   else (Array.update (done, M.FrameInfo.index fi, true)
                                         ; List.push (newFrameInfos, fi))
                             val () =
                                List.foreach
                                (chunks, fn M.Chunk.T {blocks, ...} =>
                                 Vector.foreach
                                 (blocks, fn M.Block.T {kind, ...} =>
                                  case M.Kind.frameInfoOpt kind of
                                     NONE => ()
                                   | SOME fi => if M.Kind.isEntry kind
                                                   then newFrameInfo fi
                                                   else ()))
                             val () = Vector.foreach (frameInfos, newFrameInfo)
                             val frameInfos =
                                Vector.fromListRev (!newFrameInfos)
                             val () =
                                Vector.foreachi
                                (frameInfos, fn (i, fi) =>
                                 M.FrameInfo.setIndex (fi, i))
                          in
                             frameInfos
                          end
                     else frameInfos
            in
               (frameInfos, frameOffsets)
            end
         fun getFrameInfo {entry: bool,
                           kind: M.FrameInfo.Kind.t,
                           offsets: Bytes.t list,
                           size: Bytes.t,
                           sourceSeqIndex: int option}: M.FrameInfo.t =
            let
               val frameOffsets = getFrameOffsets (ByteSet.fromList offsets)
               fun new () =
                  let
                     val index = Counter.next frameInfosCounter
                     val frameInfo =
                        M.FrameInfo.new
                        {frameOffsets = frameOffsets,
                         index = index,
                         kind = kind,
                         size = size,
                         sourceSeqIndex = sourceSeqIndex}
                     val _ = List.push (frameInfos, frameInfo)
                  in
                     frameInfo
                  end
            in
               (* If we are using the C or LLVM codegens, then we want
                * each entry frame to have a different index, because
                * the index will be used for the trampoline
                * (nextChunks mapping and ChunkSwitch); moreover, we
                * want the indices of entry frames of a chunk to be
                * consecutive integers so that gcc will use a jump
                * table.
                *)
               if entry
                  andalso (!Control.codegen = Control.CCodegen
                           orelse !Control.codegen = Control.LLVMCodegen)
                  then new ()
                  else HashTable.lookupOrInsert
                       (table,
                        {frameOffsets = frameOffsets,
                         kind = kind,
                         size = size,
                         sourceSeqIndex = sourceSeqIndex},
                        fn () => new ())
            end
      end
      val {get = frameInfo: Label.t -> M.FrameInfo.t option,
           set = setFrameInfo, ...} = 
         Property.getSetOnce (Label.plist,
                              Property.initConst NONE)
      val setFrameInfo =
         Trace.trace2 ("Backend.setFrameInfo",
                       Label.layout, Option.layout M.FrameInfo.layout,
                       Unit.layout)
         setFrameInfo
      (* The global raise operands. *)
      local
         val table: (Type.t vector * M.Live.t vector) list ref = ref []
      in
         fun raiseOperands (ts: Type.t vector): M.Live.t vector =
            case List.peek (!table, fn (ts', _) =>
                            Vector.equals (ts, ts', Type.equals)) of
               NONE =>
                  let
                     val gs =
                        Vector.map (ts, fn ty =>
                                    M.Live.Global
                                    (Global.new {isRoot = false,
                                                 ty = ty}))
                     val _ = List.push (table, (ts, gs))
                  in
                     gs
                  end
             | SOME (_, gs) => gs
      end
      val {get = varInfo: Var.t -> {operand: VarOperand.t,
                                    ty: Type.t},
           set = setVarInfo, ...} =
         Property.getSetOnce (Var.plist,
                              Property.initRaise ("Backend.info", Var.layout))
      val setVarInfo =
         Trace.trace2 ("Backend.setVarInfo",
                       Var.layout, VarOperand.layout o #operand, Unit.layout)
         setVarInfo
      val varInfo =
         Trace.trace ("Backend.varInfo",
                      Var.layout,
                      fn {operand, ...} =>
                      Layout.record [("operand", VarOperand.layout operand)])
         varInfo
      val varOperandOpt: Var.t -> M.Operand.t option =
         VarOperand.operand o #operand o varInfo
      val varOperand: Var.t -> M.Operand.t = valOf o varOperandOpt
      val varOperand =
         Trace.trace ("Backend.varOperand",
                      Var.layout,
                      M.Operand.layout)
         varOperand
      (* Hash tables for uniquifying globals. *)
      local
         fun 'a make {equals: 'a * 'a -> bool,
                      hash: 'a -> word,
                      ty: 'a -> Type.t} =
            let
               val table: ('a, M.Global.t) HashTable.t =
                  HashTable.new {equals = equals, hash = hash}
               fun get (value: 'a): M.Operand.t =
                  M.Operand.Global
                  (HashTable.lookupOrInsert
                   (table, value, fn () =>
                    M.Global.new {isRoot = true,
                                  ty = ty value}))
               fun all () =
                  HashTable.fold
                  (table, [], fn ((value, global), ac) =>
                   (global, value) :: ac)
            in
               (all, get)
            end
      in
         val (allReals, globalReal) =
            make {equals = RealX.equals,
                  hash = RealX.hash,
                  ty = Type.real o RealX.size}
         val (allVectors, globalVector) =
            make {equals = WordXVector.equals,
                  hash = WordXVector.hash,
                  ty = Type.ofWordXVector}
      end
      fun bogusOp (t: Type.t): M.Operand.t =
         case Type.deReal t of
            NONE => let
                       val bogusWord =
                          M.Operand.Word
                          (WordX.zero
                           (WordSize.fromBits (Type.width t)))
                    in
                       case Type.deWord t of
                          NONE => M.Operand.Cast (bogusWord, t)
                        | SOME _ => bogusWord
                    end
          | SOME s => globalReal (RealX.zero s)
      fun constOperand (c: Const.t): M.Operand.t =
         let
            datatype z = datatype Const.t
         in
            case c of
               IntInf _ =>
                  Error.bug "Backend.constOperand: IntInf"
             | Null => M.Operand.Null
             | Real r => globalReal r
             | Word w => M.Operand.Word w
             | WordVector v => globalVector v
         end
      fun parallelMove {dsts: M.Operand.t vector,
                        srcs: M.Operand.t vector}: M.Statement.t vector =
         let
            val moves =
               Vector.fold2 (srcs, dsts, [],
                             fn (src, dst, ac) => {src = src, dst = dst} :: ac)
            fun temp r =
               M.Operand.Register (Register.new (M.Operand.ty r, NONE))
         in
            Vector.fromList
            (ParallelMove.move {
                                equals = M.Operand.equals,
                                move = M.Statement.move,
                                moves = moves,
                                interfere = M.Operand.interfere,
                                temp = temp
                                })
         end
      fun runtimeOp (field: GCField.t): M.Operand.t =
         case field of
            GCField.Frontier => M.Operand.Frontier
          | GCField.StackTop => M.Operand.StackTop
          | _ => 
               M.Operand.Offset {base = M.Operand.GCState,
                                 offset = GCField.offset field,
                                 ty = Type.ofGCField field}
      val exnStackOp = runtimeOp GCField.ExnStack
      val stackBottomOp = runtimeOp GCField.StackBottom
      val stackTopOp = runtimeOp GCField.StackTop
      fun translateOperand (oper: R.Operand.t): M.Operand.t =
         let
            datatype z = datatype R.Operand.t
         in
            case oper of
               Cast (z, t) => M.Operand.Cast (translateOperand z, t)
             | Const c => constOperand c
             | GCState => M.Operand.GCState
             | Offset {base, offset, ty} =>
                  let
                     val base = translateOperand base
                  in
                     if M.Operand.isLocation base
                        then M.Operand.Offset {base = base,
                                               offset = offset,
                                               ty = ty}
                     else bogusOp ty
                  end
             | ObjptrTycon opt =>
                  M.Operand.Word
                  (WordX.fromIntInf
                   (Word.toIntInf (Runtime.typeIndexToHeader
                                   (ObjptrTycon.index opt)),
                    WordSize.objptrHeader ()))
             | Runtime f => runtimeOp f
             | SequenceOffset {base, index, offset, scale, ty} =>
                  let
                     val base = translateOperand base
                  in
                     if M.Operand.isLocation base
                        then M.Operand.SequenceOffset {base = base,
                                                        index = translateOperand index,
                                                        offset = offset,
                                                        scale = scale,
                                                        ty = ty}
                     else bogusOp ty
                  end
             | Var {var, ...} => varOperand var
         end
      fun translateOperands ops = Vector.map (ops, translateOperand)
      fun genStatement (s: R.Statement.t,
                        handlerLinkOffset: {handler: Bytes.t,
                                            link: Bytes.t} option)
         : M.Statement.t vector =
         let
            fun handlerOffset () = #handler (valOf handlerLinkOffset)
            fun linkOffset () = #link (valOf handlerLinkOffset)
            datatype z = datatype R.Statement.t
         in
            case s of
               Bind {dst = (var, _), src, ...} =>
                  Vector.new1
                  (M.Statement.move {dst = varOperand var,
                                     src = translateOperand src})
             | Move {dst, src} =>
                  Vector.new1
                  (M.Statement.move {dst = translateOperand dst,
                                     src = translateOperand src})
             | Object {dst, header, size} =>
                  M.Statement.object {dst = varOperand (#1 dst),
                                      header = header,
                                      size = size}
             | PrimApp {dst, prim, args} =>
                  let
                     datatype z = datatype Prim.Name.t
                  in
                     case Prim.name prim of
                        MLton_touch => Vector.new0 ()
                      | _ => 
                           Vector.new1
                           (M.Statement.PrimApp
                            {args = translateOperands args,
                             dst = Option.map (dst, varOperand o #1),
                             prim = prim})
                  end
             | ProfileLabel s => Vector.new1 (M.Statement.ProfileLabel s)
             | SetExnStackLocal =>
                  (* ExnStack = stackTop + (offset + LABEL_SIZE) - StackBottom; *)
                  let
                     val tmp1 =
                        M.Operand.Register
                        (Register.new (Type.cpointer (), NONE))
                     val tmp2 =
                        M.Operand.Register
                        (Register.new (Type.csize (), NONE))
                  in
                     Vector.new3
                     (M.Statement.PrimApp
                      {args = (Vector.new2
                               (stackTopOp,
                                M.Operand.Word
                                (WordX.fromIntInf
                                 (Int.toIntInf
                                  (Bytes.toInt
                                   (Bytes.+ (handlerOffset (), Runtime.labelSize ()))),
                                  WordSize.cpointer ())))),
                       dst = SOME tmp1,
                       prim = Prim.cpointerAdd},
                      M.Statement.PrimApp
                      {args = Vector.new2 (tmp1, stackBottomOp),
                       dst = SOME tmp2,
                       prim = Prim.cpointerDiff},
                      M.Statement.move
                      {dst = exnStackOp,
                       src = M.Operand.Cast (tmp2, Type.exnStack ())})
                  end
             | SetExnStackSlot =>
                  (* ExnStack = *(uint* )(stackTop + offset); *)
                  Vector.new1
                  (M.Statement.move
                   {dst = exnStackOp,
                    src = M.Operand.stackOffset {offset = linkOffset (),
                                                 ty = Type.exnStack ()}})
             | SetHandler h =>
                  Vector.new1
                  (M.Statement.move
                   {dst = M.Operand.stackOffset {offset = handlerOffset (),
                                                 ty = Type.label h},
                    src = M.Operand.Label h})
             | SetSlotExnStack =>
                  (* *(uint* )(stackTop + offset) = ExnStack; *)
                  Vector.new1
                  (M.Statement.move
                   {dst = M.Operand.stackOffset {offset = linkOffset (),
                                                 ty = Type.exnStack ()},
                    src = exnStackOp})
             | _ => Error.bug (concat
                               ["Backend.genStatement: strange statement: ",
                                R.Statement.toString s])
         end
      val genStatement =
         Trace.trace ("Backend.genStatement",
                      R.Statement.layout o #1, Vector.layout M.Statement.layout)
         genStatement
      val bugTransfer = fn () =>
         M.Transfer.CCall
         {args = (Vector.new1
                  (globalVector
                   (WordXVector.fromString
                    "backend thought control shouldn't reach here"))),
          func = Type.BuiltInCFunction.bug (),
          return = NONE}
      val {get = labelInfo: Label.t -> {args: (Var.t * Type.t) vector},
           set = setLabelInfo, ...} =
         Property.getSetOnce
         (Label.plist, Property.initRaise ("labelInfo", Label.layout))
      val setLabelInfo =
         Trace.trace2 ("Backend.setLabelInfo",
                       Label.layout, Layout.ignore, Unit.layout)
         setLabelInfo
      fun paramOffsets (xs: 'a vector, ty: 'a -> Type.t,
                        mk: {offset: Bytes.t, ty: Type.t} -> 'b): 'b vector =
         #1 (Vector.mapAndFold
             (xs, Bytes.zero,
              fn (x, offset) =>
              let
                 val ty = ty x
                 val offset = Type.align (ty, offset)
              in
                 (mk {offset = offset, ty = ty},
                  Bytes.+ (offset, Type.bytes ty))
              end))
      fun paramStackOffsets (xs: 'a vector, ty: 'a -> Type.t,
                             shift: Bytes.t): StackOffset.t vector =
         paramOffsets (xs, ty, fn {offset, ty} =>
                       StackOffset.T {offset = Bytes.+ (offset, shift),
                                      ty = ty})
      val operandLive: M.Operand.t -> M.Live.t =
         valOf o M.Live.fromOperand
      val operandsLive: M.Operand.t vector -> M.Live.t vector =
         fn ops => Vector.map (ops, operandLive)
      val isGlobal =
         let
            val {get: Var.t -> bool, set, rem, ...} =
               Property.getSet
               (Var.plist,
                Property.initRaise ("Backend.toMachine.isGlobal", Var.layout))
            val _ =
               Function.foreachDef (main, fn (x, _) => set (x, false))
            val _ =
               List.foreach
               (functions, fn f =>
                (Function.foreachUse (f, fn x => set (x, true))
                 ; Function.foreachDef (f, fn (x, _) => rem x)))
         in
            get
         end
      fun genFunc (f: Function.t, isMain: bool): unit =
         let
            val f = eliminateDeadCode f
            val {args, blocks, name, raises, returns, start, ...} =
               Function.dest f
            val raises = Option.map (raises, fn ts => raiseOperands ts)
            val returns =
               Option.map (returns, fn ts =>
                           paramStackOffsets (ts, fn t => t, Bytes.zero))
            fun newVarInfo (x, ty: Type.t) =
               let
                  val operand =
                     if isMain andalso isGlobal x
                        then let
                                val _ =
                                   Control.diagnostics
                                   (fn display =>
                                    let
                                       open Layout
                                    in
                                       display (seq
                                                [str "Global: ",
                                                 R.Var.layout x,
                                                 str ": ",
                                                 R.Type.layout ty])
                                    end)
                             in
                                VarOperand.Const (M.Operand.Global
                                                  (M.Global.new {isRoot = true,
                                                                 ty = ty}))
                             end
                     else VarOperand.Allocate {operand = ref NONE}
               in
                  setVarInfo (x, {operand = operand,
                                  ty = ty})
               end
            fun newVarInfos xts = Vector.foreach (xts, newVarInfo)
            (* Set the constant operands, labelInfo, and varInfo. *)
            val _ = newVarInfos args
            val _ =
               Rssa.Function.dfs
               (f, fn R.Block.T {args, label, statements, ...} =>
                let
                   val _ = setLabelInfo (label, {args = args})
                   val _ = newVarInfos args
                   val _ =
                      Vector.foreach
                      (statements, fn s =>
                       let
                          fun normal () = R.Statement.foreachDef (s, newVarInfo)
                       in
                          case s of
                             R.Statement.Bind {dst = (var, _), isMutable, src} =>
                                if isMutable
                                   then normal ()
                                else
                                   let
                                      fun set (z: M.Operand.t,
                                               casts: Type.t list) =
                                         let
                                            val z =
                                               List.fold
                                               (casts, z, fn (t, z) =>
                                                M.Operand.Cast (z, t))
                                         in
                                            setVarInfo
                                            (var, {operand = VarOperand.Const z,
                                                   ty = M.Operand.ty z})
                                         end
                                      fun loop (z: R.Operand.t, casts) =
                                         case z of
                                            R.Operand.Cast (z, t) =>
                                               loop (z, t :: casts)
                                          | R.Operand.Const c =>
                                               set (constOperand c, casts)
                                          | R.Operand.Var {var = var', ...} =>
                                               (case #operand (varInfo var') of
                                                   VarOperand.Const z =>
                                                      set (z, casts)
                                                 | VarOperand.Allocate _ =>
                                                      normal ())
                                          | _ => normal ()
                                   in
                                      loop (src, [])
                                   end
                           | _ => normal ()
                       end)
                in
                   fn () => ()
                end)
            (* Allocate stack slots. *)
            local
               val varInfo =
                  fn x =>
                  let
                     val {operand, ty, ...} = varInfo x
                  in
                     {operand = (case operand of
                                    VarOperand.Allocate {operand, ...} =>
                                       SOME operand
                                  | _ => NONE),
                      ty = ty}
                  end
            in
               val {handlerLinkOffset, labelInfo = labelRegInfo, ...} =
                  let
                     val paramOffsets = fn args =>
                        paramOffsets (args, fn (_, ty) => ty, fn so => so)
                  in
                     AllocateRegisters.allocate {function = f,
                                                 paramOffsets = paramOffsets,
                                                 varInfo = varInfo}
                  end
            end
            (* Set the frameInfo for blocks in this function. *)
            val _ =
               Vector.foreach
               (blocks, fn R.Block.T {kind, label, ...} =>
                let
                   fun doit (useOffsets: bool): unit =
                      let
                         val {liveNoFormals, size, ...} = labelRegInfo label
                         val offsets =
                            if useOffsets
                               then
                                  Vector.fold
                                  (liveNoFormals, [], fn (oper, ac) =>
                                   case oper of
                                      M.Operand.StackOffset (StackOffset.T {offset, ty}) =>
                                         if Type.isObjptr ty
                                            then offset :: ac
                                         else ac
                                    | _ => ac)
                            else
                               []
                         val (entry, kind) =
                            case kind of
                               R.Kind.Cont _=> (true, M.FrameInfo.Kind.ML_FRAME)
                             | R.Kind.CReturn {func} => (CFunction.maySwitchThreadsTo func,
                                                         M.FrameInfo.Kind.C_FRAME)
                             | R.Kind.Handler => (true, M.FrameInfo.Kind.ML_FRAME)
                             | R.Kind.Jump => (false, M.FrameInfo.Kind.ML_FRAME)
                         val frameInfo =
                            getFrameInfo {entry = entry,
                                          kind = kind,
                                          offsets = offsets,
                                          size = size,
                                          sourceSeqIndex = getFrameSourceSeqIndex label}
                      in
                         setFrameInfo (label, SOME frameInfo)
                      end
                in
                   case R.Kind.frameStyle kind of
                      R.Kind.None => ()
                    | R.Kind.OffsetsAndSize => doit true
                    | R.Kind.SizeOnly => doit false
                end)
            (* ------------------------------------------------- *)
            (*                    genTransfer                    *)
            (* ------------------------------------------------- *)
            fun genTransfer (t: R.Transfer.t)
               : M.Statement.t vector * M.Transfer.t =
               let
                  fun simple t = (Vector.new0 (), t)
               in
                  case t of
                     R.Transfer.CCall {args, func, return} =>
                        let
                           val return =
                              case return of
                                 NONE => NONE
                               | SOME return =>
                                    let
                                       val fio = frameInfo return
                                       val {size, ...} = labelRegInfo return
                                    in
                                        SOME {return = return,
                                              size = Option.map (fio, fn _ => size)}
                                    end
                        in
                           simple (M.Transfer.CCall
                                   {args = translateOperands args,
                                    func = func,
                                    return = return})
                        end
                   | R.Transfer.Call {func, args, return} =>
                        let
                           datatype z = datatype R.Return.t
                           val (contLive, frameSize, return) =
                              case return of
                                 Dead => (Vector.new0 (), Bytes.zero, NONE)
                               | Tail => (Vector.new0 (), Bytes.zero, NONE)
                               | NonTail {cont, handler} =>
                                    let
                                       val {liveNoFormals, size, ...} =
                                          labelRegInfo cont
                                       datatype z = datatype R.Handler.t
                                       val handler =
                                          case handler of
                                             Caller => NONE
                                           | Dead => NONE
                                           | Handle h => SOME h
                                    in
                                       (liveNoFormals,
                                        size, 
                                        SOME {return = cont,
                                              handler = handler,
                                              size = size})
                                    end
                           val dsts =
                              paramStackOffsets
                              (args, R.Operand.ty, frameSize)
                           val setupArgs =
                              parallelMove
                              {dsts = Vector.map (dsts, M.Operand.StackOffset),
                               srcs = translateOperands args}
                           val live =
                              Vector.concat [operandsLive contLive,
                                             Vector.map (dsts, Live.StackOffset)]
                           val transfer =
                              M.Transfer.Call {label = funcToLabel func,
                                               live = live,
                                               return = return}
                        in
                           (setupArgs, transfer)
                        end
                   | R.Transfer.Goto {dst, args} =>
                        let
                           val (dsts', srcs') =
                              Vector.unzip
                              (Vector.keepAllMap2
                               (#args (labelInfo dst), args, fn ((dst, _), src) =>
                                case varOperandOpt dst of
                                   NONE => NONE
                                 | SOME dst => SOME (dst, translateOperand src)))
                        in
                           (parallelMove {srcs = srcs', dsts = dsts'},
                            M.Transfer.Goto dst)
                        end
                   | R.Transfer.Raise srcs =>
                        (M.Statement.moves {dsts = Vector.map (valOf raises,
                                                               Live.toOperand),
                                            srcs = translateOperands srcs},
                         M.Transfer.Raise)
                   | R.Transfer.Return xs =>
                        (parallelMove {dsts = Vector.map (valOf returns,
                                                          M.Operand.StackOffset),
                                       srcs = translateOperands xs},
                         M.Transfer.Return)
                   | R.Transfer.Switch switch =>
                        let
                           val R.Switch.T {cases, default, size, test} =
                              switch
                        in
                           simple
                           (case (Vector.length cases, default) of
                               (0, NONE) => bugTransfer ()
                             | (1, NONE) =>
                                  M.Transfer.Goto (#2 (Vector.sub (cases, 0)))
                             | (0, SOME dst) => M.Transfer.Goto dst
                             | _ =>
                                  M.Transfer.Switch
                                  (M.Switch.T
                                   {cases = cases,
                                    default = default,
                                    size = size,
                                    test = translateOperand test}))
                        end
               end
            val genTransfer =
               Trace.trace ("Backend.genTransfer",
                            R.Transfer.layout,
                            Layout.tuple2 (Vector.layout M.Statement.layout,
                                           M.Transfer.layout))
               genTransfer
            fun genBlock (R.Block.T {args, kind, label, statements, transfer,
                                     ...}) : unit =
               let
                  val {live, liveNoFormals, size, ...} = labelRegInfo label
                  val statements =
                     Vector.concatV
                     (Vector.map (statements, fn s =>
                                  genStatement (s, handlerLinkOffset)))
                  val (preTransfer, transfer) = genTransfer transfer
                  val (kind, live, pre) =
                     case kind of
                        R.Kind.Cont _ =>
                           let
                              val srcs = paramStackOffsets (args, #2, size)
                              val (dsts', srcs') =
                                 Vector.unzip
                                 (Vector.keepAllMap2
                                  (args, srcs, fn ((dst, _), src) =>
                                   case varOperandOpt dst of
                                      NONE => NONE
                                    | SOME dst => SOME (dst, M.Operand.StackOffset src)))
                           in
                              (M.Kind.Cont {args = Vector.map (srcs, Live.StackOffset),
                                            frameInfo = valOf (frameInfo label)},
                               liveNoFormals,
                               parallelMove {dsts = dsts', srcs = srcs'})
                           end
                      | R.Kind.CReturn {func, ...} =>
                           let
                              val dst =
                                 case Vector.length args of
                                    0 => NONE
                                  | 1 => Option.map
                                         (varOperandOpt (#1 (Vector.first args)),
                                          operandLive)
                                  | _ => Error.bug "Backend.genBlock: CReturn"
                           in
                              (M.Kind.CReturn {dst = dst,
                                               frameInfo = frameInfo label,
                                               func = func},
                               liveNoFormals,
                               Vector.new0 ())
                           end
                      | R.Kind.Handler =>
                           let
                              val handles = raiseOperands (Vector.map (args, #2))
                              val (dsts', srcs') =
                                 Vector.unzip
                                 (Vector.keepAllMap2
                                  (args, handles, fn ((dst, _), h) =>
                                   case varOperandOpt dst of
                                      NONE => NONE
                                    | SOME dst =>SOME (dst, Live.toOperand h)))
                           in
                              (M.Kind.Handler
                               {frameInfo = valOf (frameInfo label),
                                handles = handles},
                               liveNoFormals,
                               M.Statement.moves {dsts = dsts', srcs = srcs'})
                           end
                      | R.Kind.Jump => (M.Kind.Jump, live, Vector.new0 ())
                  val (first, statements) =
                     if !Control.profile = Control.ProfileTimeLabel
                        then
                           case (if Vector.isEmpty statements
                                    then NONE
                                 else (case Vector.first statements of
                                          s as M.Statement.ProfileLabel _ =>
                                             SOME s
                                        | _ => NONE)) of
                              NONE =>
                                 Error.bug
                                 (concat ["Backend.genBlock: ",
                                          "missing ProfileLabel in ",
                                          Label.toString label])
                            | SOME s =>
                                 (Vector.new1 s,
                                  Vector.dropPrefix (statements, 1))
                     else (Vector.new0 (), statements)
                  val statements =
                     Vector.concat [first, pre, statements, preTransfer]
                  val returns =
                     Option.map (returns, fn returns =>
                                 Vector.map (returns, Live.StackOffset))
               in
                  Chunk.newBlock (labelChunk label,
                                  {kind = kind,
                                   label = label,
                                   live = operandsLive live,
                                   raises = raises,
                                   returns = returns,
                                   statements = statements,
                                   transfer = transfer})
               end
            val genBlock = traceGenBlock genBlock
            val _ = Vector.foreach (blocks, genBlock)
            val _ =
               let
                  val returns =
                     Option.map
                     (returns, fn returns =>
                      Vector.map (returns, Live.StackOffset))
                  val frameInfo =
                     getFrameInfo {entry = true,
                                   kind = M.FrameInfo.Kind.ML_FRAME,
                                   offsets = [],
                                   size = Bytes.zero,
                                   sourceSeqIndex = NONE}
                  val srcs =
                     paramStackOffsets
                     (args, #2, Bytes.zero)
                  val srcs =
                     Vector.map (srcs, M.Operand.StackOffset)
                  val statements =
                     parallelMove
                     {dsts = Vector.map (args, varOperand o #1),
                      srcs = srcs}
               in
                  Chunk.newBlock
                  (funcChunk name,
                   {label = funcToLabel name,
                    kind = M.Kind.Func {frameInfo = frameInfo},
                    live = operandsLive srcs,
                    raises = raises,
                    returns = returns,
                    statements = statements,
                    transfer = M.Transfer.Goto start})
               end
            val _ =
               if isMain
                  then ()
               else Vector.foreach (blocks, R.Block.clear)
         in
            ()
         end
      val genFunc =
         Trace.trace2 ("Backend.genFunc",
                       Func.layout o Function.name, Bool.layout, Unit.layout)
         genFunc
      (* Generate the main function first.
       * Need to do this in order to set globals.
       *)
      val _ = genFunc (main, true)
      val _ = List.foreach (functions, fn f => genFunc (f, false))
      val chunks = !chunks
      fun chunkToMachine (Chunk.T {chunkLabel, blocks}) =
         let
            val blocks = Vector.fromList (!blocks)
            val regMax = CType.memo (fn _ => ref ~1)
            val regsNeedingIndex =
               Vector.fold
               (blocks, [], fn (b, ac) =>
                M.Block.foldDefs
                (b, ac, fn (z, ac) =>
                 case z of
                    M.Operand.Register r =>
                       (case Register.indexOpt r of
                           NONE => r :: ac
                         | SOME i =>
                              let
                                 val z = regMax (Type.toCType (Register.ty r))
                                 val _ =
                                    if i > !z
                                       then z := i
                                    else ()
                              in
                                 ac
                              end)
                  | _ => ac))
            val _ =
               List.foreach
               (regsNeedingIndex, fn r =>
                let
                   val z = regMax (Type.toCType (Register.ty r))
                   val i = 1 + !z
                   val _ = z := i
                   val _ = Register.setIndex (r, i)
                in
                   ()
                end)
         in
            M.Chunk.T {chunkLabel = chunkLabel,
                       blocks = blocks,
                       regMax = ! o regMax}
         end
      val mainName = R.Function.name main
      val main = {chunkLabel = Chunk.label (funcChunk mainName),
                  label = funcToLabel mainName}
      val chunks = List.revMap (chunks, chunkToMachine)
      (* The clear is necessary because properties have been attached to Funcs
       * and Labels, and they appear as labels in the resulting program.
       *)
      val _ = List.foreach (chunks, fn M.Chunk.T {blocks, ...} =>
                            Vector.foreach (blocks, Label.clear o M.Block.label))
      val (frameInfos, frameOffsets) = allFrameInfo chunks
      val maxFrameSize: Bytes.t =
         List.fold
         (chunks, Bytes.zero, fn (M.Chunk.T {blocks, ...}, max) =>
          Vector.fold
          (blocks, max, fn (M.Block.T {kind, statements, transfer, ...}, max) =>
           let
              fun doOperand (z: M.Operand.t, max: Bytes.t): Bytes.t =
                 let
                    datatype z = datatype M.Operand.t
                 in
                    case z of
                       SequenceOffset {base, index, ...} =>
                          doOperand (base, doOperand (index, max))
                     | Cast (z, _) => doOperand (z, max)
                     | Contents {oper, ...} => doOperand (oper, max)
                     | Offset {base, ...} => doOperand (base, max)
                     | StackOffset (StackOffset.T {offset, ty}) =>
                          Bytes.max (Bytes.+ (offset, Type.bytes ty), max)
                     | _ => max
                 end
              val max =
                 case M.Kind.frameInfoOpt kind of
                    NONE => max
                  | SOME fi => Bytes.max (max, M.FrameInfo.size fi)
              val max =
                 Vector.fold
                 (statements, max, fn (s, max) =>
                  M.Statement.foldOperands (s, max, doOperand))
              val max =
                 M.Transfer.foldOperands (transfer, max, doOperand)
           in
              max
           end))
      val maxFrameSize = Bytes.alignWord32 maxFrameSize
      val machine =
         M.Program.T
         {chunks = chunks,
          frameInfos = frameInfos,
          frameOffsets = frameOffsets,
          handlesSignals = handlesSignals,
          main = main,
          maxFrameSize = maxFrameSize,
          objectTypes = objectTypes,
          reals = allReals (),
          sourceMaps = sourceMaps,
          vectors = allVectors ()}
   in
      machine
   end
end
