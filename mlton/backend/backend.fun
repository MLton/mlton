(* Copyright (C) 2009,2013-2014,2017,2019-2022 Matthew Fluet.
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
   structure Label = Label
   structure Live = Live
   structure ObjptrTycon = ObjptrTycon
   structure RealX = RealX
   structure Runtime = Runtime
   structure StackOffset = StackOffset
   structure StaticHeap = StaticHeap
   structure Temporary = Temporary
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
   structure Object = Object
   structure ObjectType = ObjectType
   structure Prim = Prim
   structure Prod = Prod
   structure Type = Type
   structure Var = Var
end 

structure AllocateVariables = AllocateVariables (structure Machine = Machine
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
      val R.Program.T {functions, handlesSignals, main, objectTypes, profileInfo, statics} = rssa
      (* tyconTy *)
      fun tyconTy tycon =
         Vector.sub (objectTypes, ObjptrTycon.index tycon)
      (* returnsTo and raisesTo info *)
      val rflow = R.Program.rflow rssa
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
         val nextFrameInfo = Counter.generator 0
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
         val nextFrameOffset = Counter.generator 0
         val {get = getFrameOffsets: ByteSet.t -> M.FrameOffsets.t, ...} =
            Property.get
            (ByteSet.plist,
             Property.initFun
             (fn offsets =>
              let
                 val index = nextFrameOffset ()
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
                     val index = nextFrameInfo ()
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

      val (addToStaticHeaps, finishStaticHeaps, allGlobalObjptrs) =
         let
            open StaticHeap
            val allGlobalObjptrs = ref []
            fun varElem (x: Var.t): Elem.t * bool =
               case varOperand x of
                  M.Operand.Const c => (Elem.Const c, false)
                | M.Operand.Global g =>
                     (case List.peek (!allGlobalObjptrs, fn (_, g') =>
                                      M.Global.equals (g, g')) of
                         SOME (r, _) => (Elem.Ref r, true)
                       | NONE => Error.bug "Backend.staticHeaps.varElem: invalid operand,Global")
                | M.Operand.StaticHeapRef r => (Elem.Ref r,
                                                Kind.isDynamic (Ref.kind r))
                | _ => Error.bug "Backend.staticHeaps.varElem: invalid operand"
            fun translateOperand (oper: R.Operand.t): Elem.t * bool =
               case oper of
                  R.Operand.Cast (z, ty) =>
                     let
                        val (z, hasDynamic) = translateOperand z
                     in
                        (Elem.Cast (z, ty), hasDynamic)
                     end
                | R.Operand.Const c => (Elem.Const c, false)
                | R.Operand.Var {var, ...} => varElem var
                | _ => Error.bug "Backend.staticHeaps.translateOperand: invalid operand"
            fun translateInit init =
               Vector.mapAndFold
               (init, false, fn ({offset, src}, hasDynamic') =>
                let
                   val (src, hasDynamic) = translateOperand src
                in
                   ({offset = offset, src = src},
                    hasDynamic' orelse hasDynamic)
                end)
            fun translateObject obj =
               case obj of
                  R.Object.Normal {init, tycon} =>
                     let
                        val {components, ...} = ObjectType.deNormal (tyconTy tycon)
                        val (init, hasDynamic) = translateInit init
                        val kind =
                           if hasDynamic
                              then Kind.Dynamic
                              else if Prod.someIsMutable components
                                      then if Vector.exists (Prod.dest components,
                                                             fn {elt, isMutable} =>
                                                             isMutable andalso Type.isObjptr elt)
                                              then (* Reference to root static heap
                                                    * won't map to valid card slot.
                                                    *)
                                                   if !Control.markCards
                                                      then Kind.Dynamic
                                                      else Kind.Root
                                              else Kind.Mutable
                              else Kind.Immutable
                     in
                        {kind = kind,
                         obj = Object.Normal {init = init,
                                              tycon = tycon},
                         offset = Runtime.normalMetaDataSize (),
                         size = R.Object.size (obj, {tyconTy = tyconTy}),
                         tycon = tycon}
                     end
                | R.Object.Sequence {init, tycon} =>
                     let
                        val {components, hasIdentity} = ObjectType.deSequence (tyconTy tycon)
                        val (init, hasDynamic) =
                           Vector.mapAndFold
                           (init, false, fn (init, hasDynamic') =>
                            let
                               val (init, hasDynamic) = translateInit init
                            in
                               (init, hasDynamic' orelse hasDynamic)
                            end)
                        val kind =
                           if hasDynamic
                              then Kind.Dynamic
                              else if hasIdentity
                                      then if Vector.isEmpty init
                                              then (* An empty sequence;
                                                    * elements will never be updated,
                                                    * but header may be updated.
                                                    *)
                                                   Kind.Mutable
                                              else if Vector.exists (Prod.dest components,
                                                                     fn {elt, isMutable} =>
                                                                     isMutable andalso Type.isObjptr elt)
                                                      then (* Reference to root static heap
                                                            * won't map to valid card slot.
                                                            *)
                                                           if !Control.markCards
                                                              then Kind.Dynamic
                                                              else Kind.Root
                                                      else Kind.Mutable
                              else Kind.Immutable
                     in
                        {kind = kind,
                         obj = Object.Sequence {init = init,
                                                tycon = tycon},
                         offset = Runtime.sequenceMetaDataSize (),
                         size = R.Object.size (obj, {tyconTy = tyconTy}),
                         tycon = tycon}
                     end

            val kindAcc = Kind.memoize (fn _ =>
                                        {objs = ref [],
                                         nextIndex = Counter.generator 0,
                                         nextOffset = ref Bytes.zero})

            fun add obj =
               let
                  val {kind, obj, offset, size, tycon} =
                     translateObject obj
                  val {objs, nextIndex, nextOffset} = kindAcc kind
                  val r = Ref.T {index = nextIndex (),
                                 kind = kind,
                                 offset = Bytes.+ (!nextOffset, offset),
                                 ty = Type.objptr tycon}
                  val oper =
                     case kind of
                        Kind.Dynamic =>
                           let
                              val g = M.Global.new (Type.objptr tycon)
                           in
                              List.push (allGlobalObjptrs, (r, g))
                              ; M.Operand.Global g
                           end
                      | _ => M.Operand.StaticHeapRef r
               in
                  List.push (objs, obj)
                  ; nextOffset := Bytes.+ (!nextOffset, size)
                  ; oper
               end

            fun finish () =
               Kind.memoize (Vector.fromListRev o ! o #objs o kindAcc)
         in
            (add, finish, fn () => !allGlobalObjptrs)
         end

      val () = Vector.foreach (statics, fn {dst = (dstVar, dstTy), obj} =>
                               let
                                  val oper = addToStaticHeaps obj
                               in
                                  setVarInfo (dstVar, {operand = VarOperand.Const oper, ty = dstTy})
                               end)

      (* Hash tables for uniquifying globals. *)
      local
         fun 'a make {equals: 'a * 'a -> bool,
                      hash: 'a -> word,
                      oper: 'a -> M.Operand.t} =
            let
               val table: ('a, M.Operand.t) HashTable.t =
                  HashTable.new {equals = equals, hash = hash}
               fun get (value: 'a): M.Operand.t =
                  HashTable.lookupOrInsert
                  (table, value, fn () => oper value)
            in
               get
            end
      in
         local
            val allGlobalReals = ref []
         in
            val globalReal =
               make {equals = RealX.equals,
                     hash = RealX.hash,
                     oper = fn r => let
                                       val g = M.Global.new (Type.real (RealX.size r))
                                    in
                                       List.push (allGlobalReals, (r, g))
                                       ; M.Operand.Global g
                                    end}
            val allGlobalReals = fn () => !allGlobalReals
         end

         val globalWordVector =
            make {equals = WordXVector.equals,
                  hash = WordXVector.hash,
                  oper = addToStaticHeaps o R.Object.fromWordXVector}
      end
      fun constOperand (c: Const.t): M.Operand.t =
         let
            datatype z = datatype Const.t
         in
            case c of
               IntInf _ =>
                  Error.bug "Backend.constOperand: IntInf"
             | Real r => globalReal r
             | WordVector v => globalWordVector v
             | _ => M.Operand.Const c
         end
      fun parallelMove {dsts: M.Operand.t vector,
                        srcs: M.Operand.t vector}: M.Statement.t vector =
         let
            val moves =
               Vector.fold2 (srcs, dsts, [],
                             fn (src, dst, ac) => {src = src, dst = dst} :: ac)
            fun temp t =
               M.Operand.Temporary (Temporary.new (M.Operand.ty t, NONE))
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
          | _ => M.Operand.gcField field
      val exnStackOp = runtimeOp GCField.ExnStack
      val stackBottomOp = runtimeOp GCField.StackBottom
      val stackTopOp = runtimeOp GCField.StackTop

      val rec isWord =
         fn M.Operand.Const (Const.Word _) => true
          | M.Operand.Cast (z, _) => isWord z
          | _ => false
      fun bogusOp (t: Type.t): M.Operand.t =
         case Type.deReal t of
            NONE => let
                       val bogusWord =
                          M.Operand.word
                          (WordX.zero
                           (WordSize.fromBits (Type.width t)))
                    in
                       case Type.deWord t of
                          NONE => M.Operand.Cast (bogusWord, t)
                        | SOME _ => bogusWord
                    end
          | SOME s => globalReal (RealX.zero s)
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
                    (* Native codegens can't handle this;
                     * Dead code may treat small constant
                     * intInfs as large and take offsets *)
                     if isWord base
                     then bogusOp ty
                     else M.Operand.Offset {base = base,
                                            offset = offset,
                                            ty = ty,
                                            volatile = false}
                  end
             | ObjptrTycon opt =>
                  M.Operand.word (ObjptrTycon.toHeader opt)
             | Runtime f => runtimeOp f
             | SequenceOffset {base, index, offset, scale, ty} =>
                  let
                     val base = translateOperand base
                  in
                     if isWord base
                     then bogusOp ty
                     else M.Operand.SequenceOffset
                              {base = base,
                               index = translateOperand index,
                               offset = offset,
                               scale = scale,
                               ty = ty}
                  end
             | Var {var, ...} => varOperand var
         end
      fun translateOperands ops = Vector.map (ops, translateOperand)
      fun genStatement (s: R.Statement.t,
                        handlersInfo: {handlerOffset: Bytes.t,
                                       linkOffset: Bytes.t} option)
         : M.Statement.t vector =
         let
            fun handlerOffset () = #handlerOffset (valOf handlersInfo)
            fun linkOffset () = #linkOffset (valOf handlersInfo)
            datatype z = datatype R.Statement.t
            fun move arg =
               case M.Statement.move arg of
                  NONE => Vector.new0 ()
                | SOME move => Vector.new1 move
            fun mkInit (init, mkDst) =
               Vector.toListMap
               (init, fn {src, offset} =>
                move {dst = mkDst {offset = offset,
                                   ty = R.Operand.ty src},
                      src = translateOperand src})
         in
            case s of
               Bind {dst = (var, _), src, ...} =>
                  (* CHECK *)
                  let
                     val oper = varOperand var
                  in
                     if M.Operand.isDestination oper
                        then move {dst = oper,
                                   src = translateOperand src}
                        else Vector.new0 () (* Destination already propagated *)
                  end
             | Move {dst, src} =>
                  move {dst = translateOperand dst,
                        src = translateOperand src}
             | Object {dst = (dst, _), obj as Object.Normal {init, tycon}} =>
                  let
                     val dst = varOperand dst
                     val header = ObjptrTycon.toHeader tycon
                     fun mkDst {offset, ty} =
                        M.Operand.Offset {base = dst,
                                          offset = offset,
                                          ty = ty,
                                          volatile = false}
                  in
                     Vector.concat
                     (M.Statement.object {dst = dst,
                                          header = header,
                                          size = Object.size (obj, {tyconTy = tyconTy})}
                      :: mkInit (init, mkDst))
                  end
             | Object {dst = (dst, _), obj as Object.Sequence {init, tycon}} =>
                  let
                     val dst = varOperand dst
                     val header = ObjptrTycon.toHeader tycon
                     val elt = ObjectType.componentsSize (tyconTy tycon)
                     val (scale, mkIndex) =
                        case Scale.fromBytes elt of
                           NONE =>
                              (Scale.One, fn index =>
                               M.Operand.word
                               (WordX.mul
                                (WordX.fromInt (index, WordSize.seqIndex ()),
                                 WordX.fromBytes (elt, WordSize.seqIndex ()),
                                 {signed = false})))
                         | SOME s =>
                              (s, fn index =>
                               M.Operand.word
                               (WordX.fromInt (index, WordSize.seqIndex ())))
                  in
                     Vector.concat
                     (M.Statement.sequence {dst = dst,
                                            header = header,
                                            length = Vector.length init,
                                            size = Object.size (obj, {tyconTy = tyconTy})}
                      :: (List.concat o Vector.toListMapi)
                         (init, fn (index, init) =>
                          let
                             fun mkDst {offset, ty} =
                                M.Operand.SequenceOffset
                                {base = dst,
                                 index = mkIndex index,
                                 offset = offset,
                                 scale = scale,
                                 ty = ty}
                          in
                             mkInit (init, mkDst)
                          end))
                  end
             | PrimApp {dst, prim, args} =>
                  (case prim of
                      Prim.MLton_touch => Vector.new0 ()
                    | _ =>
                         Vector.new1
                         (M.Statement.PrimApp
                          {args = translateOperands args,
                           dst = Option.map (dst, varOperand o #1),
                           prim = prim}))
             | SetExnStackLocal =>
                  (* ExnStack = stackTop + (handlerOffset + LABEL_SIZE) - StackBottom; *)
                  let
                     val tmp =
                        M.Operand.Temporary
                        (Temporary.new (Type.cpointer (), NONE))
                  in
                     Vector.new2
                     (M.Statement.PrimApp
                      {args = (Vector.new2
                               (stackTopOp,
                                M.Operand.word
                                (WordX.fromBytes
                                 (Bytes.+ (handlerOffset (), Runtime.labelSize ()),
                                  WordSize.cptrdiff ())))),
                       dst = SOME tmp,
                       prim = Prim.CPointer_add},
                      M.Statement.PrimApp
                      {args = Vector.new2 (tmp, stackBottomOp),
                       dst = SOME exnStackOp,
                       prim = Prim.CPointer_diff})
                  end
             | SetExnStackSlot =>
                  (* ExnStack = *(ptrdiff_t* )(stackTop + linkOffset); *)
                  move
                  {dst = exnStackOp,
                   src = M.Operand.stackOffset {offset = linkOffset (),
                                                ty = Type.exnStack (),
                                                volatile = false}}
             | SetHandler h =>
                  (* *(uintptr_t)(stackTop + handlerOffset) = h; *)
                  move
                  {dst = M.Operand.stackOffset {offset = handlerOffset (),
                                                ty = Type.label h,
                                                volatile = false},
                   src = M.Operand.Label h}
             | SetSlotExnStack =>
                  (* *(ptrdiff_t* )(stackTop + linkOffset) = ExnStack; *)
                  move
                  {dst = M.Operand.stackOffset {offset = linkOffset (),
                                                ty = Type.exnStack (),
                                                volatile = false},
                   src = exnStackOp}
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
                  (globalWordVector
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
                        mk: {offset: Bytes.t, ty: Type.t, volatile: bool} -> 'b): 'b vector =
         #1 (Vector.mapAndFold
             (xs, Bytes.zero,
              fn (x, offset) =>
              let
                 val ty = ty x
                 val offset = Type.align (ty, offset)
              in
                 (mk {offset = offset, ty = ty, volatile = false},
                  Bytes.+ (offset, Type.bytes ty))
              end))
      fun paramStackOffsets (xs: 'a vector, ty: 'a -> Type.t,
                             shift: Bytes.t): StackOffset.t vector =
         paramOffsets (xs, ty, fn {offset, ty, volatile} =>
                       StackOffset.T {offset = Bytes.+ (offset, shift),
                                      ty = ty,
                                      volatile = volatile})
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
            val {raisesTo, returnsTo} = rflow name
            val (returnLives, returnOperands) =
               case returns of
                  NONE => (NONE, NONE)
                | SOME returns =>
                     let
                        val returnStackOffsets =
                           paramStackOffsets (returns, fn t => t, Bytes.zero)
                     in
                        (SOME (Vector.map (returnStackOffsets, M.Live.StackOffset)),
                         SOME (Vector.map (returnStackOffsets, M.Operand.StackOffset)))
                     end
            val raiseLives =
               case raises of
                  NONE => NONE
                | SOME _ => SOME (Vector.new0 ())
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
                                VarOperand.Const (M.Operand.Global (M.Global.new ty))
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
                             R.Statement.Bind {dst = (var, _), src, ...} =>
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
               val {handlersInfo, labelInfo = labelRegInfo, ...} =
                  let
                     val paramOffsets = fn args =>
                        paramOffsets (args, fn (_, ty) => ty, fn so => so)
                  in
                     AllocateVariables.allocate {function = f,
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
                                      M.Operand.StackOffset (StackOffset.T {offset, ty, ...}) =>
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
                           (parallelMove {dsts = dsts', srcs = srcs'},
                            M.Transfer.Goto dst)
                        end
                   | R.Transfer.Raise srcs =>
                        let
                           val handlerStackTop =
                              M.Operand.Temporary
                              (Temporary.new (Type.cpointer (), NONE))
                           val dsts =
                              paramOffsets
                              (srcs, R.Operand.ty, fn {offset, ty, volatile} =>
                               M.Operand.Offset {base = handlerStackTop,
                                                 offset = offset,
                                                 ty = ty,
                                                 volatile = volatile})
                        in
                           if Vector.isEmpty srcs
                              then (Vector.new0 (), M.Transfer.Raise {raisesTo = raisesTo})
                              else (Vector.concat
                                    [Vector.new1
                                     (M.Statement.PrimApp
                                      {args = Vector.new2 (stackBottomOp, exnStackOp),
                                       dst = SOME handlerStackTop,
                                       prim = Prim.CPointer_add}),
                                     parallelMove {dsts = dsts,
                                                   srcs = translateOperands srcs}],
                                    M.Transfer.Raise {raisesTo = raisesTo})
                        end
                   | R.Transfer.Return xs =>
                        (parallelMove {dsts = valOf returnOperands,
                                       srcs = translateOperands xs},
                         M.Transfer.Return {returnsTo = returnsTo})
                   | R.Transfer.Switch switch =>
                        let
                           val R.Switch.T {cases, default, expect, size, test} =
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
                                    expect = expect,
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
                                  genStatement (s, handlersInfo)))
                  val (preTransfer, transfer) = genTransfer transfer
                  fun doContHandler mkMachineKind =
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
                        (mkMachineKind {args = Vector.map (srcs, Live.StackOffset),
                                        frameInfo = valOf (frameInfo label)},
                         liveNoFormals,
                         parallelMove {dsts = dsts', srcs = srcs'})
                     end
                  val (kind, live, pre) =
                     case kind of
                        R.Kind.Cont _ => doContHandler M.Kind.Cont
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
                      | R.Kind.Handler => doContHandler M.Kind.Handler
                      | R.Kind.Jump => (M.Kind.Jump, live, Vector.new0 ())
                  val statements =
                     Vector.concat [pre, statements, preTransfer]
               in
                  Chunk.newBlock (labelChunk label,
                                  {kind = kind,
                                   label = label,
                                   live = operandsLive live,
                                   raises = raiseLives,
                                   returns = returnLives,
                                   statements = statements,
                                   transfer = transfer})
               end
            val genBlock = traceGenBlock genBlock
            val _ = Vector.foreach (blocks, genBlock)
            val _ =
               let
                  val frameInfo =
                     getFrameInfo {entry = true,
                                   kind = M.FrameInfo.Kind.ML_FRAME,
                                   offsets = [],
                                   size = Bytes.zero,
                                   sourceSeqIndex = NONE}
                  val srcs =
                     paramStackOffsets (args, #2, Bytes.zero)
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
                    raises = raiseLives,
                    returns = returnLives,
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
            val tempsMax = CType.memo (fn _ => ref ~1)
            val tempsNeedingIndex =
               Vector.fold
               (blocks, [], fn (b, ac) =>
                M.Block.foldDefs
                (b, ac, fn (z, ac) =>
                 case z of
                    M.Operand.Temporary t =>
                       (case Temporary.indexOpt t of
                           NONE => t :: ac
                         | SOME i =>
                              let
                                 val z = tempsMax (Type.toCType (Temporary.ty t))
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
               (tempsNeedingIndex, fn t =>
                let
                   val z = tempsMax (Type.toCType (Temporary.ty t))
                   val i = 1 + !z
                   val _ = z := i
                   val _ = Temporary.setIndex (t, i)
                in
                   ()
                end)
         in
            M.Chunk.T {chunkLabel = chunkLabel,
                       blocks = blocks,
                       tempsMax = ! o tempsMax}
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
                     | Offset {base, ...} => doOperand (base, max)
                     | StackOffset (StackOffset.T {offset, ty, ...}) =>
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
          globals = {objptrs = allGlobalObjptrs (),
                     reals = allGlobalReals ()},
          handlesSignals = handlesSignals,
          main = main,
          maxFrameSize = maxFrameSize,
          objectTypes = objectTypes,
          sourceMaps = sourceMaps,
          staticHeaps = finishStaticHeaps ()}
   in
      machine
   end
end
