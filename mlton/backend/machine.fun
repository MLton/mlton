(* Copyright (C) 2009,2014,2016-2017,2019-2023 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Machine (S: MACHINE_STRUCTS): MACHINE =
struct

open S

structure ChunkLabel = Id (val noname = "Chunk")

structure Global =
   struct
      datatype t = T of {index: int,
                         ty: Type.t}

      fun layout (T {index, ty, ...}) =
         let
            open Layout
         in
            seq [str (concat ["G", Type.name ty]),
                 paren (Int.layout index),
                 str ": ",
                 Type.layout ty]
         end

      local
         fun make f (T r) = f r
      in
         val index = make #index
         val ty = make #ty
      end

      val memo = CType.memo (fn _ => Counter.new 0)
      fun numberOfType t = Counter.value (memo t)

      fun new ty =
         T {index = Counter.next (memo (Type.toCType ty)),
            ty = ty}

      fun equals (T {index = i, ty},
                  T {index = i', ty = ty'}) =
         i = i'
         andalso Type.equals (ty, ty')

      val isSubtype: t * t -> bool =
         fn (T {index = i, ty},
             T {index = i', ty = ty'}) =>
         i = i'
         andalso Type.isSubtype (ty, ty')
         andalso CType.equals (Type.toCType ty, Type.toCType ty')
   end

structure StackOffset =
   struct
      datatype t = T of {offset: Bytes.t,
                         ty: Type.t,
                         volatile: bool}

      local
         fun make f (T r) = f r
      in
         val ty = make #ty
      end

      fun layout (T {offset, ty, volatile}): Layout.t =
         let
            open Layout
         in
            seq [str (concat ["S", if volatile then "V" else "", Type.name ty]),
                 paren (Bytes.layout offset),
                 str ": ", Type.layout ty]
         end

      val equals: t * t -> bool =
         fn (T {offset = b, ty, ...}, T {offset = b', ty = ty', ...}) =>
         Bytes.equals (b, b') andalso Type.equals (ty, ty')

      val isSubtype: t * t -> bool =
         fn (T {offset = b, ty = t, ...}, T {offset = b', ty = t', ...}) =>
         Bytes.equals (b, b') andalso Type.isSubtype (t, t')

      val interfere: t * t -> bool =
         fn (T {offset = b, ty = ty, ...}, T {offset = b', ty = ty', ...}) =>
         let
            val max = Bytes.+ (b, Type.bytes ty)
            val max' = Bytes.+ (b', Type.bytes ty')
         in
            Bytes.> (max, b') andalso Bytes.> (max', b)
         end

      fun shift (T {offset, ty, volatile}, size): t =
         T {offset = Bytes.- (offset, size),
            ty = ty,
            volatile = volatile}
   end

structure StaticHeap =
   struct
      structure Kind =
         struct
            datatype t = Dynamic | Immutable | Mutable | Root

            val all = [Immutable, Mutable, Root, Dynamic]

            val isDynamic = fn Dynamic => true | _ => false

            fun equals (k1, k2) =
               case (k1, k2) of
                  (Dynamic, Dynamic) => true
                | (Immutable, Immutable) => true
                | (Mutable, Mutable) => true
                | (Root, Root) => true
                | _ => false

            fun toString k =
               case k of
                  Dynamic => "dynamic"
                | Immutable => "immutable"
                | Mutable => "mutable"
                | Root => "root"
            val layout = Layout.str o toString

            fun name k =
               case k of
                  Dynamic => "D"
                | Immutable => "I"
                | Mutable => "M"
                | Root => "R"

            fun memoize f =
               let
                  val dyn = f Dynamic
                  val imm = f Immutable
                  val mut = f Mutable
                  val root = f Root
               in
                  fn Dynamic => dyn
                   | Immutable => imm
                   | Mutable => mut
                   | Root => root
               end

            val label = memoize (fn k => Label.fromString (concat ["staticHeap", name k]))
         end

      structure Ref =
         struct
            datatype t = T of {index: int,
                               kind: Kind.t,
                               offset: Bytes.t,
                               ty: Type.t}

            local
               fun mk sel (T r) = sel r
            in
               val index = mk #index
               val kind = mk #kind
               val offset = mk #offset
               val ty = mk #ty
            end

            fun equals (T {index = index1, kind = kind1, offset = offset1, ...},
                        T {index = index2, kind = kind2, offset = offset2, ...}) =
               Int.equals (index1, index2)
               andalso Kind.equals (kind1, kind2)
               andalso Bytes.equals (offset1, offset2)

            fun layout (T {index, kind, offset, ty}) =
               let
                  open Layout
               in
                  seq [str (concat ["H", Kind.name kind]),
                       tuple [Int.layout index, Bytes.layout offset],
                       str ": ",
                       Type.layout ty]
               end
         end

      structure Elem =
         struct
            datatype t =
               Cast of t * Type.t
             | Const of Const.t
             | Ref of Ref.t

            fun ty e =
               case e of
                  Cast (_, ty) => ty
                | Const c => Type.ofConst c
                | Ref r => Ref.ty r

            fun layout e =
               let
                  open Layout
               in
                  case e of
                     Cast (z, ty) =>
                        seq [str "Cast ", tuple [layout z, Type.layout ty]]
                   | Const c => Const.layout c
                   | Ref r => Ref.layout r
               end

            val word = Const o Const.word
            val deWord =
               fn Const (Const.Word w) => SOME w
                | _ => NONE
         end

      structure Object = Object (open S
                                 structure Use = Elem)
   end

structure Temporary =
   struct
      datatype t = T of {index: int option ref,
                         ty: Type.t}

      local
         fun make f (T r) = f r
      in
         val indexOpt = ! o (make #index)
         val ty = make #ty
      end

      fun layout (T {index, ty, ...}) =
         let
            open Layout
         in
            seq [str (concat ["T", Type.name ty]),
                 paren (case !index of
                           NONE => str "NONE"
                         | SOME i => Int.layout i),
                 str ": ",
                 Type.layout ty]
         end

      val toString = Layout.toString o layout

      fun index (r as T {index, ...}) =
         case !index of
            NONE =>
               Error.bug (concat ["Machine.Temporary: temporary ",
                                  toString r, " missing index"])
          | SOME i => i

      fun setIndex (r as T {index, ...}, i) =
         case !index of
            NONE => index := SOME i
          | SOME _ =>
               Error.bug (concat ["Machine.Temporary: temporary ",
                                  toString r, " index already set"])

      fun new (ty, i) = T {index = ref i,
                           ty = ty}

      fun equals (r, r') =
         (case (indexOpt r, indexOpt r') of
             (SOME i, SOME i') => i = i'
           | _ => false)
         andalso CType.equals (Type.toCType (ty r), Type.toCType (ty r'))

      val equals =
         Trace.trace2 ("Machine.Temporary.equals", layout, layout, Bool.layout) equals

      val isSubtype: t * t -> bool =
         fn (T {index = i, ty = t}, T {index = i', ty = t'}) =>
         (case (!i, !i') of
             (SOME i, SOME i') => i = i'
           | _ => false)
         andalso Type.isSubtype (t, t')
         andalso CType.equals (Type.toCType t, Type.toCType t')
   end

structure Operand =
   struct
      datatype t =
         Cast of t * Type.t
       | Const of Const.t
       | Frontier
       | GCState
       | Global of Global.t
       | Label of Label.t
       | Offset of {base: t,
                    offset: Bytes.t,
                    ty: Type.t,
                    volatile: bool}
       | SequenceOffset of {base: t,
                            index: t,
                            offset: Bytes.t,
                            scale: Scale.t,
                            ty: Type.t,
                            volatile: bool}
       | StackOffset of StackOffset.t
       | StackTop
       | StaticHeapRef of StaticHeap.Ref.t
       | Temporary of Temporary.t

    val word = Const o Const.Word

    val zero = word o WordX.zero

    val ty =
       fn Cast (_, ty) => ty
        | Const c => Type.ofConst c
        | Frontier => Type.cpointer ()
        | GCState => Type.gcState ()
        | Global g => Global.ty g
        | Label l => Type.label l
        | Offset {ty, ...} => ty
        | SequenceOffset {ty, ...} => ty
        | StackOffset s => StackOffset.ty s
        | StackTop => Type.cpointer ()
        | StaticHeapRef h => StaticHeap.Ref.ty h
        | Temporary t => Temporary.ty t

    fun layout (z: t): Layout.t =
         let
            open Layout 
            fun constrain (ty: Type.t): Layout.t =
               if !Control.showTypes
                  then seq [str ": ", Type.layout ty]
               else empty
         in
            case z of
               Cast (z, ty) =>
                  seq [str "Cast ", tuple [layout z, Type.layout ty]]
             | Const c => Const.layout c
             | Frontier => str "<Frontier>"
             | GCState => str "<GCState>"
             | Global g => Global.layout g
             | Label l => Label.layout l
             | Offset {base, offset, ty, volatile} =>
                  seq [str (concat ["O", if volatile then "V" else "", Type.name ty, " "]),
                       tuple [layout base, Bytes.layout offset],
                       constrain ty]
             | SequenceOffset {base, index, offset, scale, ty, volatile} =>
                  seq [str (concat ["X", if volatile then "V" else "", Type.name ty, " "]),
                       tuple [layout base, layout index, Scale.layout scale,
                              Bytes.layout offset],
                       constrain ty]
             | StackOffset so => StackOffset.layout so
             | StackTop => str "<StackTop>"
             | StaticHeapRef h => StaticHeap.Ref.layout h
             | Temporary t => Temporary.layout t
         end

    val toString = Layout.toString o layout

    val rec equals =
         fn (Cast (z, t), Cast (z', t')) =>
                Type.equals (t, t') andalso equals (z, z')
           | (Const c, Const c') => Const.equals (c, c')
           | (GCState, GCState) => true
           | (Global g, Global g') => Global.equals (g, g')
           | (Label l, Label l') => Label.equals (l, l')
           | (Offset {base = b, offset = i, ...},
              Offset {base = b', offset = i', ...}) =>
                equals (b, b') andalso Bytes.equals (i, i')
           | (SequenceOffset {base = b, index = i, ...},
              SequenceOffset {base = b', index = i', ...}) =>
                equals (b, b') andalso equals (i, i')
           | (StackOffset so, StackOffset so') => StackOffset.equals (so, so')
           | (StaticHeapRef h1, StaticHeapRef h2) =>
              StaticHeap.Ref.equals (h1, h2)
           | (Temporary t, Temporary t') => Temporary.equals (t, t')
           | _ => false

      fun gcField field =
         Offset {base = GCState,
                 offset = Runtime.GCField.offset field,
                 ty = Type.ofGCField field,
                 volatile = Runtime.GCField.volatile field}

      val stackOffset = StackOffset o StackOffset.T

      fun interfere (write: t, read: t): bool =
         let
            fun inter read = interfere (write, read)
         in
            case (read, write) of
               (Cast (z, _), _) => interfere (write, z)
             | (_, Cast (z, _)) => interfere (z, read)
             | (Global g, Global g') => Global.equals (g, g')
             | (Offset {base, ...}, _) => inter base
             | (SequenceOffset {base, index, ...}, _) =>
                  inter base orelse inter index
             | (StackOffset so, StackOffset so') =>
                  StackOffset.interfere (so, so')
             | (Temporary t, Temporary t') => Temporary.equals (t, t')
             | (StaticHeapRef h1, StaticHeapRef h2) =>
                  StaticHeap.Ref.equals (h1, h2)
             | _ => false
         end

      val rec isDestination =
         fn Cast (z, _) => isDestination z
          | Global _ => true
          | Offset _ => true
          | SequenceOffset _ => true
          | StackOffset _ => true
          | Temporary _ => true
          | _ => false
   end

structure Switch = Switch (open S
                           structure Use = Operand)

structure Statement =
   struct
      datatype t =
         Move of {dst: Operand.t,
                  src: Operand.t}
       | PrimApp of {args: Operand.t vector,
                     dst: Operand.t option,
                     prim: Type.t Prim.t}

      val layout =
         let
            open Layout
         in
            fn Move {dst, src} =>
                  mayAlign
                  [seq [Operand.layout dst, str " ="],
                   indent (Operand.layout src, 2)]
             | PrimApp {args, dst, prim, ...} =>
                  let
                     val rest =
                        seq [Prim.layout prim, str " ",
                             Vector.layout Operand.layout args]
                  in
                     case dst of
                        NONE => rest
                      | SOME z =>
                           mayAlign
                           [seq [Operand.layout z, str " ="],
                            indent (rest, 2)]
                  end
         end

      fun move (arg as {dst, src}) =
         if Operand.equals (dst, src)
            then NONE
         else SOME (Move arg)

      val move =
         Trace.trace ("Machine.Statement.move",
                      fn {dst, src} =>
                      Layout.record [("dst", Operand.layout dst),
                                     ("src", Operand.layout src)],
                      Option.layout layout)
         move

      fun object {dst, header, size} =
         let
            datatype z = datatype Operand.t
            fun bytes (b: Bytes.t): Operand.t =
               Operand.word (WordX.fromBytes (b, WordSize.csize ()))
            val metaDataSize = Runtime.normalMetaDataSize ()
            val headerOffset = Runtime.headerOffset ()
            val header = Operand.word header
            val temp = Temporary (Temporary.new (Type.cpointer (), NONE))
         in
            Vector.new4
            ((* tmp = Frontier + GC_NORMAL_METADATA_SIZE; *)
             PrimApp {args = Vector.new2 (Frontier, bytes metaDataSize),
                      dst = SOME temp,
                      prim = Prim.CPointer_add},
             (* CHECK; if objptr <> cpointer, need non-trivial coercion here. *)
             (* dst = pointerToObjptr(tmp); *)
             Move {dst = dst, src = Cast (temp, Operand.ty dst)},
             (* OW(dst, -GC_HEADER_SIZE) = header; *)
             Move {dst = Offset {base = dst,
                                 offset = headerOffset,
                                 ty = Type.objptrHeader (),
                                 volatile = false},
                   src = header},
             (* Frontier += size; *)
             PrimApp {args = Vector.new2 (Frontier, bytes size),
                      dst = SOME Frontier,
                      prim = Prim.CPointer_add})
         end

      fun sequence {dst, header, length, size} =
         let
            datatype z = datatype Operand.t
            fun bytes (b: Bytes.t): Operand.t =
               Operand.word (WordX.fromBytes (b, WordSize.csize ()))
            val metaDataSize = Runtime.sequenceMetaDataSize ()
            val headerOffset = Runtime.headerOffset ()
            val lengthOffset = Runtime.sequenceLengthOffset ()
            val counterOffset = Runtime.sequenceCounterOffset ()
            val header = Operand.word header
            val length =
               Operand.word (WordX.fromInt (length, WordSize.seqIndex ()))
            val counter = Operand.zero (WordSize.seqIndex ())
            val temp = Temporary (Temporary.new (Type.cpointer (), NONE))
         in
            Vector.new6
            ((* tmp = Frontier + GC_SEQUENCE_METADATA_SIZE; *)
             PrimApp {args = Vector.new2 (Frontier, bytes metaDataSize),
                      dst = SOME temp,
                      prim = Prim.CPointer_add},
             (* CHECK; if objptr <> cpointer, need non-trivial coercion here. *)
             (* dst = pointerToObjptr(tmp); *)
             Move {dst = dst, src = Cast (temp, Operand.ty dst)},
             (* OW(dst, -(GC_HEADER_SIZE + GC_SEQUENCE_LENGTH_SIZE + GC_SEQUENCE_COUNTER_SIZE)) = 0x0; *)
             Move {dst = Offset {base = dst,
                                 offset = counterOffset,
                                 ty = Type.seqIndex (),
                                 volatile = false},
                   src = counter},
             (* OW(dst, -(GC_HEADER_SIZE + GC_SEQUENCE_LENGTH_SIZE)) = length; *)
             Move {dst = Offset {base = dst,
                                 offset = lengthOffset,
                                 ty = Type.seqIndex (),
                                 volatile = false},
                   src = length},
             (* OW(dst, -GC_HEADER_SIZE) = header; *)
             Move {dst = Offset {base = dst,
                                 offset = headerOffset,
                                 ty = Type.objptrHeader (),
                                 volatile = false},
                   src = header},
             (* Frontier += size; *)
             PrimApp {args = Vector.new2 (Frontier, bytes size),
                      dst = SOME Frontier,
                      prim = Prim.CPointer_add})
         end

      fun foldOperands (s, ac, f) =
         case s of
            Move {dst, src} => f (dst, f (src, ac))
          | PrimApp {args, dst, ...} =>
               Vector.fold (args, Option.fold (dst, ac, f), f)

      fun foldDefs (s, a, f) =
         case s of
            Move {dst, ...} => f (dst, a)
          | PrimApp {dst, ...} => (case dst of
                                      NONE => a
                                    | SOME z => f (z, a))
   end

structure Live =
   struct
      datatype t =
         Global of Global.t
       | StackOffset of StackOffset.t
       | Temporary of Temporary.t

      val layout: t -> Layout.t =
         fn Global g => Global.layout g
          | StackOffset s => StackOffset.layout s
          | Temporary t => Temporary.layout t

      val equals: t * t -> bool =
         fn (Global g, Global g') => Global.equals (g, g')
          | (StackOffset s, StackOffset s') => StackOffset.equals (s, s')
          | (Temporary t, Temporary t') => Temporary.equals (t, t')
          | _ => false

      val ty =
         fn Global g => Global.ty g
          | StackOffset s => StackOffset.ty s
          | Temporary t => Temporary.ty t

      val isSubtype: t * t -> bool =
         fn (Global g, Global g') => Global.isSubtype (g, g')
          | (StackOffset s, StackOffset s') => StackOffset.isSubtype (s, s')
          | (Temporary t, Temporary t') => Temporary.isSubtype (t, t')
          | _ => false

      val interfere: t * t -> bool =
         fn (l, l') =>
         equals (l, l')
         orelse (case (l, l') of
                    (StackOffset s, StackOffset s') =>
                       StackOffset.interfere (s, s')
                  | _ => false)

      val fromOperand: Operand.t -> t option =
         fn Operand.Global g => SOME (Global g)
          | Operand.StackOffset s => SOME (StackOffset s)
          | Operand.Temporary t => SOME (Temporary t)
          | _ => NONE

      val toOperand: t -> Operand.t =
         fn Global g => Operand.Global g
          | StackOffset s => Operand.StackOffset s
          | Temporary t => Operand.Temporary t
   end

structure Transfer =
   struct
      datatype t =
         CCall of {args: Operand.t vector,
                   func: Type.t CFunction.t,
                   return: {return: Label.t,
                            size: Bytes.t option} option}
       | Call of {label: Label.t,
                  live: Live.t vector,
                  return: {return: Label.t,
                           handler: Label.t option,
                           size: Bytes.t} option}
       | Goto of Label.t
       | Raise of {raisesTo: Label.t list}
       | Return of {returnsTo: Label.t list}
       | Switch of Switch.t

      fun layout t =
         let
            open Layout
         in
            case t of
               CCall {args, func, return} =>
                  seq [str "CCall ",
                       record
                       [("args", Vector.layout Operand.layout args),
                        ("func", CFunction.layout (func, Type.layout)),
                        ("return", Option.layout
                         (fn {return, size} =>
                          record [("return", Label.layout return),
                                  ("size", Option.layout Bytes.layout size)])
                         return)]]
             | Call {label, live, return} => 
                  seq [str "Call ", 
                       record [("label", Label.layout label),
                               ("live", Vector.layout Live.layout live),
                               ("return", Option.layout 
                                (fn {return, handler, size} =>
                                 record [("return", Label.layout return),
                                         ("handler",
                                          Option.layout Label.layout handler),
                                         ("size", Bytes.layout size)])
                                return)]]
             | Goto l => seq [str "Goto ", Label.layout l]
             | Raise {raisesTo} =>
                  seq [str "Raise ",
                       record [("raisesTo", List.layout Label.layout raisesTo)]]
             | Return {returnsTo} =>
                  seq [str "Return ",
                       record [("returnsTo", List.layout Label.layout returnsTo)]]
             | Switch s => Switch.layout s
         end

       fun foldOperands (t, ac, f) =
         case t of
            CCall {args, ...} => Vector.fold (args, ac, f)
          | Switch s =>
               Switch.foldLabelUse
               (s, ac, {label = fn (_, a) => a,
                        use = f})
          | _ => ac
   end

structure FrameOffsets =
   struct
      datatype t = T of {index: int,
                         offsets: Bytes.t vector}

      local
         fun make f (T r) = f r
      in
         val index = make #index
         val offsets = make #offsets
      end

      fun new {index, offsets} =
         T {index = index, offsets = offsets}

      fun equals (fo1, fo2) =
         Int.equals (index fo1, index fo2)
         andalso Vector.equals (offsets fo1, offsets fo2, Bytes.equals)

      fun layout (T {index, offsets}) =
         let
            open Layout
         in
            record [("index", Int.layout index),
                    ("offsets", Vector.layout Bytes.layout offsets)]
         end

      fun hash (T {index, offsets}) =
         Hash.combine (Word.fromInt index, Hash.vectorMap (offsets, Bytes.hash))
   end

structure FrameInfo =
   struct
      structure Kind =
         struct
            datatype t =
               CONT_FRAME
             | CRETURN_FRAME
             | FUNC_FRAME
             | HANDLER_FRAME
            fun equals (k1, k2) =
               case (k1, k2) of
                  (CONT_FRAME, CONT_FRAME) => true
                | (CRETURN_FRAME, CRETURN_FRAME) => true
                | (FUNC_FRAME, FUNC_FRAME) => true
                | (HANDLER_FRAME, HANDLER_FRAME) => true
                | _ => false
            local
               val newHash = Random.word
               val cont = newHash ()
               val creturn = newHash ()
               val func = newHash ()
               val handler = newHash ()
            in
               fun hash k =
                  case k of
                     CONT_FRAME => cont
                   | CRETURN_FRAME => creturn
                   | FUNC_FRAME => func
                   | HANDLER_FRAME => handler
            end
            fun toString k =
               case k of
                  CONT_FRAME => "CONT_FRAME"
                | CRETURN_FRAME => "CRETURN_FRAME"
                | FUNC_FRAME => "FUNC_FRAME"
                | HANDLER_FRAME => "HANDLER_FRAME"
            val layout = Layout.str o toString
         end

      datatype t = T of {frameOffsets: FrameOffsets.t,
                         index: int ref,
                         kind: Kind.t,
                         size: Bytes.t,
                         sourceSeqIndex: int option}

      local
         fun make f (T r) = f r
      in
         val frameOffsets = make #frameOffsets
         val indexRef = make #index
         val kind = make #kind
         val size = make #size
         val sourceSeqIndex = make #sourceSeqIndex
      end
      val index = ! o indexRef
      fun setIndex (fi, i) = indexRef fi := i
      val offsets = FrameOffsets.offsets o frameOffsets

      fun new {frameOffsets, index, kind, size, sourceSeqIndex} =
         T {frameOffsets = frameOffsets,
            index = ref index,
            kind = kind,
            size = size,
            sourceSeqIndex = sourceSeqIndex}

      fun equals (fi1, fi2) =
         FrameOffsets.equals (frameOffsets fi1, frameOffsets fi2)
         andalso Ref.equals (indexRef fi1, indexRef fi2)
         andalso Kind.equals (kind fi1, kind fi2)
         andalso Bytes.equals (size fi1, size fi2)
         andalso Option.equals (sourceSeqIndex fi1, sourceSeqIndex fi2, Int.equals)

      fun layout (T {frameOffsets, index, kind, size, sourceSeqIndex}) =
         let
            open Layout
         in
            record [("frameOffsets", FrameOffsets.layout frameOffsets),
                    ("index", Ref.layout Int.layout index),
                    ("kind", Kind.layout kind),
                    ("size", Bytes.layout size),
                    ("sourceSeqIndex", Option.layout Int.layout sourceSeqIndex)]
         end
   end

structure Kind =
   struct
      datatype t =
         Cont of {args: Live.t vector,
                  frameInfo: FrameInfo.t}
       | CReturn of {dst: Live.t option,
                     frameInfo: FrameInfo.t option,
                     func: Type.t CFunction.t}
       | Func of {frameInfo: FrameInfo.t}
       | Handler of {args: Live.t vector,
                     frameInfo: FrameInfo.t}
       | Jump

      fun layout k =
         let
            open Layout
         in
            case k of
               Cont {args, frameInfo} =>
                  seq [str "Cont ",
                       record [("args", Vector.layout Live.layout args),
                               ("frameInfo", FrameInfo.layout frameInfo)]]
             | CReturn {dst, frameInfo, func} =>
                  seq [str "CReturn ",
                       record
                       [("dst", Option.layout Live.layout dst),
                        ("frameInfo", Option.layout FrameInfo.layout frameInfo),
                        ("func", CFunction.layout (func, Type.layout))]]
             | Func {frameInfo} =>
                  seq [str "Func ",
                       record
                       [("frameInfo", FrameInfo.layout frameInfo)]]
             | Handler {args, frameInfo} =>
                  seq [str "Handler ",
                       record [("args", Vector.layout Live.layout args),
                               ("frameInfo", FrameInfo.layout frameInfo)]]
             | Jump => str "Jump"
         end

      fun isEntry (k: t): bool =
         case k of
            Cont _ => true
          | CReturn {func, ...} => CFunction.maySwitchThreadsTo func
          | Func _ => true
          | Handler _ => true
          | _ => false

      val frameInfoOpt =
         fn Cont {frameInfo, ...} => SOME frameInfo
          | CReturn {frameInfo, ...} => frameInfo
          | Func {frameInfo, ...} => SOME frameInfo
          | Handler {frameInfo, ...} => SOME frameInfo
          | Jump => NONE
   end

structure Block =
   struct
      datatype t = T of {kind: Kind.t,
                         label: Label.t,
                         live: Live.t vector,
                         raises: Live.t vector option,
                         returns: Live.t vector option,
                         statements: Statement.t vector,
                         transfer: Transfer.t}

      fun clear (T {label, ...}) = Label.clear label

      local
         fun make g (T r) = g r
      in
         val kind = make #kind
         val label = make #label
      end

      fun layoutHeader (T {kind, label, live, raises, returns, ...}) =
         let
            open Layout
         in
            seq [Label.layout label,
                 str ": ",
                 record [("kind", Kind.layout kind),
                         ("live", Vector.layout Live.layout live),
                         ("raises",
                          Option.layout (Vector.layout Live.layout)
                          raises),
                         ("returns",
                          Option.layout (Vector.layout Live.layout)
                          returns)]]
         end

      fun layout (b as T {statements, transfer, ...}) =
         let
            open Layout
         in
            align [layoutHeader b,
                   indent (align
                           [align
                            (Vector.toListMap (statements, Statement.layout)),
                            Transfer.layout transfer],
                           2)]
         end

      fun layouts (block, output' : Layout.t -> unit) = output' (layout block)

      fun foldDefs (T {kind, statements, ...}, a, f) =
         let
            val a =
               case kind of
                  Kind.CReturn {dst, ...} =>
                     (case dst of
                         NONE => a
                       | SOME z => f (Live.toOperand z, a))
                | _ => a
            val a =
               Vector.fold (statements, a, fn (s, a) =>
                            Statement.foldDefs (s, a, f))
         in
            a
         end
   end

structure Chunk =
   struct
      datatype t = T of {blocks: Block.t vector,
                         chunkLabel: ChunkLabel.t,
                         tempsMax: CType.t -> int}

      local
         fun make sel (T r) = sel r
      in
         val chunkLabel = make #chunkLabel
      end

      fun layouts (T {blocks, chunkLabel, ...}, output' : Layout.t -> unit) =
         let
            open Layout
         in
            ((output' o seq) [str "Chunk ", ChunkLabel.layout chunkLabel]) ;
            Vector.foreach (blocks, fn block => Block.layouts (block, output'))
         end

      fun clear (T {blocks, ...}) =
         Vector.foreach (blocks, Block.clear)
   end

structure Program =
   struct
      datatype t = T of {chunks: Chunk.t list,
                         frameInfos: FrameInfo.t vector,
                         frameOffsets: FrameOffsets.t vector,
                         globals: {objptrs: (StaticHeap.Ref.t * Global.t) list,
                                   reals: (RealX.t * Global.t) list},
                         handlesSignals: bool,
                         main: {chunkLabel: ChunkLabel.t,
                                label: Label.t},
                         maxFrameSize: Bytes.t,
                         objectTypes: ObjectType.t vector,
                         sourceMaps: SourceMaps.t option,
                         staticHeaps: StaticHeap.Kind.t -> StaticHeap.Object.t vector}

      fun clear (T {chunks, ...}) =
         List.foreach (chunks, Chunk.clear)

      fun layouts (T {chunks, frameInfos, frameOffsets, handlesSignals,
                      main = {label, ...},
                      maxFrameSize, objectTypes, sourceMaps, staticHeaps, ...},
                   output': Layout.t -> unit) =
         let
            open Layout
            val output = output'
         in
            output (record
                    [("handlesSignals", Bool.layout handlesSignals),
                     ("main", Label.layout label),
                     ("maxFrameSize", Bytes.layout maxFrameSize),
                     ("frameOffsets", Vector.layout FrameOffsets.layout frameOffsets),
                     ("frameInfos", Vector.layout FrameInfo.layout frameInfos)])
            ; Option.app (sourceMaps, fn pi =>
                          (output (str "\nSourceMaps:")
                           ; SourceMaps.layouts (pi, output)))
            ; output (str "\nObjectTypes:")
            ; Vector.foreachi (objectTypes, fn (i, ty) =>
                               output (seq [str "opt_", Int.layout i,
                                            str " = ", ObjectType.layout ty]))
            ; output (str "\n")
            ; List.foreach (StaticHeap.Kind.all, fn k =>
                            (output (seq [Label.layout (StaticHeap.Kind.label k), str ":"])
                             ; output (Vector.layout StaticHeap.Object.layout (staticHeaps k))))
            ; output (str "\n")
            ; List.foreach (chunks, fn chunk => Chunk.layouts (chunk, output))
         end

      val toFile = {display = Control.Layouts layouts, style = Control.ML, suffix = "machine"}

      fun layoutStats (program as T {chunks, objectTypes, ...}) =
         let
            val numChunks = ref 0
            val numBlocks = ref 0
            val numStatements = ref 0
            val _ =
               List.foreach
               (chunks, fn Chunk.T {blocks, ...} =>
                (Int.inc numChunks
                 ; Vector.foreach
                   (blocks, fn Block.T {statements, ...} =>
                    (Int.inc numBlocks
                     ; numStatements := !numStatements + Vector.length statements))))
            val numObjectTypes = Vector.length objectTypes
            open Layout
         in
            align
            [seq [Control.sizeMessage ("machine program", program)],
             seq [str "num chunks in program = ", Int.layout (!numChunks)],
             seq [str "num blocks in program = ", Int.layout (!numBlocks)],
             seq [str "num statements in program = ", Int.layout (!numStatements)],
             seq [str "num object types in program = ", Int.layout (numObjectTypes)]]
         end

      fun shuffle (T {chunks, frameInfos, frameOffsets, globals,
                      handlesSignals, main, maxFrameSize,
                      objectTypes, sourceMaps, staticHeaps}) =
         let
            fun shuffle v =
               let
                  val a = Array.fromVector v
                  val () = Array.shuffle a
               in
                  Array.toVector a
               end
            val chunks = Vector.fromList chunks
            val chunks = shuffle chunks
            val chunks =
               Vector.map
               (chunks, fn Chunk.T {blocks, chunkLabel, tempsMax} =>
                Chunk.T
                {blocks = shuffle blocks,
                 chunkLabel = chunkLabel,
                 tempsMax = tempsMax})
            val chunks = Vector.toList chunks
         in
            T {chunks = chunks,
               frameInfos = frameInfos,
               frameOffsets = frameOffsets,
               globals = globals,
               handlesSignals = handlesSignals,
               main = main,
               maxFrameSize = maxFrameSize,
               objectTypes = objectTypes,
               sourceMaps = sourceMaps,
               staticHeaps = staticHeaps}
         end

      structure Alloc =
         struct
            datatype t = T of Live.t list

            fun layout (T ds) = List.layout Live.layout ds

            fun forall (T ds, f) = List.forall (ds, f o Live.toOperand)

            fun defineLive (T ls, l) = T (l :: ls)

            fun define (T ds, z) =
               case Live.fromOperand z of
                  NONE => T ds
                | SOME d => T (d :: ds)

            val new: Live.t list -> t = T

            fun doesDefine (T ls, l': Live.t): bool =
               let
                  val oper' = Live.toOperand l'
               in
                  case List.peek (ls, fn l =>
                                  Operand.interfere (Live.toOperand l, oper')) of
                     NONE => false
                   | SOME l => Live.isSubtype (l, l')
               end

            val doesDefine =
               Trace.trace2 
               ("Machine.Program.Alloc.doesDefine", 
                layout, Live.layout, Bool.layout)
               doesDefine
         end

      fun typeCheck (program as
                     T {chunks, frameInfos, frameOffsets, globals = {objptrs, reals, ...},
                        maxFrameSize, objectTypes, sourceMaps, staticHeaps, ...}) =
         let
            val _ =
               Err.check
               ("sourceMaps",
                fn () =>
                (case (!Control.profile, sourceMaps) of
                    (Control.ProfileNone, NONE) => true
                  | (_, NONE) => false
                  | (Control.ProfileNone, SOME _) => false
                  | (_, SOME sourceMaps) => SourceMaps.check sourceMaps),
                fn () => Option.layout SourceMaps.layout sourceMaps)
            val _ =
               Vector.foreachi
               (frameOffsets, fn (i, fo) =>
                let
                   val index = FrameOffsets.index fo
                   val offsets = FrameOffsets.offsets fo
                in
                   Err.check ("frameOffsets",
                              fn () => (Int.equals (i, index)
                                        andalso Vector.forall
                                                (offsets, fn offset =>
                                                 Bytes.< (offset, maxFrameSize))),
                              fn () => FrameOffsets.layout fo)
                end)
            fun checkFrameOffsets fo =
               let
                  val index = FrameOffsets.index fo
               in
                  FrameOffsets.equals (Vector.sub (frameOffsets, index), fo)
                  handle Subscript => false
               end
            val _ =
               Vector.foreachi
               (frameInfos, fn (i, fi) =>
                let
                   val index = FrameInfo.index fi
                   val frameOffsets = FrameInfo.frameOffsets fi
                   val size = FrameInfo.size fi
                in
                   Err.check
                   ("frameInfos",
                    fn () => (Int.equals (i, index)
                              andalso checkFrameOffsets frameOffsets
                              andalso Bytes.<= (size, maxFrameSize)
                              andalso Bytes.<= (size, Runtime.maxFrameSize)
                              andalso (Bytes.isAligned
                                       (size,
                                        {alignment = (case !Control.align of
                                                         Control.Align4 => Bytes.inWord32
                                                       | Control.Align8 => Bytes.inWord64)}))),
                    fn () => FrameInfo.layout fi)
                end)
            fun checkFrameInfo fi =
               let
                  val index = FrameInfo.index fi
               in
                  FrameInfo.equals (Vector.sub (frameInfos, index), fi)
                  handle Subscript => false
               end
            val _ =
               Vector.foreach
               (objectTypes, fn ty =>
                Err.check ("objectType",
                           fn () => ObjectType.isOk ty,
                           fn () => ObjectType.layout ty))
            fun tyconTy (opt: ObjptrTycon.t): ObjectType.t =
               Vector.sub (objectTypes, ObjptrTycon.index opt)
            open Layout

            val staticHeaps =
               let
                  open StaticHeap
               in
                  Kind.memoize
                  (fn k =>
                   (#1 o Vector.mapAndFold)
                   (staticHeaps k, Bytes.zero, fn (obj, next) =>
                    ((Bytes.+ (next, Object.metaDataSize obj), obj),
                     Bytes.+ (next, Object.size (obj, {tyconTy = tyconTy})))))
               end

            fun checkGlobal (name, global, isOk, layoutVal) =
               let
                  val ty = Global.ty global
                  open Layout
               in
                  Err.check
                  (name,
                   fn () => isOk ty,
                   fn () => seq [layoutVal (), str ": ", Type.layout ty])
               end
            val _ =
               List.foreach
               (objptrs, fn (r, g) =>
                checkGlobal
                ("global objptr", g,
                 fn t => Type.equals (t, StaticHeap.Ref.ty r),
                 fn () => StaticHeap.Ref.layout r))
            val _ =
               List.foreach
               (reals, fn (r, g) =>
                checkGlobal
                ("global real", g,
                 fn t => Type.equals (t, Type.real (RealX.size r)),
                 fn () => RealX.layout (r, {suffix=true})))
            (* Check for no duplicate labels. *)
            local
               val {get, ...} =
                  Property.get (Label.plist,
                                Property.initFun (fn _ => ref false))
            in
               val _ =
                  List.foreach
                  (chunks, fn Chunk.T {blocks, ...} =>
                   Vector.foreach
                   (blocks, fn Block.T {label, ...} =>
                    let
                       val r = get label
                    in
                       if !r
                          then Error.bug "Machine.Program.typeCheck: duplicate label"
                       else r := true
                    end))
            end
            val {get = labelBlock: Label.t -> Block.t,
                 set = setLabelBlock, ...} =
               Property.getSetOnce (Label.plist,
                                    Property.initRaise ("block", Label.layout))
            val _ =
               List.foreach
               (chunks, fn Chunk.T {blocks, ...} =>
                Vector.foreach
                (blocks, fn b as Block.T {label, ...} =>
                 setLabelBlock (label, b)))
            fun checkStaticHeapRef (StaticHeap.Ref.T {index, kind, offset, ty}) =
               let
                  val (dataOffset, obj) = Vector.sub (staticHeaps kind, index)
               in
                  Bytes.equals (dataOffset, offset)
                  andalso Type.equals (StaticHeap.Object.ty obj, ty)
               end
            fun checkOperand (x: Operand.t, alloc: Alloc.t): unit =
               let
                  datatype z = datatype Operand.t
                  fun ok () =
                     case x of
                        Cast (z, t) =>
                           (checkOperand (z, alloc)
                            ; (Type.castIsOk
                               {from = Operand.ty z,
                                to = t,
                                tyconTy = tyconTy}))
                      | Const _ => true
                      | Frontier => true
                      | GCState => true
                      | Global _ =>
                           (* We don't check that globals are defined because
                            * they aren't captured by liveness info.  It would
                            * be nice to fix this.
                            *)
                           true
                      | Label l => 
                           (let val _ = labelBlock l
                            in true
                            end handle _ => false)
                      | Offset {base, offset, ty, volatile = _} =>
                           (checkOperand (base, alloc)
                            ; (Type.offsetIsOk
                               {base = Operand.ty base,
                                (* MachineIR doesn't distinguish
                                 * initialization of object field
                                 * from update of object field;
                                 * only the latter requires
                                 * the field to be mutable.
                                 *)
                                mustBeMutable = false,
                                offset = offset,
                                tyconTy = tyconTy,
                                result = ty}))
                      | StackOffset (so as StackOffset.T {offset, ty, volatile = _}) =>
                           Bytes.<= (Bytes.+ (offset, Type.bytes ty), maxFrameSize)
                           andalso Alloc.doesDefine (alloc, Live.StackOffset so)
                           andalso (case Type.deLabel ty of
                                       NONE => true
                                     | SOME l =>
                                          let
                                             val Block.T {kind, ...} =
                                                labelBlock l
                                             fun doit fi =
                                                let
                                                   val size = FrameInfo.size fi
                                                in
                                                   Bytes.equals
                                                   (size,
                                                    Bytes.+ (offset,
                                                             Runtime.labelSize ()))
                                                end
                                          in
                                             case kind of
                                                Kind.Cont {frameInfo, ...} =>
                                                   doit frameInfo
                                              | Kind.CReturn {frameInfo, ...} =>
                                                   (case frameInfo of
                                                       NONE => true
                                                     | SOME fi => doit fi)
                                              | Kind.Func {frameInfo, ...} =>
                                                   doit frameInfo
                                              | Kind.Handler {frameInfo, ...} =>
                                                   doit frameInfo
                                              | Kind.Jump => true
                                          end)
                      | SequenceOffset {base, index, offset, scale, ty, volatile = _} =>
                           (checkOperand (base, alloc)
                            ; checkOperand (index, alloc)
                            ; (Type.sequenceOffsetIsOk
                               {base = Operand.ty base,
                                index = Operand.ty index,
                                (* MachineIR doesn't distinguish
                                 * initialization of object field
                                 * from update of object field;
                                 * only the latter requires
                                 * the field to be mutable.
                                 *)
                                mustBeMutable = false,
                                offset = offset,
                                tyconTy = tyconTy,
                                result = ty,
                                scale = scale}))
                      | StaticHeapRef r => checkStaticHeapRef r
                      | StackTop => true
                      | Temporary t => Alloc.doesDefine (alloc, Live.Temporary t)
               in
                  Err.check ("operand", ok, fn () => Operand.layout x)
               end
            fun checkOperands (v, a) =
               Vector.foreach (v, fn z => checkOperand (z, a))
            fun check' (x, name, isOk, layout) =
               Err.check (name, fn () => isOk x, fn () => layout x)
            val labelKind = Block.kind o labelBlock
            fun checkKind (k: Kind.t, alloc: Alloc.t): Alloc.t option =
               let
                  datatype z = datatype Kind.t
                  exception No
                  fun frame' (frameInfo,
                             useSlots: bool,
                             chkKind: FrameInfo.Kind.t -> bool): bool =
                     checkFrameInfo frameInfo
                     andalso
                     chkKind (FrameInfo.kind frameInfo)
                     andalso
                     (not useSlots
                      orelse
                      let
                         val Alloc.T zs = alloc
                         val liveOffsets =
                            List.fold
                            (zs, [], fn (z, liveOffsets) =>
                             case z of
                                Live.StackOffset (StackOffset.T {offset, ty, ...}) =>
                                   if Type.isObjptr ty
                                      then offset :: liveOffsets
                                      else liveOffsets
                              | _ => raise No)
                         val liveOffsets = Array.fromList liveOffsets
                         val () = QuickSort.sortArray (liveOffsets, Bytes.<=)
                         val liveOffsets = Vector.fromArray liveOffsets
                      in
                         Vector.equals
                         (liveOffsets, FrameInfo.offsets frameInfo,
                          Bytes.equals)
                      end) handle No => false
                  fun frame (frameInfo,
                             useSlots: bool,
                             kind: FrameInfo.Kind.t): bool =
                     frame' (frameInfo,
                             useSlots,
                             fn k => FrameInfo.Kind.equals (kind, k))
                  fun slotsAreInFrame (fi: FrameInfo.t): bool =
                     let
                        val size = FrameInfo.size fi
                     in
                        Alloc.forall
                        (alloc, fn z =>
                         case z of
                            Operand.StackOffset (StackOffset.T {offset, ty, ...}) =>
                               Bytes.<= (Bytes.+ (offset, Type.bytes ty), size)
                          | _ => false)
                     end
               in
                  case k of
                     Cont {args, frameInfo} =>
                        if frame (frameInfo, true, FrameInfo.Kind.CONT_FRAME)
                           andalso slotsAreInFrame frameInfo
                           then SOME (Vector.fold
                                      (args, alloc, fn (z, alloc) =>
                                       Alloc.defineLive (alloc, z)))
                        else NONE
                   | CReturn {dst, frameInfo, func, ...} =>
                        let
                           val ok =
                              (case dst of
                                  NONE => true
                                | SOME z =>
                                     Type.isSubtype (CFunction.return func,
                                                     Live.ty z))
                              andalso
                              (if CFunction.mayGC func
                                  then (case frameInfo of
                                           NONE => false
                                         | SOME fi =>
                                              (frame (fi, true, FrameInfo.Kind.CRETURN_FRAME)
                                               andalso slotsAreInFrame fi))
                               else if !Control.profile = Control.ProfileNone
                                       then true
                                    else (case frameInfo of
                                             NONE => false
                                           | SOME fi => frame (fi, false, FrameInfo.Kind.CRETURN_FRAME)))
                        in
                           if ok
                              then SOME (case dst of
                                            NONE => alloc
                                          | SOME z => Alloc.defineLive (alloc, z))
                           else NONE
                        end
                   | Func {frameInfo, ...} =>
                        if frame (frameInfo, false, FrameInfo.Kind.FUNC_FRAME)
                           then SOME alloc
                        else NONE
                   | Handler {args, frameInfo} =>
                        if frame (frameInfo, false, FrameInfo.Kind.HANDLER_FRAME)
                           then SOME (Vector.fold
                                      (args, alloc, fn (z, alloc) =>
                                       Alloc.defineLive (alloc, z)))
                        else NONE
                   | Jump => SOME alloc
               end
            fun checkStatement (s: Statement.t, alloc: Alloc.t)
               : Alloc.t option =
               let
                  datatype z = datatype Statement.t
               in
                  case s of
                     Move {dst, src} =>
                        let
                           val _ = checkOperand (src, alloc)
                           val alloc = Alloc.define (alloc, dst)
                           val _ = checkOperand (dst, alloc)
                        in
                           if Type.isSubtype (Operand.ty src, Operand.ty dst)
                              andalso Operand.isDestination dst
                              then SOME alloc
                           else NONE
                        end
                   | PrimApp {args, dst, prim, ...} =>
                        let
                           val _ = checkOperands (args, alloc)
                           val alloc =
                              case dst of
                                 NONE => SOME alloc
                               | SOME z =>
                                    let
                                       val alloc = Alloc.define (alloc, z)
                                       val _ = checkOperand (z, alloc)
                                    in
                                       SOME alloc
                                    end
                           val ok =
                              Type.checkPrimApp
                              {args = Vector.map (args, Operand.ty),
                               prim = prim,
                               result = Option.map (dst, Operand.ty)}
                        in
                           if ok
                              then alloc
                              else NONE
                        end
               end
            fun liveIsOk (live: Live.t vector,
                          a: Alloc.t): bool =
               Vector.forall (live, fn z => Alloc.doesDefine (a, z))
            val liveIsOk =
               Trace.trace
               ("Machine.Program.typeCheck.liveIsOk",
                fn (live, a) =>
                Layout.tuple [Vector.layout Live.layout live,
                              Alloc.layout a],
                Bool.layout)
               liveIsOk
            fun liveSubset (live: Live.t vector,
                            live': Live.t vector): bool =
               Vector.forall
               (live, fn z => Vector.exists (live', fn z' =>
                                             Live.equals (z, z')))
            fun goto (Block.T {live,
                               raises = raises',
                               returns = returns', ...},
                      raises: Live.t vector option,
                      returns: Live.t vector option,
                      alloc: Alloc.t): bool =
               liveIsOk (live, alloc)
               andalso
               (case (raises, raises') of
                   (_, NONE) => true
                 | (SOME gs, SOME gs') =>
                      Vector.equals (gs', gs, Live.isSubtype)
                 | _ => false)
               andalso
               (case (returns, returns') of
                   (_, NONE) => true
                 | (SOME os, SOME os') =>
                      Vector.equals (os', os, Live.isSubtype)
                 | _ => false)
            val goto =
               Trace.trace
               ("Machine.Program.typeCheck.goto",
                fn (b, raises, returns, a) =>
                Layout.tuple [Block.layoutHeader b,
                              Option.layout (Vector.layout Live.layout) raises,
                              Option.layout (Vector.layout Live.layout) returns,
                              Alloc.layout a],
                Bool.layout)
               goto
            fun checkCont (cont: Label.t, size: Bytes.t, alloc: Alloc.t) =
               let
                  val Block.T {kind, live, ...} = labelBlock cont
               in
                  if liveIsOk (live, alloc)
                     then
                        (case kind of
                            Kind.Cont {args, frameInfo, ...} =>
                               (if Bytes.equals (FrameInfo.size frameInfo, size)
                                   then
                                      SOME
                                      (live,
                                       SOME
                                       (Vector.map
                                        (args, fn z =>
                                         case z of
                                            Live.StackOffset s =>
                                               Live.StackOffset
                                               (StackOffset.shift (s, size))
                                          | _ => z)))
                                   else NONE)
                          | _ => NONE)
                     else NONE
               end
            fun callIsOk {alloc: Alloc.t,
                          dst: Label.t,
                          live: Live.t vector,
                          raises: Live.t vector option,
                          return,
                          returns: Live.t vector option} =
               let
                  val {raises, returns, size} =
                     case return of
                        NONE =>
                           {raises = raises,
                            returns = returns,
                            size = Bytes.zero}
                      | SOME {handler, return, size} =>
                           let
                              val (contLive, returns) =
                                 Err.check'
                                 ("cont",
                                  fn () => checkCont (return, size, alloc),
                                  fn () => Label.layout return)
                              fun checkHandler () =
                                 case handler of
                                    NONE => SOME raises
                                  | SOME h =>
                                       let
                                          val Block.T {kind, live = handlerLive, ...} =
                                             labelBlock h
                                       in
                                          if liveSubset (handlerLive, contLive)
                                             then
                                                (case kind of
                                                    Kind.Handler {frameInfo, ...} =>
                                                       if Bytes.< (FrameInfo.size frameInfo, size)
                                                          then SOME (SOME (Vector.new0 ()))
                                                          else NONE
                                                    | _ => NONE)
                                             else NONE
                                       end
                              val raises =
                                 Err.check'
                                 ("handler", checkHandler,
                                  fn () => Option.layout Label.layout handler)
                           in
                              {raises = raises,
                               returns = returns,
                               size = size}
                           end
                  val b = labelBlock dst
                  val alloc =
                     Alloc.T
                     (Vector.fold
                      (live, [], fn (z, ac) =>
                       case z of
                          Live.StackOffset (StackOffset.T {offset, ty, volatile}) =>
                             if Bytes.< (offset, size)
                                then ac
                             else (Live.StackOffset
                                   (StackOffset.T
                                    {offset = Bytes.- (offset, size),
                                     ty = ty,
                                     volatile = volatile})) :: ac
                        | _ => ac))
               in
                  goto (b, raises, returns, alloc)
               end
            fun transferOk
               (t: Transfer.t,
                raises: Live.t vector option,
                returns: Live.t vector option,
                alloc: Alloc.t): bool =
               let
                  fun jump (l: Label.t) =
                     let
                        val b as Block.T {kind, ...} = labelBlock l
                     in
                        (case kind of
                            Kind.Jump => true
                          | _ => false)
                        andalso goto (b, raises, returns, alloc)
                     end
                  datatype z = datatype Transfer.t
               in
                  case t of
                     CCall {args, func, return} =>
                        let
                           val _ = checkOperands (args, alloc)
                        in
                           CFunction.isOk (func, {isUnit = Type.isUnit})
                           andalso
                           Vector.equals (args, CFunction.args func,
                                          fn (z, t) =>
                                          Type.isSubtype (Operand.ty z, t))
                           andalso
                           case return of
                              NONE => true
                            | SOME {return, size} =>
                                 let 
                                    val Block.T {live, ...} = labelBlock return
                                 in
                                    liveIsOk (live, alloc)
                                    andalso
                                    case labelKind return of
                                       Kind.CReturn
                                       {frameInfo = fi, func = f, ...} =>
                                          CFunction.equals (func, f)
                                          andalso (Option.equals
                                                   (size, Option.map (fi, FrameInfo.size),
                                                    Bytes.equals))
                                     | _ => false
                                 end
                        end
                   | Call {label, live, return} =>
                        liveIsOk (live, alloc)
                        andalso
                        callIsOk {alloc = alloc,
                                  dst = label,
                                  live = live,
                                  raises = raises,
                                  return = return,
                                  returns = returns}
                   | Goto l => jump l
                   | Raise _ =>
                        (case raises of
                            NONE => false
                          | SOME live => liveIsOk (live, alloc))
                   | Return _ =>
                        (case returns of
                            NONE => false
                          | SOME live => liveIsOk (live, alloc))
                   | Switch s =>
                        Switch.isOk
                        (s, {checkUse = fn z => checkOperand (z, alloc),
                             labelIsOk = jump})
               end
            val transferOk =
               Trace.trace
               ("Machine.Program.typeCheck.transferOk",
                fn (t, raises, returns, a) =>
                Layout.tuple [Transfer.layout t,
                              Option.layout (Vector.layout Live.layout) raises,
                              Option.layout (Vector.layout Live.layout) returns,
                              Alloc.layout a],
                Bool.layout)
               transferOk
            fun blockOk (Block.T {kind, live, raises, returns, statements,
                                  transfer, ...}): bool =
               let
                  val live = Vector.toList live
                  val _ =
                     Err.check
                     ("live",
                      fn () =>
                      let
                         fun loop zs =
                            case zs of
                               [] => true
                             | z :: zs =>
                                  List.forall
                                  (zs, fn z' =>
                                   not (Live.interfere (z, z')))
                      in
                         loop live
                      end,
                      fn () => List.layout Live.layout live)
                  val alloc = Alloc.new live
                  val alloc =
                     Err.check'
                     ("kind",
                      fn () => checkKind (kind, alloc),
                      fn () => Kind.layout kind)
                  val alloc =
                     Vector.fold
                     (statements, alloc, fn (s, alloc) =>
                      Err.check'
                      ("statement",
                       fn () => checkStatement (s, alloc),
                       fn () => Statement.layout s))
                  val _ =
                     Err.check
                     ("transfer",
                      fn () => transferOk (transfer, raises, returns, alloc),
                      fn () => Transfer.layout transfer)
               in
                  true
               end
            fun checkStaticHeapElem (e: StaticHeap.Elem.t): unit =
               let
                  datatype z = datatype StaticHeap.Elem.t
                  fun ok () =
                     case e of
                        Cast (z, t) =>
                           (checkStaticHeapElem z
                            ; Type.castIsOk
                              {from = StaticHeap.Elem.ty e,
                               to = t,
                               tyconTy = tyconTy})
                      | Const _ => true
                      | Ref r => checkStaticHeapRef r
               in
                  Err.check ("elem", ok, fn () => StaticHeap.Elem.layout e)
               end
            val _ =
               List.foreach
               (StaticHeap.Kind.all, fn kind =>
                Err.check
                ("staticHeap",
                 fn () =>
                 (Vector.foreach
                  (staticHeaps kind, fn (_, obj) =>
                   Err.check
                   ("object",
                    fn () => StaticHeap.Object.isOk (obj, {checkUse = checkStaticHeapElem,
                                                           tyconTy = tyconTy}),
                    fn () => StaticHeap.Object.layout obj))
                  ; true),
                 fn () => Label.layout (StaticHeap.Kind.label kind)))
            val _ =
               List.foreach
               (chunks,
                fn Chunk.T {blocks, ...} =>
                let
                in
                   Vector.foreach
                   (blocks, fn b =>
                    check' (b, "block", blockOk, Block.layout))
                end)
            val _ = clear program
         in
            ()
         end handle Err.E e => (Layout.outputl (Err.layout e, Out.error)
                                ; Error.bug "Machine.typeCheck")

      fun clearLabelNames (T {chunks, ...}): unit =
         List.foreach
         (chunks, fn Chunk.T {blocks, ...} =>
          Vector.foreach
          (blocks, fn Block.T {label, ...} =>
           Label.clearPrintName label))
   end

fun simplify p =
   let
      val machinePasses =
         {name = "machineShuffle", doit = Program.shuffle, execute = false} ::
         nil
      val p =
         Control.simplifyPasses
         {arg = p,
          passes = machinePasses,
          stats = Program.layoutStats,
          toFile = Program.toFile,
          typeCheck = Program.typeCheck}
   in
      p
   end

end
