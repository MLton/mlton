(* Copyright (C) 2009,2014 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Machine (S: MACHINE_STRUCTS): MACHINE =
struct

open S

structure ObjptrTycon = ObjptrTycon ()
structure Runtime = Runtime ()
structure Scale = Scale ()
structure RepType = RepType (structure CFunction = CFunction
                             structure CType = CType
                             structure Label = Label
                             structure ObjptrTycon = ObjptrTycon
                             structure Prim = Prim
                             structure RealSize = RealSize
                             structure Runtime = Runtime
                             structure Scale = Scale
                             structure WordSize = WordSize
                             structure WordX = WordX
                             structure WordXVector = WordXVector)
structure ObjectType = RepType.ObjectType

structure Type = RepType

structure ChunkLabel = Id (val noname = "ChunkLabel")

structure Register =
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
            seq [str (concat ["R", Type.name ty]),
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
               Error.bug (concat ["Machine.Register: register ", 
                                  toString r, " missing index"])
          | SOME i => i

      fun setIndex (r as T {index, ...}, i) =
         case !index of
            NONE => index := SOME i
          | SOME _ =>
               Error.bug (concat ["Machine.Register: register ", 
                                  toString r, " index already set"])

      fun new (ty, i) = T {index = ref i,
                           ty = ty}

      fun equals (r, r') =
         (case (indexOpt r, indexOpt r') of
             (SOME i, SOME i') => i = i'
           | _ => false)
         andalso CType.equals (Type.toCType (ty r), Type.toCType (ty r'))

      val equals =
         Trace.trace2 ("Machine.Register.equals", layout, layout, Bool.layout) equals

      val isSubtype: t * t -> bool =
         fn (T {index = i, ty = t}, T {index = i', ty = t'}) =>
         (case (!i, !i') of
             (SOME i, SOME i') => i = i'
           | _ => false)
         andalso Type.isSubtype (t, t')
         andalso CType.equals (Type.toCType t, Type.toCType t')
   end

structure Global =
   struct
      datatype t = T of {index: int,
                         isRoot: bool,
                         ty: Type.t}

      fun layout (T {index, isRoot, ty, ...}) =
         let
            open Layout
         in
            seq [str "glob ",
                 record [("index", Int.layout index),
                         ("isRoot", Bool.layout isRoot),
                         ("ty", Type.layout ty)]]
         end

      local
         fun make f (T r) = f r
      in
         val index = make #index
         val isRoot = make #isRoot
         val ty = make #ty
      end

      val nonRootCounter = Counter.new 0
      fun numberOfNonRoot () = Counter.value nonRootCounter

      val memo = CType.memo (fn _ => Counter.new 0)
      fun numberOfType t = Counter.value (memo t)

      fun new {isRoot, ty} =
         let
            val isRoot = isRoot orelse not (Type.isObjptr ty)
            val counter =
               if isRoot
                  then memo (Type.toCType ty)
               else nonRootCounter
            val g = T {index = Counter.next counter,
                       isRoot = isRoot,
                       ty = ty}
         in
            g
         end

      fun equals (T {index = i, isRoot = r, ty},
                  T {index = i', isRoot = r', ty = ty'}) =
         i = i'
         andalso r = r'
         andalso Type.equals (ty, ty')

      val isSubtype: t * t -> bool =
         fn (T {index = i, isRoot = r, ty},
             T {index = i', isRoot = r', ty = ty'}) =>
         i = i'
         andalso r = r'
         andalso Type.isSubtype (ty, ty')
         andalso CType.equals (Type.toCType ty, Type.toCType ty')
   end

structure StackOffset =
   struct
      datatype t = T of {offset: Bytes.t,
                         ty: Type.t}

      local
         fun make f (T r) = f r
      in
         val ty = make #ty
      end

      fun layout (T {offset, ty}): Layout.t =
         let
            open Layout
         in
            seq [str (concat ["S", Type.name ty]),
                 paren (Bytes.layout offset),
                 str ": ", Type.layout ty]
         end

      val equals: t * t -> bool =
         fn (T {offset = b, ty}, T {offset = b', ty = ty'}) =>
         Bytes.equals (b, b') andalso Type.equals (ty, ty')

      val isSubtype: t * t -> bool =
         fn (T {offset = b, ty = t}, T {offset = b', ty = t'}) =>
         Bytes.equals (b, b') andalso Type.isSubtype (t, t')

      val interfere: t * t -> bool =
         fn (T {offset = b, ty = ty}, T {offset = b', ty = ty'}) =>
         let 
            val max = Bytes.+ (b, Type.bytes ty)
            val max' = Bytes.+ (b', Type.bytes ty')
         in
            Bytes.> (max, b') andalso Bytes.> (max', b)
         end

      fun shift (T {offset, ty}, size): t =
         T {offset = Bytes.- (offset, size),
            ty = ty}
   end

structure Operand =
   struct
      datatype t =
         ArrayOffset of {base: t,
                         index: t,
                         offset: Bytes.t,
                         scale: Scale.t,
                         ty: Type.t}
       | Cast of t * Type.t
       | Contents of {oper: t,
                      ty: Type.t}
       | Frontier
       | GCState
       | Global of Global.t
       | Label of Label.t
       | Null
       | Offset of {base: t,
                    offset: Bytes.t,
                    ty: Type.t}
       | Register of Register.t
       | Real of RealX.t
       | StackOffset of StackOffset.t
       | StackTop
       | Word of WordX.t

    val ty =
       fn ArrayOffset {ty, ...} => ty
        | Cast (_, ty) => ty
        | Contents {ty, ...} => ty
        | Frontier => Type.cpointer ()
        | GCState => Type.gcState ()
        | Global g => Global.ty g
        | Label l => Type.label l
        | Null => Type.cpointer ()
        | Offset {ty, ...} => ty
        | Real r => Type.real (RealX.size r)
        | Register r => Register.ty r
        | StackOffset s => StackOffset.ty s
        | StackTop => Type.cpointer ()
        | Word w => Type.ofWordX w

    fun layout (z: t): Layout.t =
         let
            open Layout 
            fun constrain (ty: Type.t): Layout.t =
               if !Control.showTypes
                  then seq [str ": ", Type.layout ty]
               else empty
         in
            case z of
               ArrayOffset {base, index, offset, scale, ty} =>
                  seq [str (concat ["X", Type.name ty, " "]),
                       tuple [layout base, layout index, Scale.layout scale,
                              Bytes.layout offset],
                       constrain ty]
             | Cast (z, ty) =>
                  seq [str "Cast ", tuple [layout z, Type.layout ty]]
             | Contents {oper, ty} =>
                  seq [str (concat ["C", Type.name ty, " "]),
                       paren (layout oper)]
             | Frontier => str "<Frontier>"
             | GCState => str "<GCState>"
             | Global g => Global.layout g
             | Label l => Label.layout l
             | Null => str "NULL"
             | Offset {base, offset, ty} =>
                  seq [str (concat ["O", Type.name ty, " "]),
                       tuple [layout base, Bytes.layout offset],
                       constrain ty]
             | Real r => RealX.layout r
             | Register r => Register.layout r
             | StackOffset so => StackOffset.layout so
             | StackTop => str "<StackTop>"
             | Word w => WordX.layout w
         end

    val toString = Layout.toString o layout

    val rec equals =
         fn (ArrayOffset {base = b, index = i, ...},
             ArrayOffset {base = b', index = i', ...}) =>
                equals (b, b') andalso equals (i, i') 
           | (Cast (z, t), Cast (z', t')) =>
                Type.equals (t, t') andalso equals (z, z')
           | (Contents {oper = z, ...}, Contents {oper = z', ...}) =>
                equals (z, z')
           | (GCState, GCState) => true
           | (Global g, Global g') => Global.equals (g, g')
           | (Label l, Label l') => Label.equals (l, l')
           | (Offset {base = b, offset = i, ...},
              Offset {base = b', offset = i', ...}) =>
                equals (b, b') andalso Bytes.equals (i, i')
           | (Real r, Real r') => RealX.equals (r, r')
           | (Register r, Register r') => Register.equals (r, r')
           | (StackOffset so, StackOffset so') => StackOffset.equals (so, so')
           | (Word w, Word w') => WordX.equals (w, w')
           | _ => false

      val stackOffset = StackOffset o StackOffset.T

      fun interfere (write: t, read: t): bool =
         let
            fun inter read = interfere (write, read)
         in
            case (read, write) of
               (Cast (z, _), _) => interfere (write, z)
             | (_, Cast (z, _)) => interfere (z, read)
             | (ArrayOffset {base, index, ...}, _) => 
                  inter base orelse inter index
             | (Contents {oper, ...}, _) => inter oper
             | (Global g, Global g') => Global.equals (g, g')
             | (Offset {base, ...}, _) => inter base
             | (Register r, Register r') => Register.equals (r, r')
             | (StackOffset so, StackOffset so') =>
                  StackOffset.interfere (so, so')
             | _ => false
         end

      val rec isLocation =
         fn ArrayOffset _ => true
          | Cast (z, _) => isLocation z
          | Contents _ => true
          | GCState => true
          | Global _ => true
          | Offset _ => true
          | Register _ => true
          | StackOffset _ => true
          | _ => false
   end

structure Switch = Switch (open Atoms
                           structure Type = Type
                           structure Use = Operand)

structure Statement =
   struct
      datatype t =
         Move of {dst: Operand.t,
                  src: Operand.t}
       | Noop
       | PrimApp of {args: Operand.t vector,
                     dst: Operand.t option,
                     prim: Type.t Prim.t}
       | ProfileLabel of ProfileLabel.t

      val layout =
         let
            open Layout
         in
            fn Move {dst, src} =>
                  mayAlign [Operand.layout dst,
                            seq [str " = ", Operand.layout src]]
             | Noop => str "Noop"
             | PrimApp {args, dst, prim, ...} =>
                  let
                     val rest =
                        seq [Prim.layout prim, str " ",
                             Vector.layout Operand.layout args]
                  in
                     case dst of
                        NONE => rest
                      | SOME z =>
                           mayAlign [Operand.layout z,
                                     seq [str " = ", rest]]
                  end
             | ProfileLabel l =>
                  seq [str "ProfileLabel ", ProfileLabel.layout l]
         end

      fun move (arg as {dst, src}) =
         if Operand.equals (dst, src)
            then Noop
         else Move arg

      val move =
         Trace.trace ("Machine.Statement.move",
                      fn {dst, src} =>
                      Layout.record [("dst", Operand.layout dst),
                                     ("src", Operand.layout src)],
                      layout)
         move

      fun moves {srcs, dsts} =
         Vector.fromListRev
         (Vector.fold2 (srcs, dsts, [], fn (src, dst, ac)  =>
                        move {src = src, dst = dst} :: ac))

      fun object {dst, header, size} =
         let
            datatype z = datatype Operand.t
            fun bytes (b: Bytes.t): Operand.t =
               Word (WordX.fromIntInf (Bytes.toIntInf b, WordSize.csize ()))
            val temp = Register (Register.new (Type.cpointer (), NONE))
         in
            Vector.new4
            (Move {dst = Contents {oper = Frontier,
                                   ty = Type.objptrHeader ()},
                   src = Word (WordX.fromIntInf (Word.toIntInf header,
                                                 WordSize.objptrHeader ()))},
             PrimApp {args = Vector.new2 (Frontier,
                                          bytes (Runtime.headerSize ())),
                      dst = SOME temp,
                      prim = Prim.cpointerAdd},
             (* CHECK; if objptr <> cpointer, need non-trivial coercion here. *)
             Move {dst = dst, src = Cast (temp, Operand.ty dst)},
             PrimApp {args = Vector.new2 (Frontier, bytes size),
                      dst = SOME Frontier,
                      prim = Prim.cpointerAdd})
         end

      fun foldOperands (s, ac, f) =
         case s of
            Move {dst, src} => f (dst, f (src, ac))
          | PrimApp {args, dst, ...} =>
               Vector.fold (args, Option.fold (dst, ac, f), f)
          | _ => ac

      fun foldDefs (s, a, f) =
         case s of
            Move {dst, ...} => f (dst, a)
          | PrimApp {dst, ...} => (case dst of
                                      NONE => a
                                    | SOME z => f (z, a))
          | _ => a
   end

structure FrameInfo =
   struct
      datatype t = T of {frameLayoutsIndex: int}

      fun layout (T {frameLayoutsIndex, ...}) =
         Layout.record [("frameLayoutsIndex", Int.layout frameLayoutsIndex)]

      fun equals (T {frameLayoutsIndex = i}, T {frameLayoutsIndex = i'}) =
         i = i'
   end

structure Live =
   struct
      datatype t =
         Global of Global.t
       | Register of Register.t
       | StackOffset of StackOffset.t

      val layout: t -> Layout.t =
         fn Global g => Global.layout g
          | Register r => Register.layout r
          | StackOffset s => StackOffset.layout s

      val equals: t * t -> bool =
         fn (Global g, Global g') => Global.equals (g, g')
          | (Register r, Register r') => Register.equals (r, r')
          | (StackOffset s, StackOffset s') => StackOffset.equals (s, s')
          | _ => false

      val ty =
         fn Global g => Global.ty g
          | Register r => Register.ty r
          | StackOffset s => StackOffset.ty s

      val isSubtype: t * t -> bool =
         fn (Global g, Global g') => Global.isSubtype (g, g')
          | (Register r, Register r') => Register.isSubtype (r, r')
          | (StackOffset s, StackOffset s') => StackOffset.isSubtype (s, s')
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
          | Operand.Register r => SOME (Register r)
          | Operand.StackOffset s => SOME (StackOffset s)
          | _ => NONE

      val toOperand: t -> Operand.t =
         fn Global g => Operand.Global g
          | Register r => Operand.Register r
          | StackOffset s => Operand.StackOffset s
   end

structure Transfer =
   struct
      datatype t =
         Arith of {args: Operand.t vector,
                   dst: Operand.t,
                   overflow: Label.t,
                   prim: Type.t Prim.t,
                   success: Label.t}
       | CCall of {args: Operand.t vector,
                   frameInfo: FrameInfo.t option,
                   func: Type.t CFunction.t,
                   return: Label.t option}
       | Call of {label: Label.t,
                  live: Live.t vector,
                  return: {return: Label.t,
                           handler: Label.t option,
                           size: Bytes.t} option}
       | Goto of Label.t
       | Raise
       | Return
       | Switch of Switch.t

      fun layout t =
         let
            open Layout
         in
            case t of
               Arith {prim, args, dst, overflow, success, ...} =>
                  seq [str "Arith ",
                       record [("prim", Prim.layout prim),
                               ("args", Vector.layout Operand.layout args),
                               ("dst", Operand.layout dst),
                               ("overflow", Label.layout overflow),
                               ("success", Label.layout success)]]
             | CCall {args, frameInfo, func, return} =>
                  seq [str "CCall ",
                       record
                       [("args", Vector.layout Operand.layout args),
                        ("frameInfo", Option.layout FrameInfo.layout frameInfo),
                        ("func", CFunction.layout (func, Type.layout)),
                        ("return", Option.layout Label.layout return)]]
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
             | Raise => str "Raise"
             | Return => str "Return "
             | Switch s => Switch.layout s
         end

       fun foldOperands (t, ac, f) =
         case t of
            Arith {args, dst, ...} => Vector.fold (args, f (dst, ac), f)
          | CCall {args, ...} => Vector.fold (args, ac, f)
          | Switch s =>
               Switch.foldLabelUse
               (s, ac, {label = fn (_, a) => a,
                        use = f})
          | _ => ac

       fun foldDefs (t, a, f) =
         case t of
            Arith {dst, ...} => f (dst, a)
          | _ => a
   end

structure Kind =
   struct
      datatype t =
         Cont of {args: Live.t vector,
                  frameInfo: FrameInfo.t}
       | CReturn of {dst: Live.t option,
                     frameInfo: FrameInfo.t option,
                     func: Type.t CFunction.t}
       | Func
       | Handler of {frameInfo: FrameInfo.t,
                     handles: Live.t vector}
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
             | Func => str "Func"
             | Handler {frameInfo, handles} =>
                  seq [str "Handler ",
                       record [("frameInfo", FrameInfo.layout frameInfo),
                               ("handles",
                                Vector.layout Live.layout handles)]]
             | Jump => str "Jump"
         end

      val frameInfoOpt =
         fn Cont {frameInfo, ...} => SOME frameInfo
          | CReturn {frameInfo, ...} => frameInfo
          | Handler {frameInfo, ...} => SOME frameInfo
          | _ => NONE
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

      fun layout (T {kind, label, live, raises, returns, statements, transfer}) =
         let
            open Layout
         in
            align [seq [Label.layout label, 
                        str ": ",
                        record [("kind", Kind.layout kind),
                                ("live", Vector.layout Live.layout live),
                                ("raises",
                                 Option.layout (Vector.layout Live.layout)
                                 raises),
                                ("returns",
                                 Option.layout (Vector.layout Live.layout)
                                 returns)]],
                   indent (align
                           [align (Vector.toListMap
                                   (statements, Statement.layout)),
                            Transfer.layout transfer],
                           4)]
         end

      fun layouts (block, output' : Layout.t -> unit) = output' (layout block)

      fun foldDefs (T {kind, statements, transfer, ...}, a, f) =
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
            val a = Transfer.foldDefs (transfer, a, f)
         in
            a
         end
   end

structure Chunk =
   struct
      datatype t = T of {blocks: Block.t vector,
                         chunkLabel: ChunkLabel.t,
                         regMax: CType.t -> int}

      fun layouts (T {blocks, ...}, output : Layout.t -> unit) =
         Vector.foreach (blocks, fn block => Block.layouts (block, output))

      fun clear (T {blocks, ...}) =
         Vector.foreach (blocks, Block.clear)
   end

structure ProfileInfo =
   struct
      datatype t =
         T of {frameSources: int vector,
               labels: {label: ProfileLabel.t,
                        sourceSeqsIndex: int} vector,
               names: string vector,
               sourceSeqs: int vector vector,
               sources: {nameIndex: int,
                         successorsIndex: int} vector}

      val empty = T {frameSources = Vector.new0 (),
                     labels = Vector.new0 (),
                     names = Vector.new0 (),
                     sourceSeqs = Vector.new0 (),
                     sources = Vector.new0 ()}

      fun clear (T {labels, ...}) =
         Vector.foreach (labels, ProfileLabel.clear o #label)

      fun layout (T {frameSources, labels, names, sourceSeqs, sources}) =
         Layout.record
         [("frameSources", Vector.layout Int.layout frameSources),
          ("labels",
           Vector.layout (fn {label, sourceSeqsIndex} =>
                          Layout.record
                          [("label", ProfileLabel.layout label),
                           ("sourceSeqsIndex",
                            Int.layout sourceSeqsIndex)])
           labels),
          ("names", Vector.layout String.layout names),
          ("sourceSeqs", Vector.layout (Vector.layout Int.layout) sourceSeqs),
          ("sources",
           Vector.layout (fn {nameIndex, successorsIndex} =>
                          Layout.record [("nameIndex", Int.layout nameIndex),
                                         ("successorsIndex",
                                          Int.layout successorsIndex)])
           sources)]

      fun layouts (pi, output) = output (layout pi)

      fun isOK (T {frameSources, labels, names, sourceSeqs, sources}): bool =
         let
            val namesLength = Vector.length names
            val sourceSeqsLength = Vector.length sourceSeqs
            val sourcesLength = Vector.length sources
         in
            !Control.profile = Control.ProfileNone
            orelse
            (Vector.forall (frameSources, fn i =>
                            0 <= i andalso i < sourceSeqsLength)
             andalso (Vector.forall
                      (labels, fn {sourceSeqsIndex = i, ...} =>
                       0 <= i andalso i < sourceSeqsLength))
             andalso (Vector.forall
                      (sourceSeqs, fn v =>
                       Vector.forall
                       (v, fn i => 0 <= i andalso i < sourcesLength)))
             andalso (Vector.forall
                      (sources, fn {nameIndex, successorsIndex} =>
                       0 <= nameIndex
                       andalso nameIndex < namesLength
                       andalso 0 <= successorsIndex
                       andalso successorsIndex < sourceSeqsLength)))
         end

       fun modify (T {frameSources, labels, names, sourceSeqs, sources})
          : {newProfileLabel: ProfileLabel.t -> ProfileLabel.t,
             delProfileLabel: ProfileLabel.t -> unit,
             getProfileInfo: unit -> t} =
          let
             val {get: ProfileLabel.t -> int, set, ...} =
                Property.getSet
                (ProfileLabel.plist, 
                 Property.initRaise ("ProfileInfo.extend", ProfileLabel.layout))
             val _ =
                Vector.foreach
                (labels, fn {label, sourceSeqsIndex} =>
                 set (label, sourceSeqsIndex))
             val new = ref []
             fun newProfileLabel l =
               let
                  val i = get l
                  val l' = ProfileLabel.new ()
                  val _ = set (l', i)
                  val _ = List.push (new, {label = l', sourceSeqsIndex = i})
               in
                  l'
               end
             fun delProfileLabel l = set (l, ~1)
             fun getProfileInfo () =
                let
                   val labels = Vector.concat
                                [labels, Vector.fromList (!new)]
                   val labels = Vector.keepAll
                                (labels, fn {label, ...} =>
                                 get label <> ~1)
                   val pi = T {frameSources = frameSources,
                               labels = Vector.concat
                                        [labels, Vector.fromList (!new)],
                               names = names,
                               sourceSeqs = sourceSeqs,
                               sources = sources}
                in
                  Assert.assert ("Machine.getProfileInfo", fn () => isOK pi);
                  pi
                end
          in
             {newProfileLabel = newProfileLabel,
              delProfileLabel = delProfileLabel,
              getProfileInfo = getProfileInfo}
          end
   end

structure Program =
   struct
      datatype t = T of {chunks: Chunk.t list,
                         frameLayouts: {frameOffsetsIndex: int,
                                        isC: bool,
                                        size: Bytes.t} vector,
                         frameOffsets: Bytes.t vector vector,
                         handlesSignals: bool,
                         main: {chunkLabel: ChunkLabel.t,
                                label: Label.t},
                         maxFrameSize: Bytes.t,
                         objectTypes: ObjectType.t vector,
                         profileInfo: ProfileInfo.t option,
                         reals: (Global.t * RealX.t) list,
                         vectors: (Global.t * WordXVector.t) list}

      fun clear (T {chunks, profileInfo, ...}) =
         (List.foreach (chunks, Chunk.clear)
          ; Option.app (profileInfo, ProfileInfo.clear))

      fun frameSize (T {frameLayouts, ...},
                     FrameInfo.T {frameLayoutsIndex, ...}) =
         #size (Vector.sub (frameLayouts, frameLayoutsIndex))

      fun layouts (T {chunks, frameLayouts, frameOffsets, handlesSignals,
                      main = {label, ...},
                      maxFrameSize, objectTypes, profileInfo, ...},
                   output': Layout.t -> unit) =
         let
            open Layout
            val output = output'
         in
            output (record
                    [("handlesSignals", Bool.layout handlesSignals),
                     ("main", Label.layout label),
                     ("maxFrameSize", Bytes.layout maxFrameSize),
                     ("frameOffsets",
                      Vector.layout (Vector.layout Bytes.layout) frameOffsets),
                     ("frameLayouts",
                      Vector.layout (fn {frameOffsetsIndex, isC, size} =>
                                     record [("frameOffsetsIndex",
                                              Int.layout frameOffsetsIndex),
                                             ("isC", Bool.layout isC),
                                             ("size", Bytes.layout size)])
                      frameLayouts)])
            ; Option.app (profileInfo, fn pi =>
                          (output (str "\nProfileInfo:")
                           ; ProfileInfo.layouts (pi, output)))
            ; output (str "\nObjectTypes:")
            ; Vector.foreachi (objectTypes, fn (i, ty) =>
                               output (seq [str "opt_", Int.layout i,
                                            str " = ", ObjectType.layout ty]))
            ; output (str "\n")
            ; List.foreach (chunks, fn chunk => Chunk.layouts (chunk, output))
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
                     T {chunks, frameLayouts, frameOffsets,
                        maxFrameSize, objectTypes, profileInfo, reals,
                        vectors, ...}) =
         let
            val _ =
               if !Control.profile = Control.ProfileTimeLabel
                  then
                     List.foreach
                     (chunks, fn Chunk.T {blocks, ...} =>
                      Vector.foreach
                      (blocks, fn Block.T {kind, label, statements, ...} =>
                       if (case kind of
                              Kind.Func => true
                            | _ => false)
                          orelse (0 < Vector.length statements
                                  andalso (case Vector.sub (statements, 0) of
                                              Statement.ProfileLabel _ => true
                                            | _ => false))
                          then ()
                       else print (concat ["missing profile info: ",
                                           Label.toString label, "\n"])))
               else ()
            val profileLabelIsOk =
               case profileInfo of
                  NONE =>
                     if !Control.profile = Control.ProfileNone
                        then fn _ => false
                     else Error.bug 
                          "Machine.Program.typeCheck.profileLabelIsOk: profileInfo = NONE"
                | SOME (ProfileInfo.T {frameSources,
                                       labels = profileLabels, ...}) =>
                     if !Control.profile = Control.ProfileNone
                        orelse (Vector.length frameSources
                                <> Vector.length frameLayouts)
                        then Error.bug 
                             "Machine.Program.typeCheck.profileLabelIsOk: profileInfo = SOME"
                     else
                        let
                           val {get = profileLabelCount, ...} =
                              Property.get
                              (ProfileLabel.plist,
                               Property.initFun (fn _ => ref 0))
                           val _ =
                              Vector.foreach
                              (profileLabels, fn {label, ...} =>
                               let
                                  val r = profileLabelCount label
                               in
                                  if 0 = !r
                                     then r := 1
                                  else Error.bug 
                                       "Machine.Program.typeCheck.profileLabelIsOk: duplicate profile label"
                               end)
                        in
                           fn l =>
                           let
                              val r = profileLabelCount l
                           in
                              if 1 = !r 
                                 then (r := 2; true)
                              else false
                           end
                        end
            fun getFrameInfo (FrameInfo.T {frameLayoutsIndex, ...}) =
               Vector.sub (frameLayouts, frameLayoutsIndex)
            val _ =
               Vector.foreach
               (frameLayouts, fn {frameOffsetsIndex, size, ...} =>
                Err.check
                ("frameLayouts",
                 fn () => (0 <= frameOffsetsIndex
                           andalso frameOffsetsIndex < Vector.length frameOffsets
                           andalso Bytes.<= (size, maxFrameSize)
                           andalso Bytes.<= (size, Runtime.maxFrameSize)
                           andalso Bytes.isWord32Aligned size),
                 fn () => Layout.record [("frameOffsetsIndex",
                                          Int.layout frameOffsetsIndex),
                                         ("size", Bytes.layout size)]))
            val _ =
               Vector.foreach
               (objectTypes, fn ty =>
                Err.check ("objectType",
                           fn () => ObjectType.isOk ty,
                           fn () => ObjectType.layout ty))
            fun tyconTy (opt: ObjptrTycon.t): ObjectType.t =
               Vector.sub (objectTypes, ObjptrTycon.index opt)
            open Layout
            fun globals (name, gs, isOk, layout) =
               List.foreach
               (gs, fn (g, s) =>
                let
                   val ty = Global.ty g
                in
                   Err.check
                   (concat ["global ", name],
                    fn () => isOk (ty, s),
                    fn () => seq [layout s, str ": ", Type.layout ty])
                end)
            val _ =
               globals ("real", reals,
                        fn (t, r) => Type.equals (t, Type.real (RealX.size r)),
                        RealX.layout)
            val _ =
               globals ("vector", vectors,
                        fn (t, v) =>
                        Type.equals (t, Type.ofWordXVector v),
                        WordXVector.layout)
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
            fun checkOperand (x: Operand.t, alloc: Alloc.t): unit =
               let
                  datatype z = datatype Operand.t
                  fun ok () =
                     case x of
                        ArrayOffset {base, index, offset, scale, ty} =>
                           (checkOperand (base, alloc)
                            ; checkOperand (index, alloc)
                            ; (Operand.isLocation base
                               andalso
                               (Type.arrayOffsetIsOk {base = Operand.ty base,
                                                      index = Operand.ty index,
                                                      offset = offset,
                                                      tyconTy = tyconTy,
                                                      result = ty,
                                                      scale = scale})))
                      | Cast (z, t) =>
                           (checkOperand (z, alloc)
                            ; (Type.castIsOk
                               {from = Operand.ty z,
                                to = t,
                                tyconTy = tyconTy}))
                      | Contents {oper, ...} =>
                           (checkOperand (oper, alloc)
                            ; Type.isCPointer (Operand.ty oper))
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
                      | Null => true
                      | Offset {base, offset, ty} =>
                           (checkOperand (base, alloc)
                            ; (Operand.isLocation base
                               andalso
                               (case base of
                                  Operand.GCState => true
                                | _ => 
                                     Type.offsetIsOk {base = Operand.ty base,
                                                      offset = offset,
                                                      tyconTy = tyconTy,
                                                      result = ty})))
                      | Real _ => true
                      | Register r => Alloc.doesDefine (alloc, Live.Register r)
                      | StackOffset (so as StackOffset.T {offset, ty, ...}) =>
                           Bytes.<= (Bytes.+ (offset, Type.bytes ty),
                                     maxFrameSize)
                           andalso Alloc.doesDefine (alloc, Live.StackOffset so)
                           andalso (case Type.deLabel ty of
                                       NONE => true
                                     | SOME l =>
                                          let
                                             val Block.T {kind, ...} =
                                                labelBlock l
                                             fun doit fi =
                                                let
                                                   val {size, ...} =
                                                      getFrameInfo fi
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
                                              | Kind.Func => true
                                              | Kind.Handler {frameInfo, ...} =>
                                                   doit frameInfo
                                              | Kind.Jump => true
                                          end)
                      | StackTop => true
                      | Word _ => true
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
                  fun frame (FrameInfo.T {frameLayoutsIndex},
                             useSlots: bool,
                             isC: bool): bool =
                     let
                        val {frameOffsetsIndex, isC = isC', ...} =
                           Vector.sub (frameLayouts, frameLayoutsIndex)
                           handle Subscript => raise No
                     in
                        isC = isC'
                        andalso
                        (not useSlots
                         orelse
                         let
                            val Alloc.T zs = alloc
                            val liveOffsets =
                               List.fold
                               (zs, [], fn (z, liveOffsets) =>
                                case z of
                                   Live.StackOffset (StackOffset.T {offset, ty}) =>
                                      if Type.isObjptr ty
                                         then offset :: liveOffsets
                                      else liveOffsets
                                 | _ => raise No)
                            val liveOffsets = Array.fromList liveOffsets
                            val () = QuickSort.sortArray (liveOffsets, Bytes.<=)
                            val liveOffsets = Vector.fromArray liveOffsets
                            val liveOffsets' =
                               Vector.sub (frameOffsets, frameOffsetsIndex)
                               handle Subscript => raise No
                         in
                            Vector.equals (liveOffsets, liveOffsets',
                                           Bytes.equals)
                         end)
                     end handle No => false
                  fun slotsAreInFrame (fi: FrameInfo.t): bool =
                     let
                        val {size, ...} = getFrameInfo fi
                     in
                        Alloc.forall
                        (alloc, fn z =>
                         case z of
                            Operand.StackOffset (StackOffset.T {offset, ty}) =>
                               Bytes.<= (Bytes.+ (offset, Type.bytes ty), size)
                          | _ => false)
                     end
               in
                  case k of
                     Cont {args, frameInfo} =>
                        if frame (frameInfo, true, false)
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
                                              (frame (fi, true, true)
                                               andalso slotsAreInFrame fi))
                               else if !Control.profile = Control.ProfileNone
                                       then true
                                    else (case frameInfo of
                                             NONE => false
                                           | SOME fi => frame (fi, false, true)))
                        in
                           if ok
                              then SOME (case dst of
                                            NONE => alloc
                                          | SOME z => Alloc.defineLive (alloc, z))
                           else NONE
                        end
                   | Func => SOME alloc
                   | Handler {frameInfo, ...} =>
                        if frame (frameInfo, false, false)
                           then SOME alloc
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
                              andalso Operand.isLocation dst
                              then SOME alloc
                           else NONE
                        end
                   | Noop => SOME alloc
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
                   | ProfileLabel l =>
                        if profileLabelIsOk l
                           then SOME alloc
                        else NONE
               end
            fun liveIsOk (live: Live.t vector,
                          a: Alloc.t): bool =
               Vector.forall (live, fn z => Alloc.doesDefine (a, z))
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
            fun checkCont (cont: Label.t, size: Bytes.t, alloc: Alloc.t) =
               let
                  val Block.T {kind, live, ...} = labelBlock cont
               in
                  if Vector.forall (live, fn z => Alloc.doesDefine (alloc, z))
                     then
                        (case kind of
                            Kind.Cont {args, frameInfo, ...} =>
                               (if Bytes.equals (size,
                                                 #size (getFrameInfo frameInfo))
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
                                          val Block.T {kind, live, ...} =
                                             labelBlock h
                                       in
                                          if liveSubset (live, contLive)
                                             then
                                                (case kind of
                                                    Kind.Handler {handles, ...} =>
                                                       SOME (SOME handles)
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
                          Live.StackOffset (StackOffset.T {offset, ty}) =>
                             if Bytes.< (offset, size)
                                then ac
                             else (Live.StackOffset
                                   (StackOffset.T
                                    {offset = Bytes.- (offset, size),
                                     ty = ty})) :: ac
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
                  fun jump (l: Label.t, a: Alloc.t) =
                     let
                        val b as Block.T {kind, ...} = labelBlock l
                     in
                        (case kind of
                            Kind.Jump => true
                          | _ => false)
                            andalso goto (b, raises, returns, a)
                     end
                  datatype z = datatype Transfer.t
               in
                  case t of
                     Arith {args, dst, overflow, prim, success, ...} =>
                        let
                           val _ = checkOperands (args, alloc)
                           val alloc = Alloc.define (alloc, dst)
                           val _ = checkOperand (dst, alloc)
                        in
                           Prim.mayOverflow prim
                           andalso jump (overflow, alloc)
                           andalso jump (success, alloc)
                           andalso
                           Type.checkPrimApp
                           {args = Vector.map (args, Operand.ty),
                            prim = prim,
                            result = SOME (Operand.ty dst)}
                        end
                   | CCall {args, frameInfo = fi, func, return} =>
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
                            | SOME l =>
                                 let 
                                    val Block.T {live, ...} = labelBlock l
                                 in
                                    liveIsOk (live, alloc)
                                    andalso
                                    case labelKind l of
                                       Kind.CReturn
                                       {frameInfo = fi', func = f, ...} => 
                                          CFunction.equals (func, f)
                                          andalso (Option.equals
                                                   (fi, fi', FrameInfo.equals))
                                     | _ => false
                                 end
                        end
                   | Call {label, live, return} =>
                        Vector.forall
                        (live, fn z => Alloc.doesDefine (alloc, z))
                        andalso
                        callIsOk {alloc = alloc,
                                  dst = label,
                                  live = live,
                                  raises = raises,
                                  return = return,
                                  returns = returns}
                      | Goto l => jump (l, alloc)
                      | Raise =>
                           (case raises of
                               NONE => false
                             | SOME zs =>
                                  Vector.forall
                                  (zs, fn z => Alloc.doesDefine (alloc, z)))
                      | Return =>
                           (case returns of
                               NONE => false
                             | SOME zs =>
                                  Vector.forall
                                  (zs, fn z => Alloc.doesDefine (alloc, z)))
                      | Switch s =>
                           Switch.isOk
                           (s, {checkUse = fn z => checkOperand (z, alloc),
                                labelIsOk = fn l => jump (l, alloc)})
               end
            val transferOk =
               Trace.trace
               ("Machine.Program.typeCheck.transferOk",
                fn (t, _, _, a) =>
                Layout.tuple [Transfer.layout t, Alloc.layout a],
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

end
