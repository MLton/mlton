(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RepType (S: REP_TYPE_STRUCTS): REP_TYPE =
struct

open S

structure CFunction = CFunction

type int = Int.t

structure Type =
   struct
      datatype t = T of {node: node,
                         width: Bits.t}
      and node =
          CPointer
        | ExnStack
        | GCState
        | Label of Label.t
        | Objptr of ObjptrTycon.t vector
        | Real of RealSize.t
        | Seq of t vector
        | Word

      local
         fun make f (T r) = f r
      in
         val node = make #node
         val width = make #width
      end
      val bytes: t -> Bytes.t = Bits.toBytes o width

      val rec layout: t -> Layout.t =
         fn t =>
         let
            open Layout
         in
            case node t of
               CPointer => str "CPointer"
             | ExnStack => str "ExnStack"
             | GCState => str "GCState"
             | Label l => seq [str "Label ", Label.layout l]
             | Objptr opts =>
                  seq [str "Objptr ",
                       tuple (Vector.toListMap (opts, ObjptrTycon.layout))]
             | Real s => str (concat ["Real", RealSize.toString s])
             | Seq ts => List.layout layout (Vector.toList ts)
             | Word => str (concat ["Word", Bits.toString (width t)])
         end

      val rec equals: t * t -> bool =
         fn (t, t') =>
         Bits.equals (width t, width t')
         andalso
         (case (node t, node t') of
             (CPointer, CPointer) => true
           | (ExnStack, ExnStack) => true
           | (GCState, GCState) => true
           | (Label l, Label l') => Label.equals (l, l')
           | (Objptr opts, Objptr opts') =>
                Vector.equals (opts, opts', ObjptrTycon.equals)
           | (Real s, Real s') => RealSize.equals (s, s')
           | (Seq ts, Seq ts') => Vector.equals (ts, ts', equals)
           | (Word, Word) => true
           | _ => false)

      val sameWidth: t * t -> bool =
         fn (t, t') => Bits.equals (width t, width t')


      val cpointer: unit -> t = fn () =>
         T {node = CPointer, width = WordSize.bits (WordSize.cpointer ())}

      val exnStack: unit -> t = fn () => 
         T {node = ExnStack, width = WordSize.bits WordSize.exnStack}

      val gcState: unit -> t = fn () => 
         T {node = GCState, width = WordSize.bits (WordSize.cpointer ())}

      val label: Label.t -> t =
         fn l => T {node = Label l, width = WordSize.bits (WordSize.cpointer ())}

      val objptr: ObjptrTycon.t -> t =
         fn opt => T {node = Objptr (Vector.new1 opt),
                      width = WordSize.bits (WordSize.objptr ())}

      val real: RealSize.t -> t =
         fn s => T {node = Real s, width = RealSize.bits s}
 
      val word: Bits.t -> t = fn width => T {node = Word, width = width}


      val bool: t = word (WordSize.bits WordSize.bool)

      val csize: unit -> t = word o WordSize.bits o WordSize.csize

      val cint: unit -> t = word o WordSize.bits o WordSize.cint

      val objptrHeader: unit -> t = word o WordSize.bits o WordSize.objptrHeader

      val seqIndex: unit -> t = word o WordSize.bits o WordSize.seqIndex

      val shiftArg: t = word (WordSize.bits WordSize.shiftArg)

      val stack : unit -> t = fn () => 
         objptr ObjptrTycon.stack

      val thread : unit -> t = fn () => 
         objptr ObjptrTycon.thread

      val word32: t = word (WordSize.bits WordSize.word32)

      val wordVector: Bits.t -> t = objptr o ObjptrTycon.wordVector

      val word8Vector: unit -> t =  fn () => 
         wordVector (WordSize.bits WordSize.word8)

      val string: unit -> t = word8Vector

      val unit: t = word Bits.zero

      val zero: Bits.t -> t = word


      val ofWordX: WordX.t -> t = 
         fn w => word (WordSize.bits (WordX.size w))

      fun ofWordXVector (v: WordXVector.t): t =
         wordVector (WordSize.bits (WordXVector.elementSize v))


      val seq: t vector -> t =
         fn ts =>
         if 0 = Vector.length ts
            then unit
         else
            let
               fun seqOnto (ts, ac) =
                  Vector.foldr
                  (ts, ac, fn (t, ac) =>
                   case ac of
                      [] => [t]
                    | t' :: ac' =>
                         (case (node t, node t') of
                             (Seq ts, _) => seqOnto (ts, ac)
                           | (Word, Word) => word (Bits.+ (width t, width t')) :: ac'
                           | _ => t :: ac))
            in
               case seqOnto (ts, []) of
                  [t] => t
                | ts =>
                     let
                        val ts = Vector.fromList ts
                     in
                        T {node = Seq ts,
                           width = Vector.fold (ts, Bits.zero, fn (t, ac) =>
                                                Bits.+ (ac, width t))}
                     end
            end

      val seq = Trace.trace ("RepType.Type.seq", Vector.layout layout, layout) seq

      val sum: t vector -> t =
         fn ts =>
         if 0 = Vector.length ts
            then Error.bug "RepType.Type.sum: empty"
         else
            let
               val opts =
                  Vector.concatV
                  (Vector.keepAllMap
                   (ts, fn t =>
                    case node t of
                       Objptr opts => SOME opts
                     | _ => NONE))
            in
               if 0 = Vector.length opts
                  then Vector.sub (ts, 0)
               else
                  T {node = (Objptr (QuickSort.sortVector (opts, ObjptrTycon.<=))),
                     width = WordSize.bits (WordSize.objptr ())}
            end

      val sum = Trace.trace ("RepType.Type.sum", Vector.layout layout, layout) sum

      val intInf: unit -> t = fn () =>
         sum (Vector.new2
              (wordVector (WordSize.bits (WordSize.bigIntInfWord ())),
               seq (Vector.new2
                    (word Bits.one,
                     word (Bits.- (WordSize.bits (WordSize.smallIntInfWord ()), 
                                   Bits.one))))))


      val deLabel: t -> Label.t option =
         fn t =>
         case node t of
            Label l => SOME l
          | _ => NONE

      val deObjptr: t -> ObjptrTycon.t option =
         fn t => 
         case node t of
            Objptr opts =>
               if 1 = Vector.length opts
                  then SOME (Vector.sub (opts, 0))
               else NONE
          | _ => NONE

      val deReal: t -> RealSize.t option =
         fn t =>
         case node t of
            Real s => SOME s
          | _ => NONE

      val isCPointer: t -> bool =
         fn t =>
         case node t of
            CPointer => true
          | _ => false

      val isObjptr: t -> bool =
         fn t =>
         case node t of
            Objptr _ => true
          | _ => false

      val isUnit: t -> bool = fn t => Bits.equals (Bits.zero, width t)

      val isSubtype: t * t -> bool =
         fn (t, t') =>
         if not (sameWidth (t, t'))
            then Error.bug "RepType.Type.isSubtype"
         else
            (equals (t, t')
             orelse
             case (node t, node t') of
                (Objptr opts, Objptr opts') =>
                   Vector.isSubsequence (opts, opts', ObjptrTycon.equals)
              | (Real _, _) => false
              | (Word, Objptr _) => true
              | (_, Word) => true
              | _ => false)

      val isSubtype =
         Trace.trace2 ("RepType.Type.isSubtype", layout, layout, Bool.layout)
         isSubtype


      val resize: t * Bits.t -> t = fn (_, b) => word b

      val bogusWord: t -> WordX.t =
         fn t => WordX.one (WordSize.fromBits (width t))

      local
         structure C =
            struct
               open CType

               fun fromBits (b: Bits.t): t =
                  case Bits.toInt b of
                     8 => Word8
                   | 16 => Word16
                   | 32 => Word32
                   | 64 => Word64
                   | _ => Error.bug (concat ["RepType.Type.CType.fromBits: ",
                                             Bits.toString b])
            end
      in
         val toCType: t -> CType.t =
            fn t =>
            if isObjptr t
               then C.Objptr
            else 
               case node t of
                  CPointer => C.CPointer
                | Real s =>
                     (case s of
                         RealSize.R32 => C.Real32
                       | RealSize.R64 => C.Real64)
                | _ => C.fromBits (width t)
                         
         val name = C.name o toCType
            
         val align: t * Bytes.t -> Bytes.t =
            fn (t, n) => C.align (toCType t, n)
      end

      fun bytesAndObjptrs (t: t): Bytes.t * int =
         case node t of
            Objptr _ => (Bytes.zero, 1)
          | Seq ts =>
               (case Vector.peeki (ts, isObjptr o #2) of
                   NONE => (bytes t, 0)
                 | SOME (i, _) =>
                      let
                         val b = bytes (seq (Vector.prefix (ts, i)))
                         val j = (Vector.length ts) - i
                      in
                         (b, j)
                      end)
          | _ => (bytes t, 0)
   end

structure ObjectType =
   struct
      structure ObjptrTycon = ObjptrTycon
      structure Runtime = Runtime

      type ty = Type.t
      datatype t =
         Array of {elt: ty,
                   hasIdentity: bool}
       | Normal of {hasIdentity: bool,
                    ty: ty}
       | Stack
       | Weak of Type.t
       | WeakGone

      fun layout (t: t) =
         let
            open Layout
         in
            case t of
               Array {elt, hasIdentity} =>
                  seq [str "Array ",
                       record [("elt", Type.layout elt),
                               ("hasIdentity", Bool.layout hasIdentity)]]
             | Normal {hasIdentity, ty} =>
                  seq [str "Normal ",
                       record [("hasIdentity", Bool.layout hasIdentity),
                               ("ty", Type.layout ty)]]
             | Stack => str "Stack"
             | Weak t => seq [str "Weak ", Type.layout t]
             | WeakGone => str "WeakGone"
         end

      fun isOk (t: t): bool =
         case t of
            Array {elt, ...} =>
               let
                  val b = Type.width elt
               in
                  Bits.> (b, Bits.zero)
                  andalso Bits.isByteAligned b
               end
          | Normal {ty, ...} =>
               not (Type.isUnit ty) 
               andalso Bits.isWord32Aligned (Type.width ty)
          | Stack => true
          | Weak t => Type.isObjptr t
          | WeakGone => true

      val stack = Stack

      val thread = fn () =>
         Normal {hasIdentity = true,
                 ty = Type.seq (Vector.new3 (Type.csize (),
                                             Type.exnStack (),
                                             Type.stack ()))}

      (* Order in the following vector matters.  The basic pointer tycons must
       * correspond to the constants in gc.h.
       * STACK_TYPE_INDEX,
       * THREAD_TYPE_INDEX,
       * WEAK_GONE_TYPE_INDEX,
       * WORD8_VECTOR_TYPE_INDEX,
       * WORD16_VECTOR_TYPE_INDEX,
       * WORD32_VECTOR_TYPE_INDEX.
       * WORD64_VECTOR_TYPE_INDEX.
       *)
      val basic = fn () => 
         let
            fun wordVec i =
               let
                  val b = Bits.fromInt i
               in
                  (ObjptrTycon.wordVector b,
                   Array {hasIdentity = false,
                          elt = Type.word b})
               end
         in
            Vector.fromList
            [(ObjptrTycon.stack, stack),
             (ObjptrTycon.thread, thread ()),
             (ObjptrTycon.weakGone, WeakGone),
             wordVec 8,
             wordVec 32,
             wordVec 16,
             wordVec 64]
         end

      local
         structure R = Runtime.RObjectType
      in
         fun toRuntime (t: t): R.t =
            case t of
               Array {elt, hasIdentity} =>
                  let
                     val (b, nops) = Type.bytesAndObjptrs elt
                  in
                     R.Array {hasIdentity = hasIdentity,
                              bytesNonObjptrs = b,
                              numObjptrs = nops}
                  end
             | Normal {hasIdentity, ty} =>
                  let
                     val (b, nops) = Type.bytesAndObjptrs ty
                  in
                     R.Normal {hasIdentity = hasIdentity,
                               bytesNonObjptrs = b,
                               numObjptrs = nops}
                  end
             | Stack => R.Stack
             | Weak _ => R.Weak
             | WeakGone => R.WeakGone
      end
   end

open Type

structure GCField = Runtime.GCField

fun ofGCField (f: GCField.t): t =
   let
      datatype z = datatype GCField.t
   in
      case f of
         CanHandle => word32
       | CardMap => cpointer ()
       | CurrentThread => thread ()
       | CurSourceSeqsIndex => word32
       | ExnStack => exnStack ()
       | Frontier => cpointer ()
       | Limit => cpointer ()
       | LimitPlusSlop => cpointer ()
       | MaxFrameSize => word32
       | SignalIsPending => word32
       | StackBottom => cpointer ()
       | StackLimit => cpointer ()
       | StackTop => cpointer ()
   end

fun arrayOffsetIsOk {base,index, offset, tyconTy, result, scale} = 
   case (base, index, offset, tyconTy, result, scale) of _ => true

fun castIsOk {from, to, tyconTy = _} =
   Bits.equals (width from, width to)

fun checkPrimApp {args, prim, result} = 
    case (args, Prim.name prim, result) of _ => true

fun offsetIsOk {base, offset, tyconTy, result} = 
   case (base, offset, tyconTy, result) of _ => true



structure BuiltInCFunction =
   struct
      open CFunction

      datatype z = datatype Convention.t
      datatype z = datatype Target.t

      fun bug () = 
         vanilla {args = Vector.new1 (string ()),
                  name = "MLton_bug",
                  prototype = (Vector.new1 CType.cpointer, NONE),
                  return = unit}

      local
         open Type
      in
         val Word32 = word (Bits.fromInt 32)
         val unit = unit
      end

      local
         fun make b = fn () =>
            T {args = let
                         open Type
                      in
                         Vector.new5 (gcState (), Word32, bool, cpointer (), Word32)
                      end,
                   bytesNeeded = NONE,
                   convention = Cdecl,
                   ensuresBytesFree = true,
                   mayGC = true,
                   maySwitchThreads = b,
                   modifiesFrontier = true,
                   prototype = let
                                  open CType
                               in
                                  (Vector.new5 (cpointer, Word32, bool, cpointer, Word32),
                                   NONE)
                               end,
                   readsStackTop = true,
                   return = unit,       
                   target = Direct "GC_collect",
                   writesStackTop = true}
         val t = make true
         val f = make false
      in
         fun gc {maySwitchThreads = b} = if b then t () else f ()
      end
   end

end
