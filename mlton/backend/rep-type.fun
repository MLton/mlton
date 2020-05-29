(* Copyright (C) 2009-2010,2014,2016-2017,2019-2020 Matthew Fluet.
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor RepType (S: REP_TYPE_STRUCTS): REP_TYPE =
struct

open S

structure CFunction = CFunction

structure Type =
   struct
      datatype t = T of {node: node,
                         width: Bits.t}
      and node =
          Bits
        | CPointer
        | Label of Label.t
        | Objptr of ObjptrTycon.t vector
        | Real of RealSize.t
        | Seq of t vector
        | Word of WordSize.t

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
               Bits => str (concat ["Bits", Bits.toString (width t)])
             | CPointer => str "CPointer"
             | Label l => seq [str "Label ", Label.layout l]
             | Objptr opts =>
                  seq [str "Objptr ",
                       tuple (Vector.toListMap (opts, ObjptrTycon.layout))]
             | Real s => str (concat ["Real", RealSize.toString s])
             | Seq ts => List.layout layout (Vector.toList ts)
             | Word s => str (concat ["Word", WordSize.toString s])
         end

      val rec equals: t * t -> bool =
         fn (t, t') =>
         Bits.equals (width t, width t')
         andalso
         (case (node t, node t') of
             (Bits, Bits) => true 
           | (CPointer, CPointer) => true
           | (Label l, Label l') => Label.equals (l, l')
           | (Objptr opts, Objptr opts') =>
                Vector.equals (opts, opts', ObjptrTycon.equals)
           | (Real s, Real s') => RealSize.equals (s, s')
           | (Seq ts, Seq ts') => Vector.equals (ts, ts', equals)
           | (Word s, Word s') => WordSize.equals (s, s')
           | _ => false)

      val sameWidth: t * t -> bool =
         fn (t, t') => Bits.equals (width t, width t')


      val bits: Bits.t -> t =
         fn width => T {node = Bits, width = width}

      val cpointer: unit -> t = fn () =>
         T {node = CPointer, width = WordSize.bits (WordSize.cpointer ())}

      val label: Label.t -> t =
         fn l => T {node = Label l, width = WordSize.bits (WordSize.cpointer ())}

      val objptr: ObjptrTycon.t -> t =
         fn opt => T {node = Objptr (Vector.new1 opt),
                      width = WordSize.bits (WordSize.objptr ())}

      val real: RealSize.t -> t =
         fn s => T {node = Real s, width = RealSize.bits s}
 
      val word: WordSize.t -> t = 
         fn s => T {node = Word s, width = WordSize.bits s}


      val bool: t = word WordSize.bool

      val cint: unit -> t = word o WordSize.cint

      val compareRes = word WordSize.compareRes

      val cptrdiff: unit -> t = word o WordSize.cptrdiff

      val csize: unit -> t = word o WordSize.csize

      val exnStack: unit -> t = cptrdiff

      val gcState: unit -> t = cpointer

      local
         val b = Random.word ()
         val cpointer = Random.word ()
         val label = Random.word ()
         val objptr = Random.word ()
      in
         fun hash (T {node, width}) =
            case node of
                 Bits => Hash.combine (b, Bits.toWord width)
               | CPointer => cpointer
               | Label l => Hash.combine (label, Label.hash l)
               | Objptr os => Hash.combine (objptr,
                  Hash.vectorMap (os, ObjptrTycon.hash))
               | Real rs => RealSize.hash rs
               | Seq ts => Hash.vectorMap (ts, hash)
               | Word ws => WordSize.hash ws
      end

      val objptrHeader: unit -> t = word o WordSize.objptrHeader

      val seqIndex: unit -> t = word o WordSize.seqIndex

      val shiftArg: t = word WordSize.shiftArg

      val stack : unit -> t = fn () => 
         objptr ObjptrTycon.stack

      val thread : unit -> t = fn () => 
         objptr ObjptrTycon.thread

      val word0: t = bits Bits.zero
      val word8: t = word WordSize.word8
      val word32: t = word WordSize.word32

      val wordVector: WordSize.t -> t = 
         objptr o ObjptrTycon.wordVector

      val word8Vector: unit -> t =  fn () => 
         wordVector WordSize.word8

      val string: unit -> t = word8Vector

      val unit: t = bits Bits.zero

      val zero: Bits.t -> t = bits

      val ofRealX: RealX.t -> t =
         fn r => real (RealX.size r)

      val ofWordX: WordX.t -> t = 
         fn w => word (WordX.size w)

      fun ofWordXVector (v: WordXVector.t): t =
         wordVector (WordXVector.elementSize v)


      val seq: t vector -> t =
         fn ts =>
         if Vector.isEmpty ts
            then unit
         else
            let
               fun seqOnto (ts, ac) =
                  Vector.foldr
                  (ts, ac, fn (t, ac) =>
                   if Bits.equals (width t, Bits.zero)
                      then ac
                   else (case node t of
                            Seq ts => seqOnto (ts, ac)
                          | _ => (case ac of
                                     [] => [t]
                                   | t' :: ac' =>
                                        (case (node t, node t') of
                                            (Bits, Bits) => 
                                               bits (Bits.+ (width t, width t')) :: ac'
                                          | _ => t :: ac))))
            in
               case seqOnto (ts, []) of
                  [] => word0
                | [t] => t
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
         if Vector.isEmpty ts
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
               if Vector.isEmpty opts
                  then Vector.first ts
               else
                  T {node = (Objptr (QuickSort.sortVector (opts, ObjptrTycon.<=))),
                     width = WordSize.bits (WordSize.objptr ())}
            end

      val sum = Trace.trace ("RepType.Type.sum", Vector.layout layout, layout) sum

      val intInf: unit -> t = fn () =>
         sum (Vector.new2
              (wordVector (WordSize.bigIntInfWord ()),
               seq (Vector.new2
                    (bits Bits.one,
                     word (WordSize.fromBits 
                           (Bits.- (WordSize.bits (WordSize.smallIntInfWord ()),
                                    Bits.one)))))))


      fun ofConst (c: Const.t): t =
         let
            datatype z = datatype Const.t
         in
            case c of
               CSymbol _ => cpointer ()
             | IntInf _ => intInf ()
             | Null => cpointer ()
             | Real r => ofRealX r
             | Word w => ofWordX w
             | WordVector v => ofWordXVector v
         end


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
                  then SOME (Vector.first opts)
               else NONE
          | _ => NONE

      val deObjptrs: t -> ObjptrTycon.t vector option =
         fn t =>
         case node t of
            Objptr opts => SOME opts
          | _ => NONE

      val deReal: t -> RealSize.t option =
         fn t =>
         case node t of
            Real s => SOME s
          | _ => NONE
      
      val deSeq: t -> t vector option =
         fn t =>
         case node t of
            Seq v => SOME v
          | _ => NONE

      val deWord: t -> WordSize.t option =
         fn t =>
         case node t of
            Word s => SOME s
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
            then false (* Error.bug "RepType.Type.isSubtype" *)
         else
            (equals (t, t')
             orelse
             case (node t, node t') of
                (Objptr opts, Objptr opts') =>
                   Vector.isSubsequence (opts, opts', ObjptrTycon.equals)
              | (Real _, _) => false
              | (Bits, Objptr _) => true
              | (Word _, Objptr _) => true
              | (Seq ts, Objptr _) =>
                   Vector.forall 
                   (ts, (fn Bits => true 
                          | Real _ => true 
                          | Word _ => true 
                          | _ => false) o node)
              | (_, Bits) => true
              | (_, Word _) => true
              | (_, Seq ts) => 
                   Vector.forall 
                   (ts, (fn Bits => true 
                          | Real _ => true
                          | Word _ => true 
                          | _ => false) o node)
              | _ => false)

      val isSubtype =
         Trace.trace2 ("RepType.Type.isSubtype", layout, layout, Bool.layout)
         isSubtype

      fun exists (t, p) =
         if p t
            then true
         else (case node t of
                  Seq ts => Vector.exists (ts, fn t => exists (t, p))
                | _ => false)


      val resize: t * Bits.t -> t = fn (_, b) => bits b

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
                | Label _ =>
                     (case !Control.codegen of
                         Control.Codegen.AMD64Codegen => C.CPointer
                       | Control.Codegen.CCodegen => C.fromBits (width t)
                       | Control.Codegen.LLVMCodegen => C.fromBits (width t)
                       | Control.Codegen.X86Codegen => C.CPointer)
                | Real s =>
                     (case s of
                         RealSize.R32 => C.Real32
                       | RealSize.R64 => C.Real64)
                | _ => C.fromBits (width t)
                         
         val name = C.name o toCType
            
         val align: t * Bytes.t -> Bytes.t =
            fn (t, n) => C.align (toCType t, n)
      end
   end

structure ObjectType =
   struct
      structure Prod = Prod
      structure ObjptrTycon = ObjptrTycon
      structure Runtime = Runtime

      type ty = Type.t
      datatype t =
         Normal of {components: ty Prod.t,
                    hasIdentity: bool}
       | Sequence of {components: ty Prod.t,
                      hasIdentity: bool}
       | Stack
       | Weak of Type.t option

      fun deNormal t =
         case t of
            Normal {components, hasIdentity} =>
               {components = components, hasIdentity = hasIdentity}
          | _ => Error.bug "ObjectType.deNormal"

      fun deSequence t =
         case t of
            Sequence {components, hasIdentity} =>
               {components = components, hasIdentity = hasIdentity}
          | _ => Error.bug "ObjectType.deSequence"

      fun components t =
         case t of
            Normal {components, ...} => components
          | Sequence {components, ...} => components
          | _ => Error.bug "ObjectType.components"

      fun componentsSize t =
         Prod.fold (components t, Bytes.zero, fn (ty, b) =>
                    Bytes.+ (b, Type.bytes ty))

      fun layout (t: t) =
         let
            open Layout
         in
            case t of
               Normal {components, hasIdentity} =>
                  seq [str "Normal ",
                       record [("components", Prod.layout (components, Type.layout)),
                               ("hasIdentity", Bool.layout hasIdentity)]]
             | Sequence {components, hasIdentity} =>
                  seq [str "Sequence ",
                       record [("components", Prod.layout (components, Type.layout)),
                               ("hasIdentity", Bool.layout hasIdentity)]]
             | Stack => str "Stack"
             | Weak t => seq [str "Weak ", Option.layout Type.layout t]
         end

      fun isOk (t: t): bool =
         let
            fun componentsOk components =
               Exn.withEscape
               (fn escape =>
                ((ignore o Prod.fold)
                 (components, false, fn (ty, hasObjptr) =>
                  if Bits.isPrim (Type.width ty)
                     then if Type.isObjptr ty
                             then true
                             else if hasObjptr
                                     then escape false
                                     else false
                     else escape false)
                 ; true))
         in
            case t of
               Normal {components, ...} =>
                  componentsOk components
                  andalso
                  let
                     val b = Prod.fold (components, Type.width (Type.objptrHeader ()),
                                        fn (ty, b) => Bits.+ (b, Type.width ty))
                  in
                     case !Control.align of
                        Control.Align4 => Bits.isWord32Aligned b
                      | Control.Align8 => Bits.isWord64Aligned b
                  end
             | Sequence {components, ...} => componentsOk components
             | Stack => true
             | Weak to => Option.fold (to, true, fn (t,_) => Type.isObjptr t)
         end
      val stack = Stack

      val thread = fn () =>
         let
            val padding =
               let
                  val align =
                     case !Control.align of
                        Control.Align4 => Bytes.fromInt 4
                      | Control.Align8 => Bytes.fromInt 8
                  val bytesMetaData =
                     Bits.toBytes (Control.Target.Size.normalMetaData ())
                  val bytesCSize =
                     Bits.toBytes (Control.Target.Size.csize ())
                  val bytesExnStack =
                     Bits.toBytes (Type.width (Type.exnStack ()))
                  val bytesStack =
                     Bits.toBytes (Type.width (Type.stack ()))

                  val bytesObject =
                     Bytes.+ (bytesMetaData,
                     Bytes.+ (bytesCSize,
                     Bytes.+ (bytesExnStack,
                              bytesStack)))
                  val bytesTotal =
                     Bytes.align (bytesObject, {alignment = align})
                  val bytesPad = Bytes.- (bytesTotal, bytesObject)
               in
                  Type.bits (Bytes.toBits bytesPad)
               end
            val components =
               Vector.new3 (Type.csize (), Type.exnStack (), Type.stack ())
            val components =
               Vector.map (components, fn ty => {elt = ty, isMutable = true})
            val components =
               if Type.isUnit padding
                  then components
                  else Vector.concat [Vector.new1 {elt = padding, isMutable = false},
                                      components]
         in
            Normal {components = Prod.make components,
                    hasIdentity = true}
         end

      (* Order in the following vector matters.  The basic pointer tycons must
       * correspond to the constants in gc/object.h.
       * STACK_TYPE_INDEX,
       * THREAD_TYPE_INDEX,
       * WEAK_GONE_TYPE_INDEX,
       * REAL32_VECTOR_TYPE_INDEX,
       * REAL64_VECTOR_TYPE_INDEX,
       * WORD8_VECTOR_TYPE_INDEX,
       * WORD16_VECTOR_TYPE_INDEX,
       * WORD32_VECTOR_TYPE_INDEX.
       * WORD64_VECTOR_TYPE_INDEX.
       *)
      val basic = fn () => 
         let
            fun realVec rs =
               (ObjptrTycon.realVector rs,
                Sequence {components = Prod.new1Immutable (Type.real rs),
                          hasIdentity = false})
            fun wordVec ws =
               (ObjptrTycon.wordVector ws,
                Sequence {components = Prod.new1Immutable (Type.word ws),
                          hasIdentity = false})
         in
            Vector.fromList
            [(ObjptrTycon.stack, stack),
             (ObjptrTycon.thread, thread ()),
             (ObjptrTycon.weakGone, Weak NONE),
             realVec RealSize.R32,
             realVec RealSize.R64,
             wordVec WordSize.word8,
             wordVec WordSize.word32,
             wordVec WordSize.word16,
             wordVec WordSize.word64]
         end

      local
         structure R = Runtime.RObjectType
         fun componentsToBytes components =
            Vector.fold
            (components, Bytes.zero, fn ({elt = ty, isMutable = _}, b) =>
             Bytes.+ (Type.bytes ty, b))
         fun componentsToRuntime components =
            let
               val components = Prod.dest components
            in
               case Vector.peeki (components, Type.isObjptr o #elt o #2) of
                  NONE =>
                     {bytesNonObjptrs = componentsToBytes components,
                      numObjptrs = 0}
                | SOME (i, _) =>
                     {bytesNonObjptrs = componentsToBytes (Vector.prefix (components, i)),
                      numObjptrs = Vector.length components - i}
            end
      in
         fun toRuntime (t: t): R.t =
            case t of
               Normal {components, hasIdentity} =>
                  let
                     val {bytesNonObjptrs, numObjptrs} =
                        componentsToRuntime components
                  in
                     R.Normal {hasIdentity = hasIdentity,
                               bytesNonObjptrs = bytesNonObjptrs,
                               numObjptrs = numObjptrs}
                  end
             | Sequence {components, hasIdentity} =>
                  let
                     val {bytesNonObjptrs, numObjptrs} =
                        componentsToRuntime components
                  in
                     R.Sequence {hasIdentity = hasIdentity,
                                 bytesNonObjptrs = bytesNonObjptrs,
                                 numObjptrs = numObjptrs}
                  end
             | Stack => R.Stack
             | Weak to => R.Weak {gone = Option.isNone to}
      end
   end

open Type

structure GCField = Runtime.GCField

fun ofGCField (f: GCField.t): t =
   let
      datatype z = datatype GCField.t
   in
      case f of
         AtomicState => word32
       | CardMapAbsolute => cpointer ()
       | CurSourceSeqIndex => word32
       | ExnStack => exnStack ()
       | Frontier => cpointer ()
       | Limit => cpointer ()
       | LimitPlusSlop => cpointer ()
       | SignalIsPending => word32
       | StackBottom => cpointer ()
       | StackLimit => cpointer ()
       | StackTop => cpointer ()
   end

fun castIsOk {from, to, tyconTy = _} =
   Bits.equals (width from, width to)

fun checkPrimApp {args, prim, result} = 
   let
      fun done (argsP, resultP) =
         let
            val argsP = Vector.fromList argsP
         in
            (Vector.length args = Vector.length argsP)
            andalso (Vector.forall2 (args, argsP, 
                                     fn (arg, argP) => argP arg))
            andalso (case (result, resultP) of
                        (NONE, NONE) => true
                      | (SOME result, SOME resultP) => resultP result
                      | _ => false)
         end
      val bits = fn s => fn t => equals (t, bits s)
      val bool = fn t => equals (t, bool)
      val cpointer = fn t => equals (t, cpointer ())
      val objptr = fn t => (case node t of Objptr _ => true | _ => false)
      val real = fn s => fn t => equals (t, real s)
      val seq = fn s => fn t => 
         (case node t 
             of Seq _ => Bits.equals (width t, WordSize.bits s) 
           | _ => false)
      val word = fn s => fn t => equals (t, word s)

      val cint = word (WordSize.cint ())
      val csize = word (WordSize.csize ())
      val cptrdiff = word (WordSize.cptrdiff ())
      val shiftArg = word WordSize.shiftArg

      val or = fn (p1, p2) => fn t => p1 t orelse p2 t
      val bitsOrSeq = fn s => or (bits (WordSize.bits s), seq s)
      val wordOrBitsOrSeq = fn s => or (word s, bitsOrSeq s)
      local
         fun make f s = let val t = f s in done ([t], SOME t) end
      in
         val realUnary = make real
         val wordUnary = make wordOrBitsOrSeq
      end
      local
         fun make f s = let val t = f s in done ([t], SOME bool) end
      in
         val wordUnaryP = make wordOrBitsOrSeq
      end
      local
         fun make f s = let val t = f s in done ([t, t], SOME t) end
      in
         val realBinary = make real
         val wordBinary = make wordOrBitsOrSeq
      end
      local
         fun make f s = let val t = f s in done ([t, t], SOME bool) end
      in
         val realCompare = make real
         val wordBinaryP = make wordOrBitsOrSeq
         val wordCompare = make wordOrBitsOrSeq
         val objptrCompare = make (fn _ => objptr) ()
      end
      fun realTernary s = done ([real s, real s, real s], SOME (real s))
      fun wordShift s = done ([wordOrBitsOrSeq s, shiftArg], SOME (wordOrBitsOrSeq s))
   in
      case prim of
         Prim.CFunction f => done (Vector.toListMap (CFunction.args f,
                                                     fn t' => fn t => equals (t', t)),
                                   SOME (fn t => equals (t, CFunction.return f)))
       | Prim.CPointer_add => done ([cpointer, cptrdiff], SOME cpointer)
       | Prim.CPointer_diff => done ([cpointer, cpointer], SOME cptrdiff)
       | Prim.CPointer_equal => done ([cpointer, cpointer], SOME bool)
       | Prim.CPointer_fromWord => done ([csize], SOME cpointer)
       | Prim.CPointer_lt => done ([cpointer, cpointer], SOME bool)
       | Prim.CPointer_sub => done ([cpointer, cptrdiff], SOME cpointer)
       | Prim.CPointer_toWord => done ([cpointer], SOME csize)
       | Prim.MLton_touch => done ([objptr], NONE)
       | Prim.Real_Math_acos s => realUnary s
       | Prim.Real_Math_asin s => realUnary s
       | Prim.Real_Math_atan s => realUnary s
       | Prim.Real_Math_atan2 s => realBinary s
       | Prim.Real_Math_cos s => realUnary s
       | Prim.Real_Math_exp s => realUnary s
       | Prim.Real_Math_ln s => realUnary s
       | Prim.Real_Math_log10 s => realUnary s
       | Prim.Real_Math_sin s => realUnary s
       | Prim.Real_Math_sqrt s => realUnary s
       | Prim.Real_Math_tan s => realUnary s
       | Prim.Real_abs s => realUnary s
       | Prim.Real_add s => realBinary s
       | Prim.Real_castToWord (s, s') => done ([real s], SOME (word s'))
       | Prim.Real_div s => realBinary s
       | Prim.Real_equal s => realCompare s
       | Prim.Real_ldexp s => done ([real s, cint], SOME (real s))
       | Prim.Real_le s => realCompare s
       | Prim.Real_lt s => realCompare s
       | Prim.Real_mul s => realBinary s
       | Prim.Real_muladd s => realTernary s
       | Prim.Real_mulsub s => realTernary s
       | Prim.Real_neg s => realUnary s
       | Prim.Real_qequal s => realCompare s
       | Prim.Real_rndToReal (s, s') => done ([real s], SOME (real s'))
       | Prim.Real_rndToWord (s, s', _) => done ([real s], SOME (word s'))
       | Prim.Real_round s => realUnary s
       | Prim.Real_sub s => realBinary s
       | Prim.Thread_returnToC => done ([], NONE)
       | Prim.Word_add s => wordBinary s
       | Prim.Word_addCheckP (s, _) => wordBinaryP s
       | Prim.Word_andb s => wordBinary s
       | Prim.Word_castToReal (s, s') => done ([word s], SOME (real s'))
       | Prim.Word_equal s => (wordCompare s) orelse objptrCompare
       | Prim.Word_extdToWord (s, s', _) => done ([wordOrBitsOrSeq s],
                                                  SOME (wordOrBitsOrSeq s'))
       | Prim.Word_lshift s => wordShift s
       | Prim.Word_lt (s, _) => wordCompare s
       | Prim.Word_mul (s, _) => wordBinary s
       | Prim.Word_mulCheckP (s, _) => wordBinaryP s
       | Prim.Word_neg s => wordUnary s
       | Prim.Word_negCheckP (s, _) => wordUnaryP s
       | Prim.Word_notb s => wordUnary s
       | Prim.Word_orb s => wordBinary s
       | Prim.Word_quot (s, _) => wordBinary s
       | Prim.Word_rem (s, _) => wordBinary s
       | Prim.Word_rndToReal (s, s', _) => done ([word s], SOME (real s'))
       | Prim.Word_rol s => wordShift s
       | Prim.Word_ror s => wordShift s
       | Prim.Word_rshift (s, _) => wordShift s
       | Prim.Word_sub s => wordBinary s
       | Prim.Word_subCheckP (s, _) => wordBinaryP s
       | Prim.Word_xorb s => wordBinary s
       | _ => Error.bug (concat ["RepType.checkPrimApp got strange prim: ",
                                 Prim.toString prim])
   end

local

fun extractTys (tys, dropBits, takeBits) =
   Exn.withEscape
   (fn escape =>
    let
       fun dropTys (tys, bits) =
          let
             fun loop (tys, bits) =
                if Bits.equals (bits, Bits.zero)
                   then tys
                else (case tys of
                         [] => escape NONE
                       | ty::tys =>
                            let
                               val b = width ty
                            in
                               if Bits.>= (bits, b)
                                  then loop (tys, Bits.- (bits, b))
                               else (case node ty of
                                        Bits => (Type.bits (Bits.- (b, bits))) :: tys
                                      | _ => escape NONE)
                            end)
          in
             if Bits.< (bits, Bits.zero)
                then escape NONE
             else loop (tys, bits)
          end
       val dropTys =
          Trace.trace2
          ("RepType.checkOffset.dropTys",
           List.layout Type.layout, Bits.layout,
           List.layout Type.layout)
          dropTys
       fun takeTys (tys, bits) =
          let
             fun loop (tys, bits, acc) =
                if Bits.equals (bits, Bits.zero)
                   then acc
                else (case tys of
                         [] => escape NONE
                       | ty::tys =>
                            let
                               val b = width ty
                            in
                               if Bits.>= (bits, b)
                                  then loop (tys, Bits.- (bits, b), ty :: acc)
                               else (case node ty of
                                        Bits => (Type.bits bits) :: acc
                                      | _ => escape NONE)
                            end)
          in
             if Bits.< (bits, Bits.zero)
                then escape NONE
             else List.rev (loop (tys, bits, []))
          end
       val takeTys =
          Trace.trace2
          ("RepType.checkOffset.takeTys",
           List.layout Type.layout, Bits.layout,
           List.layout Type.layout)
          takeTys
    in
       SOME (takeTys (dropTys (tys, dropBits), takeBits))
    end)
val extractTys =
   Trace.trace3
   ("RepType.checkOffset.extractTys",
    List.layout Type.layout, Bits.layout, Bits.layout,
    Option.layout (List.layout Type.layout))
   extractTys

fun getTys ty =
   case node ty of
      Seq tys => Vector.toList tys
    | _ => [ty]

in

fun checkOffset {components, isSequence, mustBeMutable, offset, result} =
   Exn.withEscape
   (fn escape0 =>
    let
       val ({elt = componentTy, isMutable = componentIsMutable, ...}, componentOffset) =
          Exn.withEscape
          (fn escape1 =>
           let
              val _ =
                 Vector.fold
                 (Prod.dest components, Bytes.zero,
                  fn (comp as {elt = compTy, ...}, compOffset) =>
                  let
                     val compOffset' = Bytes.+ (compOffset, Type.bytes compTy)
                  in
                     if Bytes.< (offset, compOffset')
                        then escape1 (comp, compOffset)
                        else compOffset'
                  end)
           in
              escape0 false
           end)

       val componentBits = Type.width componentTy
       val componentTys = getTys componentTy

       val offsetBytes = Bytes.- (offset, componentOffset)
       val offsetBits = Bytes.toBits offsetBytes

       val resultBits = Type.width result
       val resultTys = getTys result

       val alignBits =
          case !Control.align of
             Control.Align4 => Bits.inWord32
           | Control.Align8 => Bits.inWord64

       val adjOffsetBits =
          if Control.Target.bigEndian ()
             andalso Bits.< (resultBits, Bits.inWord32)
             andalso Bits.> (componentBits, resultBits)
             then let
                     val paddedComponentBits =
                        if isSequence
                           then Bits.min (componentBits, Bits.inWord32)
                           else Bits.inWord32
                     val paddedComponentOffsetBits =
                        Bits.alignDown (offsetBits, {alignment = paddedComponentBits})
                  in
                     Bits.+ (paddedComponentOffsetBits,
                             Bits.- (paddedComponentBits,
                                     Bits.- (Bits.+ (resultBits, offsetBits),
                                             paddedComponentOffsetBits)))
                  end
             else offsetBits
    in
       List.exists
       (Bits.prims, fn primBits =>
        Bits.equals (resultBits, primBits)
        andalso Bits.isAligned (offsetBits, {alignment = Bits.min (primBits, alignBits)}))
       andalso
       (case extractTys (componentTys, adjOffsetBits, resultBits) of
           NONE => false
         | SOME tys => List.equals (resultTys, tys, Type.equals))
       andalso
       (not mustBeMutable orelse componentIsMutable)
    end)

(* Check that offset/result exactly corresponds to a component;
 * Doesn't work with something like:
 *   components = ([Word10, Word14, Word8])
 *   offset = 3
 *   result = Word8
 * because while the Word10 and Word14 sub-components are accessed with
 * `Select.IndirectUnpack` (by reading/writing the whole `Word32` with
 * shifting/masking), the `Word8` sub-component is accessed with
 * `Select.Indirect` (because the packing results in the `Word8` sub-component
 * ending up with an 8-bit aligned offset); see `makeSubword32s` in
 * `PackedRepresentation`.
 *)
fun checkOffsetAlt {components, isSequence = _, mustBeMutable, offset, result} =
   Exn.withEscape
   (fn escape =>
    (ignore
     (Vector.fold
      (Prod.dest components, Bytes.zero,
       fn ({elt = compTy, isMutable = compIsMutable}, compOffset) =>
       if Bytes.equals (compOffset, offset)
          then escape (equals (compTy, result)
                       andalso
                       (not mustBeMutable orelse compIsMutable))
          else Bytes.+ (compOffset, Type.bytes compTy)))
     ; false))
val _ = checkOffsetAlt

end

val checkOffset =
   Trace.trace
   ("RepType.checkOffset",
    fn {components, isSequence, mustBeMutable, offset, result} =>
    let
       open Layout
    in
       record [("components", Prod.layout (components, Type.layout)),
               ("isSequence", Bool.layout isSequence),
               ("mustBeMutable", Bool.layout mustBeMutable),
               ("offset", Bytes.layout offset),
               ("result", Type.layout result)]
    end,
    Bool.layout)
   checkOffset

fun offsetIsOk {base, mustBeMutable, offset, tyconTy, result} =
   case node base of
      CPointer => true
    | Objptr opts =>
         if Bytes.equals (offset, Runtime.headerOffset ())
            then equals (result, objptrHeader ())
         else if Bytes.equals (offset, Runtime.sequenceLengthOffset ())
            then (1 = Vector.length opts)
                 andalso (case tyconTy (Vector.sub (opts, 0)) of
                             ObjectType.Sequence _ => true
                           | _ => false)
                 andalso (equals (result, seqIndex ()))
         else if Bytes.equals (offset, Runtime.sequenceCounterOffset ())
            then (1 = Vector.length opts)
                 andalso (case tyconTy (Vector.sub (opts, 0)) of
                             ObjectType.Sequence _ => true
                           | _ => false)
                 andalso (equals (result, seqIndex ()))
         else (1 = Vector.length opts)
              andalso (case tyconTy (Vector.sub (opts, 0)) of
                          ObjectType.Normal {components, ...} =>
                             checkOffset {components = components,
                                          isSequence = false,
                                          mustBeMutable = mustBeMutable,
                                          offset = offset,
                                          result = result}
                        | _ => false)
    | _ => false

fun sequenceOffsetIsOk {base, mustBeMutable, index, offset, tyconTy, result, scale} =
   case node base of
      CPointer => 
         (equals (index, csize ()))
         andalso (case node result of
                     CPointer => true
                   | Objptr _ => true (* for FFI export of indirect types *)
                   | Real _ => true
                   | Word _ => true
                   | _ => false)
         andalso (case Scale.fromBytes (bytes result) of
                     NONE => false
                   | SOME s => scale = s)
         andalso (Bytes.equals (offset, Bytes.zero))
    | Objptr opts => 
         (equals (index, seqIndex ()))
         andalso (1 = Vector.length opts)
         andalso (case tyconTy (Vector.first opts) of
                     ObjectType.Sequence {components, ...} =>
                        let val elt = Type.seq (Vector.map (Prod.dest components, #elt)) in
                        if equals (elt, word8)
                           then (* special case for PackWord operations *)
                                (case node result of
                                    Word wsRes => 
                                       (case Scale.fromBytes (WordSize.bytes wsRes) of
                                           NONE => false
                                         | SOME s => scale = s)
                                       andalso (Bytes.equals (offset, Bytes.zero))
                                  | _ => false)
                        else (case Scale.fromBytes (bytes elt) of
                                 NONE => scale = Scale.One
                               | SOME s => scale = s)
                             andalso (checkOffset {components = components,
                                                   isSequence = true,
                                                   mustBeMutable = mustBeMutable,
                                                   offset = offset,
                                                   result = result})
                        end
                   | _ => false)
    | _ => false



structure BuiltInCFunction =
   struct
      open CFunction

      datatype z = datatype Convention.t
      datatype z = datatype Target.t

      fun bug () = 
         vanilla {args = Vector.new1 (string ()),
                  name = "MLton_bug",
                  prototype = (Vector.new1 CType.objptr, NONE),
                  return = unit}

      local
         fun make b = fn () =>
            T {args = Vector.new3 (Type.gcState (), Type.csize (), Type.bool),
               convention = Cdecl,
               inline = false,
               kind = Kind.Runtime {bytesNeeded = NONE,
                                    ensuresBytesFree = SOME 1,
                                    mayGC = true,
                                    maySwitchThreadsFrom = b,
                                    maySwitchThreadsTo = b,
                                    modifiesFrontier = true,
                                    readsStackTop = true,
                                    writesStackTop = true},
               prototype = (Vector.new3 (CType.cpointer, CType.csize (), CType.bool), NONE),
               return = Type.unit,
               symbolScope = SymbolScope.Private,
               target = Direct "GC_collect"}
         val t = make true
         val f = make false
      in
         fun gc {maySwitchThreads = b} = if b then t () else f ()
      end
   end

end
