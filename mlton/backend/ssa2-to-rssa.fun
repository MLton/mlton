(* Copyright (C) 2009,2011,2014,2017,2019-2021 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Ssa2ToRssa (S: SSA2_TO_RSSA_STRUCTS): SSA2_TO_RSSA =
struct

open S
open Rssa

datatype z = datatype WordSize.prim

structure S = Ssa2

local
   open Ssa2
in
   structure Base = Base
end

local
   open Runtime
in
   structure GCField = GCField
end

structure CFunction =
   struct
      open CFunction
      open Type.BuiltInCFunction

      type t = Type.t CFunction.t

      structure CType =
         struct
            open CType
            val gcState = CPointer
            val intInf = Objptr
            val string = Objptr
            val thread = CPointer (* CHECK; thread (= objptr) would be better? *)
         end

      datatype z = datatype Convention.t
      datatype z = datatype SymbolScope.t
      datatype z = datatype Target.t

      val copyCurrentThread = fn () =>
         T {args = Vector.new1 (Type.gcState ()),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = true,
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = true,
                                 modifiesFrontier = true,
                                 readsStackTop = true,
                                 writesStackTop = true},
            prototype = (Vector.new1 CType.gcState, NONE),
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_copyCurrentThread"}

      (* CHECK; thread as objptr *)
      val copyThread = fn () =>
         T {args = Vector.new2 (Type.gcState (), Type.thread ()),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = true,
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = false,
                                 modifiesFrontier = true,
                                 readsStackTop = true,
                                 writesStackTop = true},
            prototype = let
                           open CType
                        in
                           (Vector.new2 (CPointer, CPointer), SOME CPointer)
                        end,
            return = Type.thread (),
            symbolScope = Private,
            target = Direct "GC_copyThread"}

      val halt = fn () =>
         T {args = Vector.new2 (Type.gcState (), Type.cint ()),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = true,
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = false,
                                 modifiesFrontier = true,
                                 readsStackTop = true,
                                 writesStackTop = true},
            prototype = (Vector.new2 (CType.gcState, CType.cint ()), NONE),
            return = Type.unit,
            symbolScope = Private,
            target = Direct "MLton_halt"}

      fun gcSequenceAllocate {return} =
         T {args = Vector.new4 (Type.gcState (),
                                Type.csize (),
                                Type.seqIndex (),
                                Type.objptrHeader ()),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = SOME 1,
                                 mayGC = true,
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = false,
                                 modifiesFrontier = true,
                                 readsStackTop = true,
                                 writesStackTop = true},
            prototype = (Vector.new4 (CType.gcState,
                                      CType.csize (),
                                      CType.seqIndex (),
                                      CType.objptrHeader ()),
                         SOME CType.objptr),
            return = return,
            symbolScope = Private,
            target = Direct "GC_sequenceAllocate"}

      fun gcSequenceCopy (dt, st) =
         T {args = Vector.new6 (Type.gcState (),
                                dt,
                                Type.seqIndex (),
                                st,
                                Type.seqIndex (),
                                Type.seqIndex ()),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = false,
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = false,
                                 modifiesFrontier = false,
                                 readsStackTop = false,
                                 writesStackTop = false},
            prototype = (Vector.new6 (CType.gcState,
                                      CType.Objptr,
                                      CType.seqIndex (),
                                      CType.Objptr,
                                      CType.seqIndex (),
                                      CType.seqIndex ()),
                         NONE),
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_sequenceCopy"}

      val returnToC = fn () =>
         T {args = Vector.new0 (),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = true,
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = true,
                                 modifiesFrontier = true,
                                 readsStackTop = true,
                                 writesStackTop = true},
            prototype = (Vector.new0 (), NONE),
            return = Type.unit,
            symbolScope = Private,
            target = Direct "Thread_returnToC"}

      (* CHECK; thread as objptr *)
      val threadSwitchTo = fn () =>
         T {args = Vector.new3 (Type.gcState (), Type.thread (), Type.csize ()),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = SOME 2,
                                 mayGC = true,
                                 maySwitchThreadsFrom = true,
                                 maySwitchThreadsTo = true,
                                 modifiesFrontier = true,
                                 readsStackTop = true,
                                 writesStackTop = true},
            prototype = (Vector.new3 (CType.gcState,
                                      CType.thread,
                                      CType.csize ()),
                         NONE),
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_switchToThread"}

      (* CHECK; weak as objptr *)
      fun weakCanGet {arg} =
         T {args = Vector.new2 (Type.gcState (), arg),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = false,
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = false,
                                 modifiesFrontier = false,
                                 readsStackTop = false,
                                 writesStackTop = false},
            prototype = (Vector.new2 (CType.gcState, CType.cpointer),
                         SOME CType.bool),
            return = Type.bool,
            symbolScope = Private,
            target = Direct "GC_weakCanGet"}

      (* CHECK; weak as objptr *)
      fun weakGet {arg, return} =
         T {args = Vector.new2 (Type.gcState (), arg),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = false,
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = false,
                                 modifiesFrontier = false,
                                 readsStackTop = false,
                                 writesStackTop = false},
            prototype = (Vector.new2 (CType.gcState, CType.cpointer),
                         SOME CType.cpointer),
            return = return,
            symbolScope = Private,
            target = Direct "GC_weakGet"}

      (* CHECK; weak as objptr *)
      fun weakNew {arg, return} =
         T {args = Vector.new3 (Type.gcState (), Type.objptrHeader (), arg),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = true,
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = false,
                                 modifiesFrontier = true,
                                 readsStackTop = true,
                                 writesStackTop = true},
            prototype = (Vector.new3 (CType.gcState,
                                      CType.objptrHeader (),
                                      CType.cpointer),
                         SOME (CType.cpointer)),
            return = return,
            symbolScope = Private,
            target = Direct "GC_weakNew"}

      val worldSave = fn () =>
         T {args = Vector.new2 (Type.gcState (), Type.string ()),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = true,
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = true,
                                 modifiesFrontier = true,
                                 readsStackTop = true,
                                 writesStackTop = true},
            prototype = (Vector.new2 (CType.gcState, CType.cpointer), NONE),
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_saveWorld"}

      (* CHECK; share with objptr *)
      fun share t =
         T {args = Vector.new2 (Type.gcState (), t),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = true, (* MLton.share works by tracing an object.
                                                * Make sure all the GC invariants are true,
                                                * because tracing might encounter the current
                                                * stack in the heap.
                                                *)
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = false,
                                 modifiesFrontier = true, (* actually, just readsFrontier *)
                                 readsStackTop = true,
                                 writesStackTop = true},
            prototype = (Vector.new2 (CType.gcState, CType.cpointer), NONE),
            return = Type.unit,
            symbolScope = Private,
            target = Direct "GC_share"}

      (* CHECK; size with objptr *)
      fun size t =
         T {args = Vector.new2 (Type.gcState (), t),
            convention = Cdecl,
            inline = false,
            kind = Kind.Runtime {bytesNeeded = NONE,
                                 ensuresBytesFree = NONE,
                                 mayGC = true, (* MLton.size works by tracing an object.
                                                * Make sure all the GC invariants are true,
                                                * because tracing might encounter the current
                                                * stack in the heap.
                                                *)
                                 maySwitchThreadsFrom = false,
                                 maySwitchThreadsTo = false,
                                 modifiesFrontier = true,
                                 readsStackTop = true,
                                 writesStackTop = true},
            prototype = (Vector.new2 (CType.gcState, CType.cpointer),
                         SOME (CType.csize ())),
            return = Type.csize (),
            symbolScope = Private,
            target = Direct "GC_size"}

      fun amAllocationProfiling () =
         Control.ProfileAlloc = !Control.profile
      val intInfBinary = fn name =>
         CFunction.T {args = Vector.new4 (Type.gcState (),
                                          Type.intInf (),
                                          Type.intInf (),
                                          Type.csize ()),
                      convention = Cdecl,
                      inline = false,
                      kind = CFunction.Kind.Runtime {bytesNeeded = SOME 3,
                                                     ensuresBytesFree = NONE,
                                                     mayGC = false,
                                                     maySwitchThreadsFrom = false,
                                                     maySwitchThreadsTo = false,
                                                     modifiesFrontier = true,
                                                     readsStackTop = amAllocationProfiling (),
                                                     writesStackTop = false},
                      prototype = (Vector.new4 (CType.gcState,
                                                CType.intInf,
                                                CType.intInf,
                                                CType.csize ()),
                                   SOME CType.intInf),
                      return = Type.intInf (),
                      symbolScope = Private,
                      target = Direct (Prim.toString name)}
      val intInfCompare = fn name =>
         (* CHECK; cint would be better? *)
         CFunction.T {args = Vector.new3 (Type.gcState (),
                                          Type.intInf (),
                                          Type.intInf ()),
                      convention = Cdecl,
                      inline = false,
                      kind = CFunction.Kind.Runtime {bytesNeeded = NONE,
                                                     ensuresBytesFree = NONE,
                                                     mayGC = false,
                                                     maySwitchThreadsFrom = false,
                                                     maySwitchThreadsTo = false,
                                                     modifiesFrontier = false,
                                                     readsStackTop = false,
                                                     writesStackTop = false},
                      prototype = (Vector.new3 (CType.gcState,
                                                CType.intInf,
                                                CType.intInf),
                                   SOME CType.compareRes),
                      return = Type.compareRes,
                      symbolScope = Private,
                      target = Direct (Prim.toString name)}
      val intInfShift = fn name =>
         CFunction.T {args = Vector.new4 (Type.gcState (),
                                          Type.intInf (),
                                          Type.shiftArg,
                                          Type.csize ()),
                      convention = Cdecl,
                      inline = false,
                      kind = CFunction.Kind.Runtime {bytesNeeded = SOME 3,
                                                     ensuresBytesFree = NONE,
                                                     mayGC = false,
                                                     maySwitchThreadsFrom = false,
                                                     maySwitchThreadsTo = false,
                                                     modifiesFrontier = true,
                                                     readsStackTop = amAllocationProfiling (),
                                                     writesStackTop = false},
                      prototype = (Vector.new4 (CType.gcState,
                                                CType.intInf,
                                                CType.shiftArg,
                                                CType.csize ()),
                                   SOME CType.intInf),
                      return = Type.intInf (),
                      symbolScope = Private,
                      target = Direct (Prim.toString name)}
      val intInfToString = fn name =>
         (* CHECK; cint would be better? *)
         CFunction.T {args = Vector.new4 (Type.gcState (),
                                          Type.intInf (),
                                          Type.word WordSize.word32,
                                          Type.csize ()),
                      convention = Cdecl,
                      inline = false,
                      kind = CFunction.Kind.Runtime {bytesNeeded = SOME 3,
                                                     ensuresBytesFree = NONE,
                                                     mayGC = false,
                                                     maySwitchThreadsFrom = false,
                                                     maySwitchThreadsTo = false,
                                                     modifiesFrontier = true,
                                                     readsStackTop = amAllocationProfiling (),
                                                     writesStackTop = false},
                      prototype = (Vector.new4 (CType.gcState,
                                                CType.intInf,
                                                CType.Int32,
                                                CType.csize ()),
                                   SOME CType.string),
                      return = Type.string (),
                      symbolScope = Private,
                      target = Direct (Prim.toString name)}
      val intInfUnary = fn name =>
         CFunction.T {args = Vector.new3 (Type.gcState (),
                                          Type.intInf (),
                                          Type.csize ()),
                      convention = Cdecl,
                      inline = false,
                      kind = CFunction.Kind.Runtime {bytesNeeded = SOME 2,
                                                     ensuresBytesFree = NONE,
                                                     mayGC = false,
                                                     maySwitchThreadsFrom = false,
                                                     maySwitchThreadsTo = false,
                                                     modifiesFrontier = true,
                                                     readsStackTop = amAllocationProfiling (),
                                                     writesStackTop = false},
                      prototype = (Vector.new3 (CType.gcState,
                                                CType.intInf,
                                                CType.csize ()),
                                   SOME CType.intInf),
                      return = Type.intInf (),
                      symbolScope = Private,
                      target = Direct (Prim.toString name)}
   end

structure Prim =
   struct
      local
         structure CFunction' = CFunction
      in
         open Prim
         structure CFunction = CFunction'
      end

      datatype u = datatype t
      type t = Type.t t

      fun cFunctionRaise (p: t): CFunction.t =
         let
            datatype z = datatype CFunction.Convention.t
            datatype z = datatype CFunction.SymbolScope.t
            datatype z = datatype CFunction.Target.t
            val name = toString p
            val real = Type.real
            val word = Type.word
            val vanilla = CFunction.vanilla
            fun wordCType (s, sg) = CType.word (s, sg)
            fun realCType s = CType.real s
            fun coerce (t1, ct1, t2, ct2) =
               vanilla {args = Vector.new1 t1,
                        name = name,
                        prototype = (Vector.new1 ct1, SOME ct2),
                        return = t2}
            local
               fun make n s =
                  let
                     val t = real s
                     val ct = CType.real s
                  in
                     vanilla {args = Vector.new (n, t),
                              name = name,
                              prototype = (Vector.new (n, ct), SOME ct),
                              return = t}
                  end
            in
               val realBinary = make 2
               val realTernary = make 3
               val realUnary = make 1
            end
            fun realCompare s =
               let
                  val t = real s
               in
                  vanilla {args = Vector.new2 (t, t),
                           name = name,
                           prototype = let
                                          val t = CType.real s
                                       in
                                          (Vector.new2 (t, t), SOME CType.bool)
                                       end,
                           return = Type.bool}
               end
            local
               fun make n (s, sg) =
                  let
                     val t = word s
                     val ct = CType.word (s, sg)
                  in
                     vanilla {args = Vector.new (n, t),
                              name = name,
                              prototype = (Vector.new (n, ct), SOME ct),
                              return = t}
                  end
               fun makeCheckP n (s, sg) =
                  let
                     val t = word s
                     val ct = CType.word (s, sg)
                  in
                     vanilla {args = Vector.new (n, t),
                              name = name,
                              prototype = (Vector.new (n, ct), SOME CType.bool),
                              return = Type.bool}
                  end
            in
               val wordBinary = make 2
               val wordBinaryCheckP = makeCheckP 2
               val wordUnary = make 1
               val wordUnaryCheckP = makeCheckP 1
            end
            fun wordCompare (s, sg) =
               let
                  val t = word s
                  val ct = CType.word (s, sg)
               in
                  vanilla {args = Vector.new2 (t, t),
                           name = name,
                           prototype = (Vector.new2 (ct, ct), SOME CType.bool),
                           return = Type.bool}
               end
            fun wordShift (s, sg) =
               let
                  val t = word s
                  val ct = CType.word (s, sg)
               in
                  vanilla {args = Vector.new2 (t, Type.shiftArg),
                           name = name,
                           prototype = (Vector.new2 (ct, CType.shiftArg), SOME ct),
                           return = t}
               end
         in
            case p of
               Real_Math_acos s => realUnary s
             | Real_Math_asin s => realUnary s
             | Real_Math_atan s => realUnary s
             | Real_Math_atan2 s => realBinary s
             | Real_Math_cos s => realUnary s
             | Real_Math_exp s => realUnary s
             | Real_Math_ln s => realUnary s
             | Real_Math_log10 s => realUnary s
             | Real_Math_sin s => realUnary s
             | Real_Math_sqrt s => realUnary s
             | Real_Math_tan s => realUnary s
             | Real_abs s => realUnary s
             | Real_add s => realBinary s
             | Real_castToWord (s1, s2) =>
                  coerce (real s1, realCType s1,
                          word s2, wordCType (s2, {signed = false}))
             | Real_div s => realBinary s
             | Real_equal s => realCompare s
             | Real_ldexp s =>
                  let
                     val t = real s
                     val ct = CType.real s
                  in
                     vanilla {args = Vector.new2 (t, Type.cint ()),
                              name = name,
                              prototype = (Vector.new2 (ct, CType.cint ()),
                                           SOME ct),
                              return = t}
                  end
             | Real_le s => realCompare s
             | Real_lt s => realCompare s
             | Real_mul s => realBinary s
             | Real_muladd s => realTernary s
             | Real_mulsub s => realTernary s
             | Real_neg s => realUnary s
             | Real_qequal s => realCompare s
             | Real_rndToReal (s1, s2) =>
                  coerce (real s1, realCType s1, real s2, realCType s2)
             | Real_rndToWord (s1, s2, sg) =>
                  coerce (real s1, realCType s1,
                          word s2, wordCType (s2, sg))
             | Real_round s => realUnary s
             | Real_sub s => realBinary s
             | Thread_returnToC => CFunction.returnToC ()
             | Word_add s => wordBinary (s, {signed = false})
             | Word_addCheckP (s, sg) => wordBinaryCheckP (s, sg)
             | Word_andb s => wordBinary (s, {signed = false})
             | Word_castToReal (s1, s2) =>
                  coerce (word s1, wordCType (s1, {signed = false}),
                          real s2, realCType s2)
             | Word_equal s => wordCompare (s, {signed = false})
             | Word_extdToWord (s1, s2, sg) =>
                  coerce (word s1, wordCType (s1, sg),
                          word s2, wordCType (s2, {signed = false}))
             | Word_lshift s => wordShift (s, {signed = false})
             | Word_lt z => wordCompare z
             | Word_mul z => wordBinary z
             | Word_mulCheckP (s, sg) => wordBinaryCheckP (s, sg)
             | Word_neg s => wordUnary (s, {signed = false})
             | Word_negCheckP (s, sg) => wordUnaryCheckP (s, sg)
             | Word_notb s => wordUnary (s, {signed = false})
             | Word_orb s => wordBinary (s, {signed = false})
             | Word_quot z => wordBinary z
             | Word_rem z => wordBinary z
             | Word_rndToReal (s1, s2, sg) =>
                  coerce (word s1, wordCType (s1, sg),
                          real s2, realCType s2)
             | Word_xorb s => wordBinary (s, {signed = false})
             | Word_rol s => wordShift (s, {signed = false})
             | Word_ror s => wordShift (s, {signed = false})
             | Word_rshift z => wordShift z
             | Word_sub s => wordBinary (s, {signed = false})
             | Word_subCheckP (s, sg) => wordBinaryCheckP (s, sg)
             | _ => Error.bug "SsaToRssa.Name.cFunctionRaise"
         end

      fun cFunction p = SOME (cFunctionRaise p) handle _ => NONE
   end

datatype z = datatype Operand.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure PackedRepresentation = PackedRepresentation (structure Rssa = Rssa
                                                       structure Ssa2 = Ssa2)

structure Type =
   struct
      open Type

      fun scale (ty: t): Scale.t =
         case Scale.fromBytes (bytes ty) of
            NONE => Error.bug "SsaToRssa.Type.scale"
          | SOME s => s
   end

val cardSizeLog2 : IntInf.t = 8 (* must agree with CARD_SIZE_LOG2 in gc.c *)

fun updateCard (addr: Operand.t): Statement.t list =
   let
      val index = Var.newNoname ()
      (* CHECK; WordSize.objptr or WordSize.cpointer? *)
      val sz = WordSize.objptr ()
      val indexTy = Type.word sz
      val cardElemSize = WordSize.fromBits Bits.inByte
   in
      [PrimApp {args = (Vector.new2
                        (Operand.cast (addr, Type.bits (WordSize.bits sz)),
                         Operand.word
                         (WordX.fromIntInf (cardSizeLog2, WordSize.shiftArg)))),
                dst = SOME (index, indexTy),
                prim = Prim.Word_rshift (sz, {signed = false})},
       Move {dst = (SequenceOffset
                    {base = Runtime GCField.CardMapAbsolute,
                     index = Var {ty = indexTy, var = index},
                     offset = Bytes.zero,
                     scale = Scale.One,
                     ty = Type.word cardElemSize}),
             src = Operand.word (WordX.one cardElemSize)}]
   end

fun convertWordSize (ws: WordSize.t): WordSize.t =
   WordSize.roundUpToPrim ws

fun convertWordX (w: WordX.t): WordX.t =
   WordX.resize (w, convertWordSize (WordX.size w))

fun convert (program as S.Program.T {functions, globals, main, ...},
             {codegenImplementsPrim: Rssa.Type.t Rssa.Prim.t -> bool}): Rssa.Program.t =
   let
      val {diagnostic, genCase, object, objectTypes, select, sequence, toRtype, update} =
         PackedRepresentation.compute program
      val objectTypes = Vector.concat [ObjectType.basic (), objectTypes]
      val () =
         Vector.foreachi
         (objectTypes, fn (i, (opt, _)) => ObjptrTycon.setIndex (opt, i))
      val objectTypes = Vector.map (objectTypes, #2)
      val () = diagnostic ()

      val newObjectTypes = ref []
      local
         fun componentsHash cs =
            Prod.hash (cs, Type.hash)
         fun componentsEquals (cs1, cs2) =
            Prod.equals (cs1, cs2, Type.equals)
         val h = HashTable.new {hash = componentsHash, equals = componentsEquals}
      in
         fun allocRawOpt components =
            HashTable.lookupOrInsert
            (h, components,
             fn () =>
             let
                val rawComponents =
                   Prod.map (components, fn ty =>
                             if Type.isObjptr ty
                                then Type.bits (Type.width ty)
                                else ty)
                val rawTy = ObjectType.Sequence {components = rawComponents, hasIdentity = true}
                val rawOpt = ObjptrTycon.new ()
                val () =
                   ObjptrTycon.setIndex
                   (rawOpt, Vector.length objectTypes + HashTable.size h)
                val () =
                   List.push (newObjectTypes, rawTy)
             in
                rawOpt
             end)
      end

      val {get = varInfo: Var.t -> {ty: S.Type.t},
           set = setVarInfo, ...} =
         Property.getSetOnce (Var.plist,
                              Property.initRaise ("varInfo", Var.layout))
      val setVarInfo =
         Trace.trace2 ("SsaToRssa.setVarInfo",
                       Var.layout, S.Type.layout o #ty, Unit.layout)
         setVarInfo
      val varType = #ty o varInfo
      fun varOp (x: Var.t): Operand.t =
         Var {var = x, ty = valOf (toRtype (varType x))}
      val varOp =
         Trace.trace ("SsaToRssa.varOp", Var.layout, Operand.layout) varOp
      fun varOps xs = Vector.map (xs, varOp)
      val extraBlocks = ref []
      fun newBlock {args, kind,
                    statements: Statement.t vector,
                    transfer: Transfer.t}: Label.t =
         let
            val l = Label.newNoname ()
            val _ = List.push (extraBlocks,
                               Block.T {args = args,
                                        kind = kind,
                                        label = l,
                                        statements = statements,
                                        transfer = transfer})
         in
            l
         end
      val {get = labelInfo: (Label.t ->
                             {args: (Var.t * S.Type.t) vector,
                              cont: (Handler.t * Label.t) list ref,
                              handler: Label.t option ref}),
           set = setLabelInfo, ...} =
         Property.getSetOnce (Label.plist,
                              Property.initRaise ("label info", Label.layout))
      fun translateCase ({test: Var.t,
                          cases: (Con.t, Label.t) S.Cases.t,
                          default: Label.t option})
         : Statement.t list * Transfer.t =
         case cases of
            S.Cases.Con cases =>
               (case (Vector.length cases, default) of
                   (0, NONE) => ([], Transfer.bug ())
                 | _ =>
                      (case S.Type.dest (varType test) of
                          S.Type.Datatype tycon =>
                             let
                                val test = fn () => varOp test
                                val cases =
                                   Vector.map
                                   (cases, fn (con, dst) =>
                                    {con = con,
                                     dst = dst,
                                     dstHasArg =
                                     Vector.fold
                                     (#args (labelInfo dst), false, fn ((_,ty),b) =>
                                      b orelse isSome (toRtype ty))})
                                val (ss, t, blocks) =
                                   genCase {cases = cases,
                                            default = default,
                                            test = test,
                                            tycon = tycon}
                                val () =
                                   extraBlocks := blocks @ !extraBlocks
                             in
                                (ss, t)
                             end
                        | _ => Error.bug "SsaToRssa.translateCase: strange type"))
          | S.Cases.Word (s, cases) =>
               let
                  val cases =
                     QuickSort.sortVector
                     (Vector.map (cases, fn (w, l) => (convertWordX w, l)),
                      fn ((w, _), (w', _)) => WordX.le (w, w', {signed = false}))
               in
                  ([],
                   Switch
                   (Switch.T
                    {cases = cases,
                     default = default,
                     expect = NONE,
                     size = convertWordSize s,
                     test = varOp test}))
               end
      fun eta (l: Label.t, kind: Kind.t): Label.t =
         let
            val {args, ...} = labelInfo l
            val args = Vector.keepAllMap (args, fn (x, t) =>
                                          Option.map (toRtype t, fn t =>
                                                      (Var.new x, t)))
            val l' = Label.new l
            val _ =
               List.push
               (extraBlocks,
                Block.T {args = args,
                         kind = kind,
                         label = l',
                         statements = Vector.new0 (),
                         transfer = (Transfer.Goto
                                     {dst = l,
                                      args = Vector.map (args, fn (var, ty) =>
                                                         Var {var = var,
                                                              ty = ty})})})
         in
            l'
         end
      fun labelHandler (l: Label.t): Label.t =
         let
            val {handler, ...} = labelInfo l
         in
            case !handler of
               NONE =>
                  let
                     val l' = eta (l, Kind.Handler)
                     val _ = handler := SOME l'
                  in
                     l'
                  end
             | SOME l => l
         end
      fun labelCont (l: Label.t, h: Handler.t): Label.t =
         let
            val {cont, ...} = labelInfo l
            datatype z = datatype Handler.t
         in
            case List.peek (!cont, fn (h', _) => Handler.equals (h, h')) of
               SOME (_, l) => l
             | NONE =>
                  let
                     val l' = eta (l, Kind.Cont {handler = h})
                     val _ = List.push (cont, (h, l'))
                  in
                     l'
                  end
         end
      val labelCont =
         Trace.trace2 ("SsaToRssa.labelCont",
                       Label.layout, Handler.layout, Label.layout)
         labelCont
      fun vos (xs: Var.t vector) =
         Vector.keepAllMap (xs, fn x =>
                            Option.map (toRtype (varType x), fn _ =>
                                        varOp x))
      fun bogus (t: Type.t): Operand.t =
         case Type.deReal t of
            NONE => Operand.cast (Operand.word (Type.bogusWord t), t)
          | SOME s => Operand.Const (Const.real (RealX.zero s))
      val handlesSignals =
         S.Program.hasPrim
         (program, fn p =>
          case p of
             Prim.MLton_installSignalHandler => true
           | _ => false)
         orelse !Control.forceHandlesSignals
      fun translateFormals v =
         Vector.keepAllMap (v, fn (x, t) =>
                            Option.map (toRtype t, fn t => (x, t)))
      fun translatePrim p =
         Prim.map (p, fn t =>
                   case toRtype t of
                      NONE => Type.unit
                    | SOME t => t)
      fun translateTransfer (t: S.Transfer.t): (Statement.t list *
                                                Transfer.t) =
         case t of
            S.Transfer.Bug => ([], Transfer.bug ())
          | S.Transfer.Call {func, args, return} =>
               let
                  datatype z = datatype S.Return.t
                  val return =
                     case return of
                        Dead => Return.Dead
                      | NonTail {cont, handler} =>
                           let
                              datatype z = datatype S.Handler.t
                              val handler =
                                 case handler of
                                    Caller => Handler.Caller
                                  | Dead => Handler.Dead
                                  | Handle l => Handler.Handle (labelHandler l)
                           in
                              Return.NonTail {cont = labelCont (cont, handler),
                                              handler = handler}
                           end
                      | Tail => Return.Tail
               in
                  ([], Transfer.Call {func = func,
                                      args = vos args,
                                      return = return})
               end
          | S.Transfer.Case r => translateCase r
          | S.Transfer.Goto {dst, args} =>
               ([], Transfer.Goto {dst = dst, args = vos args})
          | S.Transfer.Raise xs => ([], Transfer.Raise (vos xs))
          | S.Transfer.Return xs => ([], Transfer.Return (vos xs))
          | S.Transfer.Runtime {args, prim, return} =>
               (case prim of
                   Prim.Thread_copyCurrent =>
                      let
                         val func = CFunction.copyCurrentThread ()
                         val l =
                            newBlock {args = Vector.new0 (),
                                      kind = Kind.CReturn {func = func},
                                      statements = Vector.new0 (),
                                      transfer = (Goto {args = Vector.new0 (),
                                                        dst = return})}
                      in
                         ([],
                          Transfer.CCall
                          {args = Vector.concat [Vector.new1 GCState,
                                                 vos args],
                           func = func,
                           return = SOME l})
                      end
                 | _ => Error.bug (concat
                                   ["SsaToRssa.translateTransfer: ",
                                    "strange Runtime prim: ",
                                    Prim.toString prim]))
      fun translateStatementsTransfer (statements, ss, transfer) =
         let
            fun loop (i, ss, t): Statement.t vector * Transfer.t =
               if i < 0
                  then (Vector.fromList ss, t)
               else
                  let
                     fun none () = loop (i - 1, ss, t)
                     fun add s = loop (i - 1, s :: ss, t)
                     fun add2 (s1, s2) = loop (i - 1, s1 :: s2 :: ss, t)
                     fun adds ss' = loop (i - 1, ss' @ ss, t)
                     val s = Vector.sub (statements, i)
                  in
                     case s of
                        S.Statement.Profile e => add (Statement.Profile e)
                      | S.Statement.Update {base, offset, value} =>
                           (case toRtype (varType value) of
                               NONE => none ()
                             | SOME t =>
                                  let
                                     val baseOp = Base.map (base, varOp)
                                     val ss =
                                        update
                                        {base = baseOp,
                                         baseTy = varType (Base.object base),
                                         offset = offset,
                                         value = varOp value}
                                     val ss =
                                        if !Control.markCards
                                           andalso Type.isObjptr t
                                           then
                                              updateCard (Base.object baseOp)
                                              @ ss
                                        else ss
                                  in
                                     adds ss
                                  end)
                      | S.Statement.Bind {exp, ty, var} =>
                  let
                     fun split (args, kind,
                                ss: Statement.t list,
                                make: Label.t -> Statement.t list * Transfer.t) =
                        let
                           val l = newBlock {args = args,
                                             kind = kind,
                                             statements = Vector.fromList ss,
                                             transfer = t}
                           val (ss, t) = make l
                        in
                           loop (i - 1, ss, t)
                        end
                     fun maybeMove (f: Type.t -> Operand.t) =
                        case toRtype ty of
                           NONE => none ()
                         | SOME ty =>
                              add (Bind {dst = (valOf var, ty),
                                         pinned = false,
                                         src = f ty})
                     fun move (src: Operand.t) = maybeMove (fn _ => src)
                  in
                     case exp of
                        S.Exp.Const c =>
                           (case c of
                               Const.IntInf i =>
                                  let
                                     fun doit c =
                                        maybeMove (fn ty => Operand.cast (Const c, ty))
                                  in
                                     case Const.IntInfRep.fromIntInf i of
                                        Const.IntInfRep.Big v =>
                                           doit (Const.WordVector v)
                                      | Const.IntInfRep.Small w =>
                                           doit (Const.Word w)
                                  end
                             | Const.Word w => move (Const (Const.Word (convertWordX w)))
                             | _ => move (Const c))
                      | S.Exp.Inject {variant, ...} =>
                           if isSome (toRtype ty)
                              then move (varOp variant)
                           else none ()
                      | S.Exp.Object {args, con} =>
                           (case toRtype ty of
                               NONE => none ()
                             | SOME dstTy =>
                                  adds (object {args = args,
                                                con = con,
                                                dst = (valOf var, dstTy),
                                                objectTy = ty,
                                                oper = varOp}))
                      | S.Exp.PrimApp {args, prim} =>
                           let
                              val prim = translatePrim prim
                              fun arg i = Vector.sub (args, i)
                              fun a i = varOp (arg i)
                              fun cast () =
                                 move (Operand.cast (a 0, valOf (toRtype ty)))
                              fun ifIsWeakPointer (ty: S.Type.t, yes, no) =
                                 case S.Type.dest ty of
                                    S.Type.Weak ty =>
                                       (case toRtype ty of
                                           NONE => no ()
                                         | SOME t =>
                                              if Type.isObjptr t
                                                 then yes t
                                              else no ())
                                  | _ => Error.bug "SsaToRssa.ifIsWeakPointer"
                              fun arrayOrVectorLength () =
                                 move (Offset
                                       {base = a 0,
                                        offset = Runtime.sequenceLengthOffset (),
                                        ty = Type.seqIndex ()})
                              fun subWord s =
                                 let
                                    val ty = Type.word s
                                 in
                                    move (SequenceOffset {base = a 0,
                                                          index = a 1,
                                                          offset = Bytes.zero,
                                                          scale = Type.scale ty,
                                                          ty = ty})
                                 end
                              fun dst () =
                                 case var of
                                    SOME x =>
                                       Option.map (toRtype (varType x), fn t =>
                                                   (x, t))
                                  | NONE => NONE
                              fun primApp (prim, args) =
                                 add (PrimApp {dst = dst (),
                                               prim = prim,
                                               args = args})
                              fun bumpAtomicState n =
                                 let
                                    val atomicState = Runtime GCField.AtomicState
                                    val res = Var.newNoname ()
                                    val resTy = Operand.ty atomicState
                                 in
                                    [Statement.PrimApp
                                     {args = (Vector.new2
                                              (atomicState,
                                               (Operand.word
                                                (WordX.fromInt (n, WordSize.word32))))),
                                      dst = SOME (res, resTy),
                                      prim = Prim.Word_add WordSize.word32},
                                     Statement.Move
                                     {dst = atomicState,
                                      src = Var {ty = resTy, var = res}}]
                                 end
                              fun ccall {args: Operand.t vector,
                                         func: CFunction.t} =
                                 let
                                    val formals =
                                       case dst () of
                                          NONE => Vector.new0 ()
                                        | SOME (x, t) => Vector.new1 (x, t)
                                 in
                                    split
                                    (formals, Kind.CReturn {func = func}, ss,
                                     fn l =>
                                     ([],
                                      Transfer.CCall {args = args,
                                                      func = func,
                                                      return = SOME l}))
                                 end
                              fun simpleCCall (f: CFunction.t) =
                                 ccall {args = vos args,
                                        func = f}
                              fun simpleCCallWithGCState (f: CFunction.t) =
                                 ccall {args = Vector.concat
                                               [Vector.new1 GCState,
                                                vos args],
                                        func = f}
                              fun sequenceAlloc (numElts: Operand.t, opt) =
                                 let
                                    val result = valOf (toRtype ty)
                                    val args =
                                       Vector.new4 (GCState,
                                                    Operand.zero (WordSize.csize ()),
                                                    numElts,
                                                    ObjptrTycon opt)
                                    val func =
                                       CFunction.gcSequenceAllocate
                                       {return = result}
                                 in
                                    ccall {args = args, func = func}
                                 end
                     fun cpointerGet () =
                        maybeMove (fn ty =>
                                   SequenceOffset {base = a 0,
                                                   index = a 1,
                                                   offset = Bytes.zero,
                                                   scale = Type.scale ty,
                                                   ty = ty})
                     fun cpointerSet () =
                        let
                           val src = a 2
                           val ty = Operand.ty src
                        in
                           add (Move {dst = SequenceOffset {base = a 0,
                                                            index = a 1,
                                                            offset = Bytes.zero,
                                                            scale = Type.scale ty,
                                                            ty = ty},
                                      src = a 2})
                        end
                     fun codegenOrC (p: Prim.t, args) =
                        if codegenImplementsPrim p
                           then primApp (p, args)
                        else (case Prim.cFunction p of
                                 NONE =>
                                    Error.bug (concat ["SsaToRssa.codegenOrC: ",
                                                       "unimplemented prim:",
                                                       Prim.toString p])
                               | SOME func => ccall {args = args, func = func})
                     fun simpleCodegenOrC (p: Prim.t) =
                        codegenOrC (p, varOps args)
                           in
                              case prim of
                                 Prim.Array_alloc {raw} =>
                                    let
                                       val allocOpt = fn () =>
                                          let
                                             val result = valOf (toRtype ty)
                                             val opt =
                                                case Type.deObjptr result of
                                                   NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Array_alloc"
                                                 | SOME opt => opt
                                          in
                                             opt
                                          end
                                       val allocRawOpt = fn () =>
                                          let
                                             val result = valOf (toRtype ty)
                                             val arrOpt =
                                                case Type.deObjptr result of
                                                   NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Array_allocRaw"
                                                 | SOME arrOpt => arrOpt
                                             val arrTy =
                                                Vector.sub (objectTypes, ObjptrTycon.index arrOpt)
                                             val arrComponents =
                                                case arrTy of
                                                   ObjectType.Sequence {components, ...} => components
                                                 | _ => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Array_allocRaw"
                                             val rawOpt = allocRawOpt arrComponents
                                          in
                                             rawOpt
                                          end
                                    in
                                       sequenceAlloc (a 0, if raw then allocRawOpt () else allocOpt ())
                                    end
                               | Prim.Array_copyArray => simpleCCallWithGCState (CFunction.gcSequenceCopy (Operand.ty (a 0), Operand.ty (a 2)))
                               | Prim.Array_copyVector => simpleCCallWithGCState (CFunction.gcSequenceCopy (Operand.ty (a 0), Operand.ty (a 2)))
                               | Prim.Array_length => arrayOrVectorLength ()
                               | Prim.Array_toArray =>
                                    let
                                       val rawarr = a 0
                                       val arrTy = valOf (toRtype ty)
                                       val arrOpt =
                                          case Type.deObjptr arrTy of
                                             NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Array_toArray"
                                           | SOME arrOpt => arrOpt
                                    in
                                       add2
                                       (Move
                                        {dst = (Offset
                                                {base = rawarr,
                                                 offset = Runtime.headerOffset (),
                                                 ty = Type.objptrHeader ()}),
                                         src = ObjptrTycon arrOpt},
                                        Bind {dst = (valOf var, arrTy),
                                              pinned = false,
                                              src = Operand.cast (rawarr, arrTy)})
                                    end
                               | Prim.Array_toVector =>
                                    let
                                       val sequence = a 0
                                       val vecTy = valOf (toRtype ty)
                                       val vecOpt =
                                          case Type.deObjptr vecTy of
                                             NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Array_toVector"
                                           | SOME vecOpt => vecOpt
                                    in
                                       add2
                                       (Move
                                        {dst = (Offset
                                                {base = sequence,
                                                 offset = Runtime.headerOffset (),
                                                 ty = Type.objptrHeader ()}),
                                         src = ObjptrTycon vecOpt},
                                        Bind {dst = (valOf var, vecTy),
                                              pinned = false,
                                              src = Operand.cast (sequence, vecTy)})
                                    end
                               | Prim.Array_uninit =>
                                    let
                                       val sequence = a 0
                                       val sequenceTy = varType (arg 0)
                                       val index = a 1
                                       val eltTys =
                                          case S.Type.deSequenceOpt sequenceTy of
                                             NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Array_uninit"
                                           | SOME eltTys => eltTys
                                       val sss =
                                          Vector.toListKeepAllMapi
                                          (S.Prod.dest eltTys, fn (offset, {elt, ...}) =>
                                           case toRtype elt of
                                              NONE => NONE
                                            | SOME elt =>
                                                 if not (Type.isObjptr elt)
                                                    then NONE
                                                    else (SOME o update)
                                                         {base = Base.SequenceSub
                                                                 {index = index,
                                                                  sequence = sequence},
                                                          baseTy = sequenceTy,
                                                          offset = offset,
                                                          value = bogus elt})
                                    in
                                       adds (List.concat sss)
                                    end
                               | Prim.Array_uninitIsNop =>
                                    let
                                       val sequenceTy = varType (arg 0)
                                       val eltTys =
                                          case S.Type.deSequenceOpt sequenceTy of
                                             NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Array_uninitIsNop"
                                           | SOME eltTys => eltTys
                                       val isNop =
                                          Vector.forall
                                          (S.Prod.dest eltTys, fn {elt, ...} =>
                                           case toRtype elt of
                                              NONE => true
                                            | SOME elt => not (Type.isObjptr elt))
                                    in
                                       move (Operand.bool isNop)
                                    end
                               | Prim.CFunction f => simpleCCall f
                               | Prim.CPointer_getCPointer => cpointerGet ()
                               | Prim.CPointer_getObjptr => cpointerGet ()
                               | Prim.CPointer_getReal _ => cpointerGet ()
                               | Prim.CPointer_getWord _ => cpointerGet ()
                               | Prim.CPointer_setCPointer => cpointerSet ()
                               | Prim.CPointer_setObjptr => cpointerSet ()
                               | Prim.CPointer_setReal _ => cpointerSet ()
                               | Prim.CPointer_setWord _ => cpointerSet ()
                               | Prim.GC_collect =>
                                    ccall
                                    {args = (Vector.new3
                                             (GCState,
                                              Operand.zero (WordSize.csize ()),
                                              Operand.bool true)),
                                     func = (CFunction.gc
                                             {maySwitchThreads = handlesSignals})}
                               | Prim.GC_state => move GCState
                               | Prim.IntInf_add =>
                                    simpleCCallWithGCState (CFunction.intInfBinary prim)
                               | Prim.IntInf_andb =>
                                    simpleCCallWithGCState (CFunction.intInfBinary prim)
                               | Prim.IntInf_arshift =>
                                    simpleCCallWithGCState (CFunction.intInfShift prim)
                               | Prim.IntInf_compare =>
                                    simpleCCallWithGCState (CFunction.intInfCompare prim)
                               | Prim.IntInf_gcd =>
                                    simpleCCallWithGCState (CFunction.intInfBinary prim)
                               | Prim.IntInf_lshift =>
                                    simpleCCallWithGCState (CFunction.intInfShift prim)
                               | Prim.IntInf_mul =>
                                    simpleCCallWithGCState (CFunction.intInfBinary prim)
                               | Prim.IntInf_neg =>
                                    simpleCCallWithGCState (CFunction.intInfUnary prim)
                               | Prim.IntInf_notb =>
                                    simpleCCallWithGCState (CFunction.intInfUnary prim)
                               | Prim.IntInf_orb =>
                                    simpleCCallWithGCState (CFunction.intInfBinary prim)
                               | Prim.IntInf_quot =>
                                    simpleCCallWithGCState (CFunction.intInfBinary prim)
                               | Prim.IntInf_rem =>
                                    simpleCCallWithGCState (CFunction.intInfBinary prim)
                               | Prim.IntInf_sub =>
                                    simpleCCallWithGCState (CFunction.intInfBinary prim)
                               | Prim.IntInf_toString =>
                                    simpleCCallWithGCState (CFunction.intInfToString prim)
                               | Prim.IntInf_toVector => cast ()
                               | Prim.IntInf_toWord => cast ()
                               | Prim.IntInf_xorb =>
                                    simpleCCallWithGCState (CFunction.intInfBinary prim)
                               | Prim.MLton_bug =>
                                    loop
                                    (i - 1, [],
                                     Transfer.CCall {args = vos args,
                                                     func = CFunction.bug (),
                                                     return = NONE})
                               | Prim.MLton_bogus =>
                                    (case toRtype ty of
                                        NONE => none ()
                                      | SOME t => move (bogus t))
                               | Prim.MLton_eq =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => move (Operand.bool true)
                                      | SOME t =>
                                           let
                                              val ws = WordSize.fromBits (Type.width t)
                                              val wordEqual = Prim.Word_equal ws
                                              val args = varOps args
                                              val (prim, args) =
                                                 case Type.toCType t of
                                                    CType.CPointer =>
                                                       (Prim.CPointer_equal, args)
                                                  | CType.Objptr =>
                                                       (wordEqual,
                                                        Vector.map
                                                        (args, fn arg =>
                                                         Operand.cast (arg, Type.word ws)))
                                                  | CType.Word8 => (wordEqual, args)
                                                  | CType.Word16 => (wordEqual, args)
                                                  | CType.Word32 => (wordEqual, args)
                                                  | CType.Word64 => (wordEqual, args)
                                                  | _ => Error.bug (concat ["SsaToRssa.translateStatementsTransfer: PrimApp,MLton_eq,", Layout.toString (Type.layout t)])
                                           in
                                              codegenOrC (prim, args)
                                           end)
                               | Prim.MLton_halt =>
                                    simpleCCallWithGCState
                                    (CFunction.halt ())
                               | Prim.MLton_installSignalHandler => none ()
                               | Prim.MLton_share =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => none ()
                                      | SOME t =>
                                           if not (Type.isObjptr t)
                                              then none ()
                                           else
                                              simpleCCallWithGCState
                                              (CFunction.share (Operand.ty (a 0))))
                               | Prim.MLton_size =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => move (Operand.word (WordX.zero (WordSize.csize ())))
                                      | SOME t =>
                                           if not (Type.isObjptr t)
                                              then move (Operand.word (WordX.zero (WordSize.csize ())))
                                           else
                                              simpleCCallWithGCState
                                              (CFunction.size (Operand.ty (a 0))))
                               | Prim.MLton_touch =>
                                    let
                                       val a  = arg 0
                                    in
                                       if isSome (toRtype (varType a))
                                          then primApp (prim, Vector.new1 (varOp a))
                                          else none ()
                                    end
                               | Prim.Thread_atomicBegin =>
                                    (* gcState.atomicState++;
                                     * if (gcState.signalsInfo.signalIsPending)
                                     *   gcState.limit = gcState.limitPlusSlop - LIMIT_SLOP;
                                     *)
                                    split
                                    (Vector.new0 (), Kind.Jump, ss,
                                     fn continue =>
                                     let
                                        datatype z = datatype GCField.t
                                        val tmp = Var.newNoname ()
                                        val size = WordSize.cpointer ()
                                        val ty = Type.cpointer ()
                                        val statements =
                                           Vector.new2
                                           (Statement.PrimApp
                                            {args = (Vector.new2
                                                     (Runtime LimitPlusSlop,
                                                      Operand.word
                                                      (WordX.fromBytes
                                                       (Runtime.limitSlop,
                                                        size)))),
                                             dst = SOME (tmp, ty),
                                             prim = Prim.CPointer_sub},
                                            Statement.Move
                                            {dst = Runtime Limit,
                                             src = Var {ty = ty, var = tmp}})
                                        val signalIsPending =
                                           newBlock
                                           {args = Vector.new0 (),
                                            kind = Kind.Jump,
                                            statements = statements,
                                            transfer = (Transfer.Goto
                                                        {args = Vector.new0 (),
                                                         dst = continue})}
                                     in
                                        (bumpAtomicState 1,
                                         if handlesSignals
                                            then
                                               Transfer.ifBool
                                               (Runtime SignalIsPending,
                                                {falsee = continue,
                                                 truee = signalIsPending})
                                         else
                                            Transfer.Goto {args = Vector.new0 (),
                                                           dst = continue})
                                     end)
                               | Prim.Thread_atomicEnd =>
                                    (* gcState.atomicState--;
                                     * if (gcState.signalsInfo.signalIsPending
                                     *     and 0 == gcState.atomicState)
                                     *   gc;
                                     *)
                                    split
                                    (Vector.new0 (), Kind.Jump, ss,
                                     fn continue =>
                                     let
                                        datatype z = datatype GCField.t
                                        val func =
                                           CFunction.gc {maySwitchThreads = true}
                                        val returnFromHandler =
                                           newBlock
                                           {args = Vector.new0 (),
                                            kind = Kind.CReturn {func = func},
                                            statements = Vector.new0 (),
                                            transfer =
                                            Goto {args = Vector.new0 (),
                                                  dst = continue}}
                                        val args =
                                           Vector.new3
                                           (GCState,
                                            Operand.zero (WordSize.csize ()),
                                            Operand.bool false)
                                        val switchToHandler =
                                           newBlock
                                           {args = Vector.new0 (),
                                            kind = Kind.Jump,
                                            statements = Vector.new0 (),
                                            transfer =
                                            Transfer.CCall
                                            {args = args,
                                             func = func,
                                             return = SOME returnFromHandler}}
                                        val testAtomicState =
                                           newBlock
                                           {args = Vector.new0 (),
                                            kind = Kind.Jump,
                                            statements = Vector.new0 (),
                                            transfer =
                                            Transfer.ifZero
                                            (Runtime AtomicState,
                                             {falsee = continue,
                                              truee = switchToHandler})}
                                     in
                                        (bumpAtomicState ~1,
                                         if handlesSignals
                                            then
                                               Transfer.ifBool
                                               (Runtime SignalIsPending,
                                                {falsee = continue,
                                                 truee = testAtomicState})
                                         else
                                            Transfer.Goto {args = Vector.new0 (),
                                                           dst = continue})
                                     end)
                               | Prim.Thread_atomicState =>
                                    move (Runtime GCField.AtomicState)
                               | Prim.Thread_copy =>
                                    simpleCCallWithGCState
                                    (CFunction.copyThread ())
                               | Prim.Thread_switchTo =>
                                    ccall {args = (Vector.new3
                                                   (GCState,
                                                    a 0,
                                                    Operand.zero (WordSize.csize ()))),
                                           func = CFunction.threadSwitchTo ()}
                               | Prim.Vector_length => arrayOrVectorLength ()
                               | Prim.Weak_canGet =>
                                    ifIsWeakPointer
                                    (varType (arg 0),
                                     fn _ =>
                                     simpleCCallWithGCState
                                     (CFunction.weakCanGet
                                      {arg = Operand.ty (a 0)}),
                                     fn () => move (Operand.bool false))
                               | Prim.Weak_get =>
                                    ifIsWeakPointer
                                    (varType (arg 0),
                                     fn t =>
                                     simpleCCallWithGCState
                                     (CFunction.weakGet
                                      {arg = Operand.ty (a 0),
                                       return = t}),
                                     fn () => (case toRtype ty of
                                                  NONE => none ()
                                                | SOME t => move (bogus t)))
                               | Prim.Weak_new =>
                                    ifIsWeakPointer
                                    (ty,
                                     fn t =>
                                     let
                                        val result = valOf (toRtype ty)
                                        val header =
                                           ObjptrTycon
                                           (case Type.deObjptr result of
                                               NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Weak_new"
                                             | SOME opt => opt)
                                        val func =
                                           CFunction.weakNew {arg = t,
                                                              return = result}
                                     in
                                        ccall {args = (Vector.concat
                                                       [Vector.new2
                                                        (GCState, header),
                                                        vos args]),
                                               func = func}
                                     end,
                                     none)
                               | Prim.Word_equal s =>
                                    simpleCodegenOrC
                                    (Prim.Word_equal
                                     (WordSize.roundUpToPrim s))
                               | Prim.Word_toIntInf => cast ()
                               | Prim.Word_extdToWord (s1, s2, {signed}) =>
                                    if WordSize.equals (s1, s2)
                                       then move (a 0)
                                    else
                                       let
                                          val signed =
                                             signed
                                             andalso Bits.< (WordSize.bits s1,
                                                             WordSize.bits s2)
                                          val s1 = WordSize.roundUpToPrim s1
                                          val s2 = WordSize.roundUpToPrim s2
                                       in
                                          if WordSize.equals (s1, s2)
                                             then cast ()
                                          else
                                             simpleCodegenOrC
                                             (Prim.Word_extdToWord
                                              (s1, s2, {signed = signed}))
                                       end
                               | Prim.WordVector_toIntInf => cast ()
                               | Prim.WordArray_subWord {eleSize, ...} =>
                                    subWord eleSize
                               | Prim.WordArray_updateWord {eleSize, ...} =>
                                    let
                                       val ty = Type.word eleSize
                                    in
                                       add (Move {dst = (SequenceOffset
                                                         {base = a 0,
                                                          index = a 1,
                                                          offset = Bytes.zero,
                                                          scale = Type.scale ty,
                                                          ty = ty}),
                                                  src = a 2})
                                    end
                               | Prim.WordVector_subWord {eleSize, ...} =>
                                    subWord eleSize
                               | Prim.World_save =>
                                    simpleCCallWithGCState
                                    (CFunction.worldSave ())
                               | _ => simpleCodegenOrC prim
                           end
                      | S.Exp.Select {base, offset} =>
                           (case var of
                               NONE => none ()
                             | SOME var =>
                                  (case toRtype ty of
                                      NONE => none ()
                                    | SOME ty =>
                                         adds
                                         (select
                                          {base = Base.map (base, varOp),
                                           baseTy = varType (Base.object base),
                                           dst = (var, ty),
                                           offset = offset})))
                      | S.Exp.Sequence {args} =>
                           (case toRtype ty of
                               NONE => none ()
                             | SOME dstTy =>
                                  adds (sequence {args = args,
                                                  dst = (valOf var, dstTy),
                                                  sequenceTy = ty,
                                                  oper = varOp}))
                      | S.Exp.Var y =>
                           (case toRtype ty of
                               NONE => none ()
                             | SOME _ => move (varOp y))
                  end
                  end
         in
            loop (Vector.length statements - 1, ss, transfer)
         end
      fun translateBlock (S.Block.T {label, args, statements, transfer}) =
         let
            val (ss, t) = translateTransfer transfer
            val (ss, t) = translateStatementsTransfer (statements, ss, t)
         in
            Block.T {args = translateFormals args,
                     kind = Kind.Jump,
                     label = label,
                     statements = ss,
                     transfer = t}
         end
      fun translateFunction (f: S.Function.t): Function.t =
         let
            val _ =
               S.Function.foreachVar (f, fn (x, t) => setVarInfo (x, {ty = t}))
            val {args, blocks, name, raises, returns, start, ...} =
               S.Function.dest f
            val _ =
               Vector.foreach
               (blocks, fn S.Block.T {label, args, ...} =>
                setLabelInfo (label, {args = args,
                                      cont = ref [],
                                      handler = ref NONE}))
            val blocks = Vector.map (blocks, translateBlock)
            val blocks = Vector.concat [Vector.fromList (!extraBlocks), blocks]
            val _ = extraBlocks := []
            fun transTypes (ts : S.Type.t vector option)
               : Type.t vector option =
               Option.map (ts, fn ts => Vector.keepAllMap (ts, toRtype))
         in
            Function.new {args = translateFormals args,
                          blocks = blocks,
                          name = name,
                          raises = transTypes raises,
                          returns = transTypes returns,
                          start = start}
         end

      val main =
         let
            val start = Label.newNoname ()
         in
            translateFunction
            (S.Function.profile
             (S.Function.new
              {args = Vector.new0 (),
               blocks = (Vector.new1
                         (S.Block.T
                          {label = start,
                           args = Vector.new0 (),
                           statements = globals,
                           transfer = (S.Transfer.Call
                                       {args = Vector.new0 (),
                                        func = main,
                                        return = S.Return.Tail})})),
               mayInline = false, (* doesn't matter *)
               name = Func.newString "initGlobals",
               raises = NONE,
               returns = NONE,
               start = start},
              S.SourceInfo.main))
         end
      val functions = List.revMap (functions, translateFunction)
      val p = Program.T {functions = functions,
                         handlesSignals = handlesSignals,
                         main = main,
                         objectTypes = Vector.concat [objectTypes, Vector.fromListRev (!newObjectTypes)],
                         profileInfo = NONE,
                         statics = Vector.new0 ()}
      val _ = Program.clear p
   in
      p
   end

end
