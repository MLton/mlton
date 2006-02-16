(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor SsaToRssa (S: SSA_TO_RSSA_STRUCTS): SSA_TO_RSSA =
struct

open S
open Rssa

datatype z = datatype IntSize.prim
datatype z = datatype WordSize.prim

structure S = Ssa

local
   open Ssa
in
   structure Base = Base
end
   
local
   open Runtime
in
   structure GCField = GCField
end

structure Prim =
   struct
      open Prim

      type t = Type.t Prim.t
   end

structure CFunction =
   struct
      open CFunction
      open Type.BuiltInCFunction

      type t = Type.t CFunction.t

      local
         open Type
      in
         val gcState = gcState
         val Word32 = word (Bits.fromInt 32)
         val unit = unit
      end

      datatype z = datatype Convention.t
      datatype z = datatype Target.t
         
      val copyCurrentThread =
         T {args = Vector.new1 gcState,
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = let
                           open CType
                        in
                           (Vector.new1 Pointer, NONE)
                        end,
            readsStackTop = true,
            return = unit,
            target = Direct "GC_copyCurrentThread",
            writesStackTop = true}

      val copyThread =
         T {args = Vector.new2 (gcState, Type.thread),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = let
                           open CType
                        in
                           (Vector.new2 (Pointer, Pointer), SOME Pointer)
                        end,
            readsStackTop = true,
            return = Type.thread,
            target = Direct "GC_copyThread",
            writesStackTop = true}

      val exit =
         T {args = Vector.new1 Word32,
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = let
                           open CType
                        in
                           (Vector.new1 Word32, NONE)
                        end,
            readsStackTop = true,
            return = unit,
            target = Direct "MLton_exit",
            writesStackTop = true}

      fun gcArrayAllocate {return} =
         T {args = Vector.new4 (gcState, Word32, Word32, Word32),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = true,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = let
                           open CType
                        in
                           (Vector.new4 (Pointer, Word32, Word32, Word32),
                            SOME Pointer)
                        end,
            readsStackTop = true,
            return = return,
            target = Direct "GC_arrayAllocate",
            writesStackTop = true}

      val returnToC =
         T {args = Vector.new0 (),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = true,
            modifiesFrontier = true,
            prototype = let
                           open CType
                        in
                           (Vector.new0 (), NONE)
                        end,
            readsStackTop = true,
            return = unit,
            target = Direct "Thread_returnToC",
            writesStackTop = true}

      val threadSwitchTo =
         T {args = Vector.new2 (Type.thread, Word32),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = true,
            mayGC = true,
            maySwitchThreads = true,
            modifiesFrontier = true,
            prototype = let
                           open CType
                        in
                           (Vector.new2 (Pointer, Word32), NONE)
                        end,
            readsStackTop = true,
            return = unit,
            target = Direct "Thread_switchTo",
            writesStackTop = true}

      fun weakCanGet t =
         vanilla {args = Vector.new1 t,
                  name = "GC_weakCanGet",
                  prototype = let
                                 open CType
                              in
                                 (Vector.new1 Pointer, SOME bool)
                              end,
                  return = Type.bool}
         
      fun weakGet {arg, return} =
         vanilla {args = Vector.new1 arg,
                  name = "GC_weakGet",
                  prototype = let
                                 open CType
                              in
                                 (Vector.new1 Pointer, SOME Pointer)
                              end,
                  return = return}
                  
      fun weakNew {arg, return} =
         T {args = Vector.new3 (gcState, Word32, arg),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = let
                           open CType
                        in
                           (Vector.new3 (Pointer, Word32, Pointer), SOME Pointer)
                        end,
            readsStackTop = true,
            return = return,
            target = Direct "GC_weakNew",
            writesStackTop = true}

      val worldSave =
         T {args = Vector.new2 (gcState, Word32),
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = true,
            maySwitchThreads = false,
            modifiesFrontier = true,
            prototype = let
                           open CType
                        in
                           (Vector.new2 (Pointer, Word32), NONE)
                        end,
            readsStackTop = true,
            return = unit,
            target = Direct "GC_saveWorld",
            writesStackTop = true}

      fun share t =
         T {args = Vector.new1 t,
            bytesNeeded = NONE,
            convention = Cdecl,
            ensuresBytesFree = false,
            mayGC = false,
            maySwitchThreads = false,
            modifiesFrontier = true, (* actually, just readsFrontier *)
            prototype = let
                           open CType
                        in
                           (Vector.new1 Pointer, NONE)
                        end,
            readsStackTop = false,
            return = unit,
            target = Direct "MLton_share",
            writesStackTop = false}

      fun size t =
         vanilla {args = Vector.new1 t,
                  name = "MLton_size",
                  prototype = let
                                 open CType
                              in
                                 (Vector.new1 Pointer, SOME Word32)
                              end,
                  return = Word32}
   end

structure Name =
   struct
      open Prim.Name

      type t = Type.t t

      fun cFunctionRaise (n: t): CFunction.t =
         let
            datatype z = datatype CFunction.Convention.t
            datatype z = datatype CFunction.Target.t
            val name = toString n
            val real = Type.real
            val word = Type.word o WordSize.bits
            val vanilla = CFunction.vanilla
            fun coerce (t1, t2, sg) =
               vanilla {args = Vector.new1 t1,
                        name = name,
                        prototype = (Vector.new1
                                     (CType.word
                                      (WordSize.fromBits (Type.width t1), sg)),
                                     SOME (Type.toCType t2)),
                        return = t2}
            fun amAllocationProfiling () =
               Control.ProfileAlloc = !Control.profile
            fun intInfBinary () =
               CFunction.T {args = Vector.new3 (Type.intInf, Type.intInf,
                                                Type.defaultWord),
                            bytesNeeded = SOME 2,
                            convention = Cdecl,
                            ensuresBytesFree = false,
                            mayGC = false,
                            maySwitchThreads = false,
                            modifiesFrontier = true,
                            prototype = let
                                           open CType
                                        in
                                           (Vector.new3 (Pointer, Pointer, Word32),
                                            SOME Pointer)
                                        end,
                            readsStackTop = amAllocationProfiling (),
                            return = Type.intInf,
                            target = Direct name,
                            writesStackTop = false}
            fun intInfShift () =
               CFunction.T {args = Vector.new3 (Type.intInf,
                                                Type.defaultWord,
                                                Type.defaultWord),
                            bytesNeeded = SOME 2,
                            convention = Cdecl,
                            ensuresBytesFree = false,
                            mayGC = false,
                            maySwitchThreads = false,
                            modifiesFrontier = true,
                            prototype = let
                                           open CType
                                        in
                                           (Vector.new3 (Pointer, Word32, Word32),
                                            SOME Pointer)
                                        end,
                            readsStackTop = amAllocationProfiling (),
                            return = Type.intInf,
                            target = Direct name,
                            writesStackTop = false}
            fun intInfToString () =
               CFunction.T {args = Vector.new3 (Type.intInf,
                                                Type.defaultWord,
                                                Type.defaultWord),
                            bytesNeeded = SOME 2,
                            convention = Cdecl,
                            ensuresBytesFree = false,
                            mayGC = false,
                            maySwitchThreads = false,
                            modifiesFrontier = true,
                            prototype = let
                                           open CType
                                        in
                                           (Vector.new3 (Pointer, Word32, Word32),
                                            SOME Pointer)
                                        end,
                            readsStackTop = amAllocationProfiling (),
                            return = Type.string,
                            target = Direct name,
                            writesStackTop = false}
            fun intInfUnary () =
               CFunction.T {args = Vector.new2 (Type.intInf, Type.defaultWord),
                            bytesNeeded = SOME 1,
                            convention = Cdecl,
                            ensuresBytesFree = false,
                            mayGC = false,
                            maySwitchThreads = false,
                            modifiesFrontier = true,
                            prototype = let
                                           open CType
                                        in
                                           (Vector.new2 (Pointer, Word32),
                                            SOME Pointer)
                                        end,
                            readsStackTop = amAllocationProfiling (),
                            return = Type.intInf,
                            target = Direct name,
                            writesStackTop = false}
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
               fun makeOverflows n (s, sg) =
                  let
                     val t = word s
                     val ct = CType.word (s, sg)
                  in
                     vanilla {args = Vector.new (n, t),
                              name = name ^ "Overflows",
                              prototype = (Vector.new (n, ct), SOME CType.bool),
                              return = Type.bool}
                  end
            in
               val wordBinary = make 2
               val wordBinaryOverflows = makeOverflows 2
               val wordUnary = make 1
               val wordUnaryOverflows = makeOverflows 1
            end
            fun wordCompare (s, sg) =
               let
                  val t = word s
               in
                  vanilla {args = Vector.new2 (t, t),
                           name = name,
                           prototype = let
                                          val t = CType.word (s, sg)
                                       in
                                          (Vector.new2 (t, t), SOME CType.bool)
                                       end,
                           return = Type.bool}
               end
            fun wordShift (s, sg) =
               let
                  val t = word s
               in
                  vanilla {args = Vector.new2 (t, Type.defaultWord),
                           name = name,
                           prototype = let
                                          val t = CType.word (s, sg)
                                       in
                                          (Vector.new2 (t, CType.Word32), SOME t)
                                       end,
                           return = t}
               end
         in
            case n of
               IntInf_add => intInfBinary ()
             | IntInf_andb => intInfBinary ()
             | IntInf_arshift => intInfShift ()
             | IntInf_compare => 
                  vanilla {args = Vector.new2 (Type.intInf, Type.intInf),
                           name = name,
                           prototype = let
                                          open CType
                                       in
                                          (Vector.new2 (Pointer, Pointer),
                                           SOME Int32)
                                       end,
                           return = Type.defaultWord}
             | IntInf_equal =>
                  vanilla {args = Vector.new2 (Type.intInf, Type.intInf),
                           name = name,
                           prototype = let
                                          open CType
                                       in
                                          (Vector.new2 (Pointer, Pointer),
                                           SOME bool)
                                       end,
                           return = Type.bool}
             | IntInf_gcd => intInfBinary ()
             | IntInf_lshift => intInfShift ()
             | IntInf_mul => intInfBinary ()
             | IntInf_neg => intInfUnary ()
             | IntInf_notb => intInfUnary ()
             | IntInf_orb => intInfBinary ()
             | IntInf_quot => intInfBinary ()
             | IntInf_rem => intInfBinary ()
             | IntInf_sub => intInfBinary ()
             | IntInf_toString => intInfToString ()
             | IntInf_xorb => intInfBinary ()
             | MLton_bug => CFunction.bug
             | Real_Math_acos s => realUnary s
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
             | Real_div s => realBinary s
             | Real_equal s => realCompare s
             | Real_ldexp s =>
                  let
                     val t = real s
                     val ct = CType.real s
                  in
                     vanilla {args = Vector.new2 (t, Type.defaultWord),
                              name = name,
                              prototype = (Vector.new2 (ct, CType.Int32),
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
             | Real_round s => realUnary s
             | Real_sub s => realBinary s
             | Thread_returnToC => CFunction.returnToC
             | Word_add s => wordBinary (s, {signed = false})
             | Word_addCheck (s, sg) => wordBinaryOverflows (s, sg)
             | Word_andb s => wordBinary (s, {signed = false})
             | Word_equal s => wordCompare (s, {signed = false})
             | Word_lshift s => wordShift (s, {signed = false})
             | Word_lt z => wordCompare z
             | Word_mul z => wordBinary z
             | Word_mulCheck (s, sg) => wordBinaryOverflows (s, sg)
             | Word_neg s => wordUnary (s, {signed = true})
             | Word_negCheck s => wordUnaryOverflows (s, {signed = true})
             | Word_notb s => wordUnary (s, {signed = false})
             | Word_orb s => wordBinary (s, {signed = false})
             | Word_quot z => wordBinary z
             | Word_rem z => wordBinary z
             | Word_rol s => wordShift (s, {signed = false})
             | Word_ror s => wordShift (s, {signed = false})
             | Word_rshift z => wordShift z
             | Word_sub s => wordBinary (s, {signed = false})
             | Word_subCheck (s, sg) => wordBinaryOverflows (s, sg)
             | Word_toReal (s1, s2, sg) =>
                  coerce (Type.word (WordSize.bits s1), Type.real s2, sg)
             | Word_toWord (s1, s2, sg) =>
                  coerce (Type.word (WordSize.bits s1),
                          Type.word (WordSize.bits s2),
                          sg)
             | Word_xorb s => wordBinary (s, {signed = false})
             | _ => Error.bug "SsaToRssa.Name.cFunctionRaise"
         end

      fun cFunction n = SOME (cFunctionRaise n) handle _ => NONE
   end

datatype z = datatype Operand.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure PackedRepresentation = PackedRepresentation (structure Rssa = Rssa
                                                       structure Ssa = Ssa)

structure Type =
   struct
      open Type
         
      fun scale (ty: t): Scale.t =
         case Scale.fromInt (Bytes.toInt (bytes ty)) of
            NONE => Error.bug "SsaToRssa.Type.scale"
          | SOME s => s
   end

val cardSizeLog2 : IntInf.t = 8 (* must agree with CARD_SIZE_LOG2 in gc.c *)

fun updateCard (addr: Operand.t): Statement.t list =
   let
      val index = Var.newNoname ()
      val indexTy = Type.defaultWord
   in
      [PrimApp {args = (Vector.new2
                        (addr,
                         Operand.word
                         (WordX.fromIntInf (cardSizeLog2, WordSize.default)))),
                dst = SOME (index, indexTy),
                prim = Prim.wordRshift (WordSize.default,
                                        {signed = false})},
       Move {dst = (ArrayOffset
                    {base = Runtime GCField.CardMap,
                     index = Var {ty = indexTy, var = index},
                     offset = Bytes.zero,
                     scale = Scale.One,
                     ty = Type.word Bits.inByte}),
             src = Operand.word (WordX.one (WordSize.fromBits Bits.inByte))}]
   end

fun convertConst (c: Const.t): Const.t =
   let
      datatype z = datatype Const.t
   in
      case c of
         Word w => Word (WordX.resize (w, WordSize.roundUpToPrim (WordX.size w)))
       | _ => c
   end

fun convert (program as S.Program.T {functions, globals, main, ...},
             {codegenImplementsPrim: Rssa.Type.t Rssa.Prim.t -> bool}): Rssa.Program.t =
   let
      val {diagnostic, genCase, object, objectTypes, select, toRtype, update} =
         PackedRepresentation.compute program
      val objectTypes = Vector.concat [ObjectType.basic, objectTypes]
      val () =
         Vector.foreachi
         (objectTypes, fn (i, (pt, _)) => PointerTycon.setIndex (pt, i))
      val objectTypes = Vector.map (objectTypes, #2)
      val () = diagnostic ()
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
                          cases: S.Cases.t,
                          default: Label.t option})
         : Statement.t list * Transfer.t =
         case cases of
            S.Cases.Con cases =>
               (case (Vector.length cases, default) of
                   (0, NONE) => ([], Transfer.bug)
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
          | S.Cases.Word (s, cs) =>
               ([],
                Switch
                (Switch.T
                 {cases = (QuickSort.sortVector
                           (cs, fn ((w, _), (w', _)) =>
                            WordX.le (w, w', {signed = false}))),
                  default = default,
                  size = s,
                  test = varOp test}))
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
          case Prim.name p of
             Prim.Name.MLton_installSignalHandler => true
           | _ => false)
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
            S.Transfer.Arith {args, overflow, prim, success, ty} =>
               let
                  val prim = translatePrim prim
                  val ty = valOf (toRtype ty)
                  val res = Var.newNoname ()
                  val noOverflow =
                     newBlock
                     {args = Vector.new0 (),
                      kind = Kind.Jump,
                      statements = Vector.new0 (),
                      transfer = (Transfer.Goto
                                  {dst = success,
                                   args = (Vector.new1
                                           (Var {var = res, ty = ty}))})}
               in
                  if codegenImplementsPrim prim
                     then ([], 
                           Transfer.Arith {dst = res,
                                           args = vos args,
                                           overflow = overflow,
                                           prim = prim,
                                           success = noOverflow,
                                           ty = ty})
                     else 
                        let
                           datatype z = datatype Prim.Name.t
                           fun doOperCheckCF (operCheck) =
                              let
                                 val operCheckCF =
                                    case Name.cFunction operCheck of
                                       NONE =>
                                          Error.bug
                                          (concat ["SsaToRssa.translateTransfer: ",
                                                   "unimplemented arith:",
                                                   Name.toString operCheck])
                                     | SOME operCheckCF => operCheckCF
                                 val afterOperCheck =
                                    let
                                       val checkRes = Var.newNoname ()
                                    in
                                       newBlock
                                       {args = Vector.new1 (checkRes, Type.bool),
                                        kind = Kind.CReturn {func = operCheckCF},
                                        statements = Vector.new0 (),
                                        transfer = (Transfer.ifBool
                                                    (Var {var = checkRes,
                                                          ty = Type.bool}, 
                                                     {falsee = noOverflow, 
                                                      truee = overflow}))}
                                    end
                              in
                                 Transfer.CCall
                                 {args = vos args,
                                  func = operCheckCF,
                                  return = SOME afterOperCheck}
                              end
                           fun doOperCF (oper, operCheck) =
                              let
                                 val operCF =
                                    case Name.cFunction oper of
                                       NONE =>
                                          Error.bug
                                          (concat ["SsaToRssa.translateTransfer: ",
                                                   "unimplemented arith:",
                                                   Name.toString oper])
                                     | SOME operCF => operCF
                                 val afterOper =
                                    newBlock
                                    {args = Vector.new1 (res, ty),
                                     kind = Kind.CReturn {func = operCF},
                                     statements = Vector.new0 (),
                                     transfer = doOperCheckCF operCheck}
                              in
                                 Transfer.CCall
                                 {args = vos args,
                                  func = operCF,
                                  return = SOME afterOper}
                              end
                           fun doPrim prim =
                              [Statement.PrimApp
                               {dst = SOME (res, ty),
                                prim = prim,
                                args = vos args}]
                           fun doit (prim, operCheck) =
                              if codegenImplementsPrim prim
                                 then (doPrim prim, doOperCheckCF operCheck)
                              else ([], doOperCF (Prim.name prim, operCheck))
                        in
                           case Prim.name prim of
                              Word_addCheck (s, sg) => 
                                 doit (Prim.wordAdd s, 
                                       Word_addCheck (s, sg))
                            | Word_mulCheck (s, sg) => 
                                 doit (Prim.wordMul (s, sg),
                                       Word_mulCheck (s, sg))
                            | Word_negCheck s => 
                                 doit (Prim.wordNeg s, 
                                       Word_negCheck s)
                            | Word_subCheck (s, sg) => 
                                 doit (Prim.wordSub s, 
                                       Word_subCheck (s, sg))
                            | _ => Error.bug (concat ["SsaToRssa.translateTransfer: ",
                                                      "strange arith:",
                                                      Name.toString (Prim.name prim)])
                        end
               end
          | S.Transfer.Bug => ([], Transfer.bug)
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
               let
                  datatype z = datatype Prim.Name.t
               in
                  case Prim.name prim of
                     MLton_halt =>
                        ([], Transfer.CCall {args = vos args,
                                             func = CFunction.exit,
                                             return = NONE})
                   | Thread_copyCurrent =>
                        let
                           val func = CFunction.copyCurrentThread
                           val l =
                              newBlock {args = Vector.new0 (),
                                        kind = Kind.CReturn {func = func},
                                        statements = Vector.new0 (),
                                        transfer =
                                        (Goto {args = Vector.new0 (),
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
                                      Prim.toString prim])
               end
      fun translateStatementsTransfer (statements, ss, transfer) =
         let
            fun loop (i, ss, t): Statement.t vector * Transfer.t =
               if i < 0
                  then (Vector.fromList ss, t)
               else
                  let
                     fun none () = loop (i - 1, ss, t)
                     fun add s = loop (i - 1, s :: ss, t)
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
                                           andalso Type.isPointer t
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
                                         isMutable = false,
                                         src = f ty})
                     fun move (src: Operand.t) = maybeMove (fn _ => src)
                  in
                     case exp of
                        S.Exp.Const c => move (Const (convertConst c))
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
                                              if Type.isPointer t
                                                 then yes t
                                              else no ())
                                  | _ => Error.bug "SsaToRssa.ifIsWeakPointer"
                              fun arrayOrVectorLength () =
                                 move (Offset
                                       {base = a 0,
                                        offset = Runtime.arrayLengthOffset,
                                        ty = Type.defaultWord})
                              fun subWord () =
                                 move (ArrayOffset {base = a 0,
                                                    index = a 1,
                                                    offset = Bytes.zero,
                                                    scale = Type.scale Type.defaultWord,
                                                    ty = Type.defaultWord})
                              fun dst () =
                                 case var of
                                    SOME x =>
                                       Option.map (toRtype (varType x), fn t =>
                                                   (x, t))
                                  | NONE => NONE
                              fun primApp prim =
                                 add (PrimApp {dst = dst (),
                                               prim = prim,
                                               args = varOps args})
                              datatype z = datatype Prim.Name.t
                              fun bumpCanHandle n =
                                 let
                                    val canHandle = Runtime GCField.CanHandle
                                    val res = Var.newNoname ()
                                    val resTy = Operand.ty canHandle
                                 in
                                    [Statement.PrimApp
                                     {args = (Vector.new2
                                              (canHandle,
                                               (Operand.word
                                                (WordX.fromIntInf
                                                 (IntInf.fromInt n,
                                                  WordSize.default))))),
                                      dst = SOME (res, resTy),
                                      prim = Prim.wordAdd WordSize.default},
                                     Statement.Move
                                     {dst = canHandle,
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
                              fun array (numElts: Operand.t) =
                                 let
                                    val result = valOf (toRtype ty)
                                    val pt =
                                       case Type.dePointer result of
                                          NONE => Error.bug "SsaToRssa.array"
                                        | SOME pt => PointerTycon pt
                                    val args =
                                       Vector.new4 (GCState,
                                                    EnsuresBytesFree,
                                                    numElts,
                                                    pt)
                                    val func =
                                       CFunction.gcArrayAllocate
                                       {return = result}
                                 in
                                    ccall {args = args, func = func}
                                 end
                     fun pointerGet () =
                        maybeMove (fn ty =>
                                   ArrayOffset {base = a 0,
                                                index = a 1,
                                                offset = Bytes.zero,
                                                scale = Type.scale ty,
                                                ty = ty})
                     fun pointerSet () =
                        let
                           val src = a 2
                           val ty = Operand.ty src
                        in
                           add (Move {dst = ArrayOffset {base = a 0,
                                                         index = a 1,
                                                         offset = Bytes.zero,
                                                         scale = Type.scale ty,
                                                         ty = ty},
                                      src = a 2})
                        end
                     fun codegenOrC (p: Prim.t) =
                        let
                           val n = Prim.name p
                        in
                           if codegenImplementsPrim p
                              then primApp p
                           else (case Name.cFunction n of
                                    NONE =>
                                       Error.bug (concat ["SsaToRssa.codegenOrC: ",
                                                          "unimplemented prim:",
                                                          Name.toString n])
                                  | SOME f => simpleCCall f)
                        end
                     datatype z = datatype Prim.Name.t
                           in
                              case Prim.name prim of
                                 Array_array => array (a 0)
                               | Array_length => arrayOrVectorLength ()
                               | Array_toVector =>
                                    let
                                       val array = a 0
                                       val vecTy = valOf (toRtype ty)
                                       val pt =
                                          case Type.dePointer vecTy of
                                             NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Array_toVector"
                                           | SOME pt => pt
                                    in
                                       loop
                                       (i - 1,
                                        Move
                                        {dst = (Offset
                                                {base = array,
                                                 offset = Runtime.headerOffset,
                                                 ty = Type.defaultWord}),
                                         src = PointerTycon pt}
                                        :: Bind {dst = (valOf var, vecTy),
                                                 isMutable = false,
                                                 src = Operand.cast (array, vecTy)}
                                        :: ss,
                                        t)
                                    end
                               | FFI f => simpleCCall f
                               | GC_collect =>
                                    ccall
                                    {args = (Vector.new5
                                             (GCState,
                                              Operand.zero WordSize.default,
                                              Operand.bool true,
                                              File,
                                              Line)),
                                     func = (CFunction.gc
                                             {maySwitchThreads = handlesSignals})}
                               | IntInf_toVector => cast ()
                               | IntInf_toWord => cast ()
                               | MLton_bogus =>
                                    (case toRtype ty of
                                        NONE => none ()
                                      | SOME t => move (bogus t))
                               | MLton_eq =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => move (Operand.bool true)
                                      | SOME t =>
                                           codegenOrC
                                           (Prim.wordEqual
                                            (WordSize.fromBits (Type.width t))))
                               | MLton_installSignalHandler => none ()
                               | MLton_share =>
                                    (case toRtype (varType (arg 0)) of
                                        NONE => none ()
                                      | SOME t =>
                                           if not (Type.isPointer t)
                                              then none ()
                                           else
                                              simpleCCall (CFunction.share
                                                           (Operand.ty (a 0))))
                               | MLton_size =>
                                    simpleCCall
                                    (CFunction.size (Operand.ty (a 0)))
                               | MLton_touch =>
                                    let
                                       val a = arg 0
                                       val args = 
                                          if isSome (toRtype (varType a))
                                             then Vector.new1 (varOp a)
                                          else Vector.new0 ()
                                    in
                                       add (PrimApp {args = args,
                                                     dst = NONE,
                                                     prim = prim})
                                    end
                               | Pointer_getPointer => pointerGet ()
                               | Pointer_getReal _ => pointerGet ()
                               | Pointer_getWord _ => pointerGet ()
                               | Pointer_setPointer => pointerSet ()
                               | Pointer_setReal _ => pointerSet ()
                               | Pointer_setWord _ => pointerSet ()
                               | Thread_atomicBegin =>
                                    (* gcState.canHandle++;
                                     * if (gcState.signalIsPending)
                                     *   gcState.limit = gcState.limitPlusSlop - LIMIT_SLOP;
                                     *)
                                    split
                                    (Vector.new0 (), Kind.Jump, ss,
                                     fn continue =>
                                     let
                                        datatype z = datatype GCField.t
                                        val tmp = Var.newNoname ()
                                        val size = WordSize.pointer ()
                                        val ty = Type.cPointer ()
                                        val statements =
                                           Vector.new2
                                           (Statement.PrimApp
                                            {args = (Vector.new2
                                                     (Runtime LimitPlusSlop,
                                                      Operand.word
                                                      (WordX.fromIntInf
                                                       (IntInf.fromInt
                                                        (Bytes.toInt Runtime.limitSlop),
                                                        size)))),
                                             dst = SOME (tmp, ty),
                                             prim = Prim.wordSub size},
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
                                        (bumpCanHandle 1,
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
                               | Thread_atomicEnd =>
                                    (* gcState.canHandle--;
                                     * if (gcState.signalIsPending
                                     *     and 0 == gcState.canHandle)
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
                                           Vector.new5
                                           (GCState,
                                            Operand.zero WordSize.default,
                                            Operand.bool false,
                                            File,
                                            Line)
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
                                        val testCanHandle =
                                           newBlock
                                           {args = Vector.new0 (),
                                            kind = Kind.Jump,
                                            statements = Vector.new0 (),
                                            transfer =
                                            Transfer.ifZero
                                            (Runtime CanHandle,
                                             {falsee = continue,
                                              truee = switchToHandler})}
                                     in
                                        (bumpCanHandle ~1,
                                         if handlesSignals 
                                            then 
                                               Transfer.ifBool
                                               (Runtime SignalIsPending,
                                                {falsee = continue,
                                                 truee = testCanHandle})
                                         else 
                                            Transfer.Goto {args = Vector.new0 (),
                                                           dst = continue})
                                     end)
                               | Thread_canHandle =>
                                    move (Runtime GCField.CanHandle)
                               | Thread_copy =>
                                    ccall {args = (Vector.concat
                                                   [Vector.new1 GCState,
                                                    vos args]),
                                           func = CFunction.copyThread}
                               | Thread_switchTo =>
                                    ccall {args = (Vector.new2
                                                   (a 0, EnsuresBytesFree)),
                                           func = CFunction.threadSwitchTo}
                               | Vector_length => arrayOrVectorLength ()
                               | Weak_canGet =>
                                    ifIsWeakPointer
                                    (varType (arg 0),
                                     fn _ => simpleCCall (CFunction.weakCanGet
                                                          (Operand.ty (a 0))),
                                     fn () => move (Operand.bool false))
                               | Weak_get =>
                                    ifIsWeakPointer
                                    (varType (arg 0),
                                     fn t => (simpleCCall
                                              (CFunction.weakGet
                                               {arg = Operand.ty (a 0),
                                                return = t})),
                                     none)
                               | Weak_new =>
                                    ifIsWeakPointer
                                    (ty,
                                     fn t =>
                                     let
                                        val result = valOf (toRtype ty)
                                        val header =
                                           PointerTycon
                                           (case Type.dePointer result of
                                               NONE => Error.bug "SsaToRssa.translateStatementsTransfer: PrimApp,Weak_new"
                                             | SOME pt => pt)
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
                               | Word_equal s =>
                                    codegenOrC (Prim.wordEqual
                                               (WordSize.roundUpToPrim s))
                               | Word_toIntInf => cast ()
                               | Word_toWord (s1, s2, {signed}) =>
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
                                             codegenOrC
                                             (Prim.wordToWord
                                              (s1, s2, {signed = signed}))
                                       end
                               | WordVector_toIntInf => move (a 0)
                               | Word8Array_subWord => subWord ()
                               | Word8Array_updateWord =>
                                    add (Move {dst = (ArrayOffset
                                                      {base = a 0,
                                                       index = a 1,
                                                       offset = Bytes.zero,
                                                       scale = Type.scale Type.defaultWord,
                                                       ty = Type.defaultWord}),
                                               src = a 2})
                               | Word8Vector_subWord => subWord ()
                               | World_save =>
                                    ccall {args = (Vector.new2
                                                   (GCState,
                                                    Vector.sub (vos args, 0))),
                                           func = CFunction.worldSave}
                               | _ => codegenOrC prim
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
             val bug = Label.newNoname ()
          in
             translateFunction
             (S.Function.profile
              (S.Function.new
               {args = Vector.new0 (),
                blocks = (Vector.new2
                          (S.Block.T
                           {label = start,
                            args = Vector.new0 (),
                            statements = globals,
                            transfer = (S.Transfer.Call
                                        {args = Vector.new0 (),
                                         func = main,
                                         return =
                                         S.Return.NonTail
                                         {cont = bug,
                                          handler = S.Handler.Dead}})},
                           S.Block.T
                           {label = bug,
                            args = Vector.new0 (),
                            statements = Vector.new0 (),
                            transfer = S.Transfer.Bug})),
                mayInline = false, (* doesn't matter *)
                name = Func.newNoname (),
                raises = NONE,
                returns = NONE,
                start = start},
               S.SourceInfo.main))
          end
      val functions = List.revMap (functions, translateFunction)
      val p = Program.T {functions = functions,
                         handlesSignals = handlesSignals,
                         main = main,
                         objectTypes = objectTypes}
      val _ = Program.clear p
   in
      p
   end

end
