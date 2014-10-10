(* Copyright (C) 2013-2014 Matthew Fluet, Brian Leibig.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor LLVMCodegen(S: LLVM_CODEGEN_STRUCTS): LLVM_CODEGEN =
struct

open S

open Machine

local
    open Runtime
in
    structure GCField = GCField
end

datatype z = datatype RealSize.t
datatype z = datatype WordSize.prim

(* LLVM codegen context. Contains various values/functions that should
   be shared amongst all codegen functions. *)
datatype Context = Context of {
    program: Program.t,
    labelToStringIndex: Label.t -> string,
    chunkLabelToString: ChunkLabel.t -> string,
    chunkLabelIndex: ChunkLabel.t -> int,
    labelChunk: Label.t -> ChunkLabel.t,
    entryLabels: Label.t vector,
    labelInfo: Label.t -> {block: Block.t,
                           chunkLabel: ChunkLabel.t,
                           frameIndex: int option,
                           layedOut: bool ref},
    printblock: bool,
    printstmt: bool,
    printmove: bool
}

fun ctypes () =
    concat ["%uintptr_t = type i", Bits.toString (Control.Target.Size.cpointer ()), "\n"]

val mltypes =
"; ML types\n\
\%Pointer = type i8*\n\
\%Int8 = type i8\n\
\%Int16 = type i16\n\
\%Int32 = type i32\n\
\%Int64 = type i64\n\
\%Real32 = type float\n\
\%Real64 = type double\n\
\%Word8 = type i8\n\
\%Word16 = type i16\n\
\%Word32 = type i32\n\
\%Word64 = type i64\n\
\%CPointer = type i8*\n\
\%Objptr = type i8*\n"

val llvmIntrinsics =
"declare float @llvm.sqrt.f32(float %Val)\n\
\declare double @llvm.sqrt.f64(double %Val)\n\
\declare float @llvm.sin.f32(float %Val)\n\
\declare double @llvm.sin.f64(double %Val)\n\
\declare float @llvm.cos.f32(float %Val)\n\
\declare double @llvm.cos.f64(double %Val)\n\
\declare float @llvm.exp.f32(float %Val)\n\
\declare double @llvm.exp.f64(double %Val)\n\
\declare float @llvm.log.f32(float %Val)\n\
\declare double @llvm.log.f64(double %Val)\n\
\declare float @llvm.log10.f32(float %Val)\n\
\declare double @llvm.log10.f64(double %Val)\n\
\declare float @llvm.fma.f32(float %a, float %b, float %c)\n\
\declare double @llvm.fma.f64(double %a, double %b, double %c)\n\
\declare float @llvm.fabs.f32(float %Val) ; requires LLVM 3.2\n\
\declare double @llvm.fabs.f64(double %Val) ; requires LLVM 3.2\n\
\declare float @llvm.rint.f32(float %Val) ; requires LLVM 3.3\n\
\declare double @llvm.rint.f64(double %Val) ; requires LLVM 3.3\n\
\declare {i8, i1} @llvm.sadd.with.overflow.i8(i8 %a, i8 %b)\n\
\declare {i16, i1} @llvm.sadd.with.overflow.i16(i16 %a, i16 %b)\n\
\declare {i32, i1} @llvm.sadd.with.overflow.i32(i32 %a, i32 %b)\n\
\declare {i64, i1} @llvm.sadd.with.overflow.i64(i64 %a, i64 %b)\n\
\declare {i8, i1} @llvm.uadd.with.overflow.i8(i8 %a, i8 %b)\n\
\declare {i16, i1} @llvm.uadd.with.overflow.i16(i16 %a, i16 %b)\n\
\declare {i32, i1} @llvm.uadd.with.overflow.i32(i32 %a, i32 %b)\n\
\declare {i64, i1} @llvm.uadd.with.overflow.i64(i64 %a, i64 %b)\n\
\declare {i8, i1} @llvm.ssub.with.overflow.i8(i8 %a, i8 %b)\n\
\declare {i16, i1} @llvm.ssub.with.overflow.i16(i16 %a, i16 %b)\n\
\declare {i32, i1} @llvm.ssub.with.overflow.i32(i32 %a, i32 %b)\n\
\declare {i64, i1} @llvm.ssub.with.overflow.i64(i64 %a, i64 %b)\n\
\declare {i8, i1} @llvm.usub.with.overflow.i8(i8 %a, i8 %b)\n\
\declare {i16, i1} @llvm.usub.with.overflow.i16(i16 %a, i16 %b)\n\
\declare {i32, i1} @llvm.usub.with.overflow.i32(i32 %a, i32 %b)\n\
\declare {i64, i1} @llvm.usub.with.overflow.i64(i64 %a, i64 %b)\n\
\declare {i8, i1} @llvm.smul.with.overflow.i8(i8 %a, i8 %b)\n\
\declare {i16, i1} @llvm.smul.with.overflow.i16(i16 %a, i16 %b)\n\
\declare {i32, i1} @llvm.smul.with.overflow.i32(i32 %a, i32 %b)\n\
\declare {i64, i1} @llvm.smul.with.overflow.i64(i64 %a, i64 %b)\n\
\declare {i8, i1} @llvm.umul.with.overflow.i8(i8 %a, i8 %b)\n\
\declare {i16, i1} @llvm.umul.with.overflow.i16(i16 %a, i16 %b)\n\
\declare {i32, i1} @llvm.umul.with.overflow.i32(i32 %a, i32 %b)\n\
\declare {i64, i1} @llvm.umul.with.overflow.i64(i64 %a, i64 %b)\n"

val globalDeclarations =
"%struct.cont = type { i8* }\n\
\%struct.GC_state = type opaque\n\
\@nextFun = external hidden global %uintptr_t\n\
\@returnToC = external hidden global i32\n\
\@nextChunks = external hidden global [0 x void (%struct.cont*)*]\n\
\@gcState = external hidden global %struct.GC_state\n"

fun implementsPrim (p: 'a Prim.t): bool =
   let
      datatype z = datatype Prim.Name.t
   in
      case Prim.name p of
         CPointer_add => true
       | CPointer_diff => true
       | CPointer_equal => true
       | CPointer_fromWord => true
       | CPointer_lt => true
       | CPointer_sub => true
       | CPointer_toWord => true
       | FFI_Symbol _ => true
       | Real_Math_acos _ => false
       | Real_Math_asin _ => false
       | Real_Math_atan _ => false
       | Real_Math_atan2 _ => false
       | Real_Math_cos _ => true
       | Real_Math_exp _ => true
       | Real_Math_ln _ => true
       | Real_Math_log10 _ => true
       | Real_Math_sin _ => true
       | Real_Math_sqrt _ => true
       | Real_Math_tan _ => false
       | Real_abs _ => true (* Requires LLVM 3.2 to use "llvm.fabs" intrinsic *)
       | Real_add _ => true
       | Real_castToWord _ => true
       | Real_div _ => true
       | Real_equal _ => true
       | Real_ldexp _ => false
       | Real_le _ => true
       | Real_lt _ => true
       | Real_mul _ => true
       | Real_muladd _ => true
       | Real_mulsub _ => true
       | Real_neg _ => true
       | Real_qequal _ => true
       | Real_rndToReal _ => true
       | Real_rndToWord _ => true
       | Real_round _ => true (* Requires LLVM 3.3 to use "llvm.rint" intrinsic *)
       | Real_sub _ => true
       | Thread_returnToC => false
       | Word_add _ => true
       | Word_addCheck _ => true
       | Word_andb _ => true
       | Word_castToReal _ => true
       | Word_equal _ => true
       | Word_extdToWord _ => true
       | Word_lshift _ => true
       | Word_lt _ => true
       | Word_mul _ => true
       | Word_mulCheck (ws, _) =>
            (case (!Control.Target.arch, ws) of
                (Control.Target.X86, ws) =>
                   (* @llvm.smul.with.overflow.i64 becomes a call to __mulodi4.
                    * @llvm.umul.with.overflow.i64 becomes a call to __udivdi3.
                    * These are provided by compiler-rt and not always by libgcc.
                    * In any case, do not depend on non-standard libraries.
                    *)
                   not (WordSize.equals (ws, WordSize.word64))
              | _ => true)
       | Word_neg _ => true
       | Word_negCheck _ => true
       | Word_notb _ => true
       | Word_orb _ => true
       | Word_quot _ => true
       | Word_rem _ => true
       | Word_rndToReal _ => true
       | Word_rol _ => true
       | Word_ror _ => true
       | Word_rshift _ => true
       | Word_sub _ => true
       | Word_subCheck _ => true
       | Word_xorb _ => true
       | _ => false
   end

(* WordX.toString converts to hexadecimal, this converts to base 10 *)
fun llwordx (w: WordX.t) =
    IntInf.format (WordX.toIntInf w, StringCvt.DEC)

fun llint (i: int) =
    if i >= 0
    then Int.toString i
    else "-" ^ Int.toString (~ i)

fun llbytes b = llint (Bytes.toInt b)

fun llstring s =
    let
        fun escapeLLVM s =
            concat (List.map (String.explode s, fn c =>
                if Char.isCntrl c
                then
                    (* take the integer value of the char, convert it into a
                     * 2-digit hex string, and put a backslash in front of it
                     *)
                    let
                        val hex = IntInf.format (Int.toIntInf (Char.ord c), StringCvt.HEX)
                    in
                        if String.length hex = 1
                        then "\\0" ^ hex
                        else "\\" ^ hex
                    end
                else
                    case c of (* " and \ need to be escaped, everything else is fine *)
                        #"\"" => "\\22"
                      | #"\\" => "\\5C"
                      | _ => Char.toString c))
    in
        concat ["c\"", escapeLLVM s, "\\00\""]
    end

fun llws (ws: WordSize.t): string =
    case WordSize.prim ws of
        WordSize.W8 => "%Word8"
      | WordSize.W16 => "%Word16"
      | WordSize.W32 => "%Word32"
      | WordSize.W64 => "%Word64"

fun llwsInt (ws: WordSize.t): string =
    case WordSize.prim ws of
        WordSize.W8 => "i8"
      | WordSize.W16 => "i16"
      | WordSize.W32 => "i32"
      | WordSize.W64 => "i64"

fun llrs (rs: RealSize.t): string =
    case rs of
        RealSize.R32 => "%Real32"
      | RealSize.R64 => "%Real64"

(* Reuse CType for LLVM type *)
fun llty (ty: Type.t): string = "%" ^ CType.toString (Type.toCType ty)

fun typeOfGlobal global =
    let
        val t = Type.toCType (Global.ty global)
        val s = CType.toString t
        val number = llint (if Global.isRoot global
                            then Global.numberOfType t
                            else Global.numberOfNonRoot ())
        val array = concat ["[", number, " x %", s, "]"]
    in
        array
    end

(* Makes a two-operand instruction:
 * <lhs> = <opr> <ty> <a0>, <a1>
*)
fun mkinst (lhs, opr, ty, a0, a1) =
    concat ["\t", lhs, " = ", opr, " ", ty, " ", a0, ", ", a1, "\n"]

(* Makes a call to an LLVM math intrinsic function, given a RealSize as rs:
 * <lhs> = call type @llvm.<f>.fX(type <a0>)
*)
fun mkmath (lhs, f, rs, a0) =
    let
        val ty = llrs rs
        val fx = case rs of RealSize.R32 => "f32" | RealSize.R64 => "f64"
    in
        concat ["\t", lhs, " = call ", ty, " @llvm.", f, ".", fx, "(", ty, " ", a0, ")\n"]
    end

(* Makes a conversion instruction:
 * <lhs> = <opr> <fromty> <arg> to <toty>
*)
fun mkconv (lhs, opr, fromty, arg, toty) =
    concat ["\t", lhs, " = ", opr, " ", fromty, " ", arg, " to ", toty, "\n"]

(* Makes a getelementptr instruction:
 * <lhs> = getelementptr inbounds <ty> <arg>, [i32 <idx>]+
 * where <idcs> is a list of integer offsets
 * and <ty> must be a pointer type
 *)
fun mkgep (lhs, ty, arg, idcs) =
    let
        val indices = String.concatWith (List.map (idcs, fn (ity, i) => ity ^ " " ^ i), ", ")
    in
        concat ["\t", lhs, " = getelementptr inbounds ", ty, " ", arg, ", ", indices, "\n"]
    end

(* Makes a load instruction:
 * <lhs> = load <ty> <arg>
 * where <ty> must be a pointer type
 *)
fun mkload (lhs, ty, arg) = concat ["\t", lhs, " = load ", ty, " ", arg, "\n"]

(* Makes a store instruction:
 * store <ty> <arg>, <ty>* <loc>
 * where <ty> is the type of <arg>
 *)
fun mkstore (ty, arg, loc) = concat ["\tstore ", ty, " ", arg, ", ", ty, "* ", loc, "\n"]

val regnum = ref 0

fun getAndIncReg () =
    let
        val regval = ! regnum
        val () = regnum := regval + 1
    in
        regval
    end

fun nextLLVMReg () = concat ["%r", Int.toString (getAndIncReg ())]

fun regName (ty: CType.t, index: int): string =
    concat ["%reg", CType.name ty, "_", Int.toString index]

val cFunctions : string list ref = ref []

fun addCFunction f = if not (List.contains (!cFunctions, f, String.equals))
                     then cFunctions := List.cons (f, !cFunctions)
                     else ()

val ffiSymbols : {name: string, cty: CType.t option, symbolScope: CFunction.SymbolScope.t} list ref = ref []

fun addFfiSymbol s = if not (List.contains (!ffiSymbols, s, fn ({name=n1, ...}, {name=n2, ...}) =>
                             String.equals (n1, n2)))
                     then ffiSymbols := List.cons (s, !ffiSymbols)
                     else ()

fun offsetGCState (gcfield, ty) =
    let
        val castreg = nextLLVMReg ()
        val cast = mkconv (castreg, "bitcast", "%struct.GC_state*", "@gcState", "%Pointer")
        val ptr = nextLLVMReg ()
        val gep = mkgep (ptr, "%Pointer", castreg, [("i32", llbytes (GCField.offset gcfield))])
        val ptr2 = nextLLVMReg ()
        val cast2 = mkconv (ptr2, "bitcast", "%Pointer", ptr, ty)
    in
        (concat [cast, gep, cast2], ptr2)
    end

(* FrontierMem = Frontier *)
fun flushFrontier () =
    let
        val comment = "\t; FlushFrontier\n"
        val (pre, reg) = offsetGCState (GCField.Frontier, "%Pointer*")
        val frontier = nextLLVMReg ()
        val load = mkload (frontier, "%Pointer*", "%frontier")
        val store = mkstore ("%Pointer", frontier, reg)
    in
        concat [comment, pre, load, store]
    end

(* StackTopMem = StackTop *)
fun flushStackTop () =
    let
        val comment = "\t; FlushStackTop\n"
        val (pre, reg) = offsetGCState (GCField.StackTop, "%Pointer*")
        val stacktop = nextLLVMReg ()
        val load = mkload (stacktop, "%Pointer*", "%stackTop")
        val store = mkstore ("%Pointer", stacktop, reg)
    in
        concat [comment, pre, load, store]
    end

(* Frontier = FrontierMem *)
fun cacheFrontier () =
    let
        val comment = "\t; CacheFrontier\n"
        val (pre, reg) = offsetGCState (GCField.Frontier, "%Pointer*")
        val frontier = nextLLVMReg ()
        val load = mkload (frontier, "%Pointer*", reg)
        val store = mkstore ("%Pointer", frontier, "%frontier")
    in
        concat [comment, pre, load, store]
    end

(* StackTop = StackTopMem *)
fun cacheStackTop () =
    let
        val comment = "\t; CacheStackTop\n"
        val (pre, reg) = offsetGCState (GCField.StackTop, "%Pointer*")
        val stacktop = nextLLVMReg ()
        val load = mkload (stacktop, "%Pointer*", reg)
        val store = mkstore ("%Pointer", stacktop, "%stackTop")
    in
        concat [comment, pre, load, store]
    end

(* l_nextFun = *(uintptr_t* )(StackTop - sizeof(void* ));
   goto top;
 *)
fun callReturn () =
    let
        val comment = "\t; Return\n"
        val stacktop = nextLLVMReg ()
        val loadst = mkload (stacktop, "%Pointer*", "%stackTop")
        val ptrsize = (llbytes o Bits.toBytes o Control.Target.Size.cpointer) ()
        val ptr = nextLLVMReg ()
        val gep = mkgep (ptr, "%Pointer", stacktop, [("i32", "-" ^ ptrsize)])
        val castreg = nextLLVMReg ()
        val cast = mkconv (castreg, "bitcast", "%Pointer", ptr, "%uintptr_t*")
        val loadreg = nextLLVMReg ()
        val loadofs = mkload (loadreg, "%uintptr_t*", castreg)
        val store = mkstore ("%uintptr_t", loadreg, "%l_nextFun")
        val br = "\tbr label %top\n"
    in
        concat [comment, loadst, gep, cast, loadofs, store, br]
    end

fun stackPush amt =
    let
        val stacktop = nextLLVMReg ()
        val load = mkload (stacktop, "%Pointer*", "%stackTop")
        val ptr = nextLLVMReg ()
        val gep = mkgep (ptr, "%Pointer", stacktop, [("i32", amt)])
        val store = mkstore ("%Pointer", ptr, "%stackTop")
        val comment = concat ["\t; Push(", amt, ")\n"]
    in
        concat [comment, load, gep, store]
    end

(* argv - vector of (pre, ty, addr) triples
   i - index of argv
   returns: (pre, type, reg)
 *)
fun getArg (argv, i) =
    if Vector.length argv > i
    then Vector.sub (argv, i)
    else ("", "", "")

(* Converts an operand into its LLVM representation. Returns a triple
 (pre, ty, reg) where

 pre - A string containing preliminary statements that must be
 executed before the register can be referenced

 ty - A string containing the LLVM representation of the register's
 type when dereferenced (meaning reg is really a pointer)

 reg - The register containing a pointer to the value of the operand
 *)
fun getOperandAddr (cxt, operand) =
    case operand of
        Operand.ArrayOffset {base, index, offset, scale, ty} =>
        let
            (* arrayoffset = base + (index * scale) + offset *)
            val (basePre, baseTy, baseReg) = getOperandValue (cxt, base)
            val (indexPre, indexTy, indexReg) = getOperandValue (cxt, index)
            val scl = Scale.toString scale (* "1", "2", "4", or "8" *)
            val scaledIndex = nextLLVMReg ()
            val scaleIndex = mkinst (scaledIndex, "mul nsw", indexTy, indexReg, scl)
            val ofs = llbytes offset
            val offsettedIndex = nextLLVMReg ()
            val offsetIndex = mkinst (offsettedIndex, "add nsw", indexTy, scaledIndex, ofs)
            val llvmTy = llty ty
            val ptr = nextLLVMReg ()
            val gep = mkgep (ptr, baseTy, baseReg, [(indexTy, offsettedIndex)])
            val castedPtr = nextLLVMReg ()
            val cast = mkconv (castedPtr, "bitcast", baseTy, ptr, llvmTy ^ "*")
        in
            (concat [basePre, indexPre, scaleIndex, offsetIndex, gep, cast], llvmTy, castedPtr)
        end
      | Operand.Contents {oper, ty} =>
        let
            val (operPre, operTy, operReg) = getOperandAddr (cxt, oper)
            val llvmTy = llty ty
            val loaded = nextLLVMReg ()
            val load = mkload (loaded, operTy ^ "*", operReg)
            val reg = nextLLVMReg ()
            val cast = mkconv (reg, "bitcast", operTy, loaded, llvmTy ^ "*")
        in
            (concat [operPre, load, cast], llvmTy, reg)
        end
      | Operand.Frontier => ("", "%Pointer", "%frontier")
      | Operand.Global global =>
        let
            val globalType = Global.ty global
            val globalIsRoot = Global.isRoot global
            val globalIndex = Global.index global
            val llvmTy = llty globalType
            val ty = typeOfGlobal global
            val globalID = if globalIsRoot
                           then "@global" ^ CType.toString (Type.toCType globalType)
                           else "@globalObjptrNonRoot"
            val ptr = nextLLVMReg ()
            val gep = mkgep (ptr, ty ^ "*", globalID, [("i32", "0"), ("i32", llint globalIndex)])
        in
            (gep, llvmTy, ptr)
        end
      | Operand.Offset {base, offset, ty} =>
        let
            val (basePre, baseTy, baseReg) = getOperandValue (cxt, base)
            val idx = llbytes offset
            val llvmTy = llty ty
            val ptr = nextLLVMReg ()
            val gep = mkgep (ptr, baseTy, baseReg, [("i32", idx)])
            val reg = nextLLVMReg ()
            val cast = mkconv (reg, "bitcast", baseTy, ptr, llvmTy ^ "*")
        in
            (concat [basePre, gep, cast], llvmTy, reg)
        end
      | Operand.Register register =>
        let
            val regty = Register.ty register
            val reg = regName (Type.toCType regty, Register.index register)
            val ty = llty regty
        in
            ("", ty, reg)
        end
      | Operand.StackOffset stackOffset =>
        let
            val StackOffset.T {offset, ty} = stackOffset
            val idx = llbytes offset
            val stackTop = nextLLVMReg ()
            val load = mkload (stackTop, "%Pointer*", "%stackTop")
            val gepReg = nextLLVMReg ()
            val gep = mkgep (gepReg, "%Pointer", stackTop, [("i32", idx)])
            val llvmTy = llty ty
            val reg = nextLLVMReg ()
            val cast = mkconv (reg, "bitcast", "%Pointer", gepReg, llvmTy ^ "*")
        in
            (concat [load, gep, cast], llvmTy, reg)
        end
      | Operand.StackTop => ("", "%Pointer", "%stackTop")
      | _ => Error.bug ("Cannot get address of " ^ Operand.toString operand)

(* ty is the type of the value *)
and getOperandValue (cxt, operand) =
    let
        fun loadOperand () =
            let
                val (pre, ty, addr) = getOperandAddr (cxt, operand)
                val reg = nextLLVMReg ()
                val load = mkload (reg, ty ^ "*", addr)
            in
                (pre ^ load, ty, reg)
            end
        val Context { labelToStringIndex, ... } = cxt
    in
        case operand of
            Operand.ArrayOffset _ => loadOperand ()
          | Operand.Cast (oper, ty) =>
            let
                val (operPre, operTy, operReg) =
                   getOperandValue (cxt, oper)
                val llvmTy = llty ty
                val reg = nextLLVMReg ()
                fun isIntType cty = case cty of
                                            CType.Int8 => true
                                          | CType.Int16 => true
                                          | CType.Int32 => true
                                          | CType.Int64 => true
                                          | CType.Word8 => true
                                          | CType.Word16 => true
                                          | CType.Word32 => true
                                          | CType.Word64 => true
                                          | _ => false
                fun isPtrType cty = case cty of
                                            CType.CPointer => true
                                          | CType.Objptr => true
                                          | _ => false
                val operIsInt = (isIntType o Type.toCType o Operand.ty) oper
                val operIsPtr = (isPtrType o Type.toCType o Operand.ty) oper
                val tyIsInt = (isIntType o Type.toCType) ty
                val tyIsPtr = (isPtrType o Type.toCType) ty
                val operation = if operIsInt andalso tyIsPtr
                                then "inttoptr"
                                else if operIsPtr andalso tyIsInt
                                        then "ptrtoint"
                                        else "bitcast"
                val inst = mkconv (reg, operation, operTy, operReg, llvmTy)
            in
                (concat [operPre, inst], llvmTy, reg)
            end
          | Operand.Contents _ => loadOperand ()
          | Operand.Frontier => loadOperand ()
          | Operand.GCState =>
            let
                val reg = nextLLVMReg ()
                val cast = mkconv (reg, "bitcast", "%struct.GC_state*", "@gcState", "%Pointer")
            in
                (cast, "%Pointer", reg)
            end
          | Operand.Global _ => loadOperand ()
          | Operand.Label label =>
            let
                val reg = nextLLVMReg ()
                val cast = mkconv (reg, "inttoptr", "%Word32", labelToStringIndex label,
                                   "%CPointer")
            in
                (cast, "%CPointer", reg)
            end
          | Operand.Null => ("", "i8*", "null")
          | Operand.Offset _ => loadOperand ()
          | Operand.Real real => ("", (llrs o RealX.size) real, RealX.toString real)
          | Operand.Register  _ => loadOperand ()
          | Operand.StackOffset _ => loadOperand ()
          | Operand.StackTop => loadOperand()
          | Operand.Word word => ("", (llws o WordX.size) word, llwordx word)
    end

(* Returns (instruction, ty) pair for the given prim operation *)
fun outputPrim (prim, res, argty, arg0, arg1, arg2) =
    let
        datatype z = datatype Prim.Name.t
    in
        case Prim.name prim of
            CPointer_add =>
            let
                val tmp1 = nextLLVMReg ()
                val inst1 = mkconv (tmp1, "ptrtoint", "%Pointer", arg0, "%uintptr_t")
                val tmp2 = nextLLVMReg ()
                val inst2 = mkinst (tmp2, "add", "%uintptr_t", tmp1, arg1)
                val inst3 = mkconv (res, "inttoptr", "%uintptr_t", tmp2, "%Pointer")
            in
                (concat [inst1, inst2, inst3], "%Pointer")
            end
          | CPointer_diff =>
            let
                val tmp1 = nextLLVMReg ()
                val inst1 = mkconv (tmp1, "ptrtoint", "%Pointer", arg0, "%uintptr_t")
                val tmp2 = nextLLVMReg ()
                val inst2 = mkconv (tmp2, "ptrtoint", "%Pointer", arg1, "%uintptr_t")
                val inst3 = mkinst (res, "sub", "%uintptr_t", tmp1, tmp2)
            in
                (concat [inst1, inst2, inst3], "%uintptr_t")
            end
          | CPointer_equal =>
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, "icmp eq", "%Pointer", arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | CPointer_fromWord =>
            (mkconv (res, "inttoptr", "%uintptr_t", arg0, "%Pointer"), "%Pointer")
          | CPointer_lt =>
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, "icmp ult", "%Pointer", arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | CPointer_sub =>
            let
                val tmp1 = nextLLVMReg ()
                val inst1 = mkconv (tmp1, "ptrtoint", "%Pointer", arg0, "%uintptr_t")
                val tmp2 = nextLLVMReg ()
                val inst2 = mkinst (tmp2, "sub", "%uintptr_t", tmp1, arg1)
                val inst3 = mkconv (res, "inttoptr", "%uintptr_t", tmp2, "%Pointer")
            in
                (concat [inst1, inst2, inst3], "%Pointer")
            end
          | CPointer_toWord =>
            (mkconv (res, "ptrtoint", "%Pointer", arg0, "%uintptr_t"), "%Pointer")
          | FFI_Symbol (s as {name, cty, ...}) =>
            let
                val () = addFfiSymbol s
                val ty = case cty of
                             SOME t => "%" ^ CType.toString t
                           | NONE => Error.bug ("ffi symbol is void function?") (* TODO *)
                val inst = mkconv (res, "bitcast", ty ^ "*", "@" ^ name, "%Pointer")
            in
                (inst, "%Pointer")
            end
          | Real_Math_cos rs => (mkmath (res, "cos", rs, arg0), llrs rs)
          | Real_Math_exp rs => (mkmath (res, "exp", rs, arg0), llrs rs)
          | Real_Math_ln rs => (mkmath (res, "log", rs, arg0), llrs rs)
          | Real_Math_log10 rs => (mkmath (res, "log10", rs, arg0), llrs rs)
          | Real_Math_sin rs => (mkmath (res, "sin", rs, arg0), llrs rs)
          | Real_Math_sqrt rs => (mkmath (res, "sqrt", rs, arg0), llrs rs)
          | Real_abs rs => (mkmath (res, "fabs", rs, arg0), llrs rs)
          | Real_add rs => (mkinst (res, "fadd", llrs rs, arg0, arg1), llrs rs)
          | Real_castToWord (rs, ws) =>
            (case rs of
                 R32 => if WordSize.equals (ws, WordSize.word32)
                        then (mkconv (res, "bitcast", "float", arg0, "i32"), "i32")
                        else Error.bug "LLVM codegen: Real_castToWord"
               | R64 => if WordSize.equals (ws, WordSize.word64)
                        then (mkconv (res, "bitcast", "double", arg0, "i64"), "i64")
                        else Error.bug "LLVM codegen: Real_castToWord")
          | Real_div rs => (mkinst (res, "fdiv", llrs rs, arg0, arg1), llrs rs)
          | Real_equal rs =>
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, "fcmp oeq", llrs rs, arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | Real_le rs =>
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, "fcmp ole", llrs rs, arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | Real_lt rs =>
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, "fcmp olt", llrs rs, arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | Real_mul rs => (mkinst (res, "fmul", llrs rs, arg0, arg1), llrs rs)
          | Real_muladd rs =>
            let
                val size = case rs of
                               RealSize.R32 => "f32"
                             | RealSize.R64 => "f64"
                val llsize = llrs rs
                val inst = concat ["\t", res, " = call ", llsize, " @llvm.fma.", size, "(",
                                   llsize, " ", arg0, ", ", llsize, " ",
                                   arg1, ", ", llsize, " ", arg2, ")\n"]
            in
                (inst, llsize)
            end
          | Real_mulsub rs =>
            let
                val size = case rs of
                               RealSize.R32 => "f32"
                             | RealSize.R64 => "f64"
                val llsize = llrs rs
                val tmp1 = nextLLVMReg ()
                val inst1 = mkinst (tmp1, "fsub", llsize, "-0.0", arg2)
                val inst2 = concat ["\t", res, " = call ", llsize, " @llvm.fma.", size, "(",
                                    llsize, " ", arg0, ", ", llsize, " ",
                                    arg1, ", ", llsize, " ", tmp1, ")\n"]
            in
                (concat [inst1, inst2], llsize)
            end
          | Real_neg rs => (mkinst (res, "fsub", llrs rs, "-0.0", arg0), llrs rs)
          | Real_qequal rs =>
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, "fcmp ueq", llrs rs, arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | Real_rndToReal rs =>
            (case rs of
                 (RealSize.R64, RealSize.R32) =>
                 (mkconv (res, "fptrunc", "double", arg0, "float"), "float")
               | (RealSize.R32, RealSize.R64) =>
                 (mkconv (res, "fpext", "float", arg0, "double"), "double")
               | (RealSize.R32, RealSize.R32) => (* this is a no-op *)
                 (mkconv (res, "bitcast", "float", arg0, "float"), "float")
               | (RealSize.R64, RealSize.R64) => (* this is a no-op *)
                 (mkconv (res, "bitcast", "double", arg0, "double"), "double"))
          | Real_rndToWord (rs, ws, {signed}) =>
            let
                val opr = if signed then "fptosi" else "fptoui"
            in
                (mkconv (res, opr, llrs rs, arg0, llws ws), llws ws)
            end
          | Real_round rs => (mkmath (res, "rint", rs, arg0), llrs rs)
          | Real_sub rs => (mkinst (res, "fsub", llrs rs, arg0, arg1), llrs rs)
          | Thread_returnToC =>
            let
                val store = mkstore ("i32", "1", "@returnToC")
                val ret = "\tret %struct.cont %cont\n"
            in
                (concat [store, ret], "")
            end
          | Word_add ws => (mkinst (res, "add", llws ws, arg0, arg1), llws ws)
          | Word_addCheck (ws, {signed}) =>
            let
                val opr = if signed then "sadd" else "uadd"
                val ty = llws ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.", opr,
                                   ".with.overflow.", llwsInt ws, "(", ty, " ", arg0,
                                   ", ", ty, " ", arg1, ")\n"]
            in
                (inst, concat ["{", ty, ", i1}"])
            end
          | Word_andb ws => (mkinst (res, "and", llws ws, arg0, arg1), llws ws)
          | Word_castToReal (ws, rs) =>
            (case rs of
                 R32 => if WordSize.equals (ws, WordSize.word32)
                        then (mkconv (res, "bitcast", "i32", arg0, "float"), "float")
                        else Error.bug "LLVM codegen: Word_castToReal"
               | R64 => if WordSize.equals (ws, WordSize.word64)
                        then (mkconv (res, "bitcast", "i64", arg0, "double"), "double")
                        else Error.bug "LLVM codegen: Word_castToReal")
          | Word_equal _ =>
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, "icmp eq", argty, arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | Word_extdToWord (ws1, ws2, {signed}) =>
            let
                val opr = case WordSize.compare (ws1, ws2) of
                              LESS => if signed then "sext" else "zext"
                            | EQUAL => Error.bug "LLVM codegen: Word_extdToWord"
                            | GREATER => "trunc"
            in
                (mkconv (res, opr, llws ws1, arg0, llws ws2), llws ws2)
            end
          | Word_lshift ws => (mkinst (res, "shl", llws ws, arg0, arg1), llws ws)
          | Word_lt (ws, {signed}) =>
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, if signed then "icmp slt" else "icmp ult",
                                  llws ws, arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | Word_mul (ws, _) => (mkinst (res, "mul", llws ws, arg0, arg1), llws ws)
          | Word_mulCheck (ws, {signed}) =>
            let
                val opr = if signed then "smul" else "umul"
                val ty = llws ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.", opr,
                                   ".with.overflow.", llwsInt ws, "(", ty, " ", arg0,
                                   ", ", ty, " ", arg1, ")\n"]
            in
                (inst, concat ["{", ty, ", i1}"])
            end
          | Word_neg ws => (mkinst (res, "sub", llws ws, "0", arg0), llws ws)
          | Word_negCheck ws =>
            let
                val ty = llws ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.ssub.with.overflow.",
                                   llwsInt ws, "(", ty, " 0, ", ty, " ", arg0, ")\n"]
                val resTy = concat ["{", ty, ", i1}"]
            in
                (inst, resTy)
            end
          | Word_notb ws => (mkinst (res, "xor", llws ws, arg0, "-1"), llws ws)
          | Word_orb ws => (mkinst (res, "or", llws ws, arg0, arg1), llws ws)
          | Word_quot (ws, {signed}) =>
            (mkinst (res, if signed then "sdiv" else "udiv", llws ws, arg0, arg1), llws ws)
          | Word_rem (ws, {signed}) =>
            (mkinst (res, if signed then "srem" else "urem", llws ws, arg0, arg1), llws ws)
          | Word_rndToReal (ws, rs, {signed}) =>
            let
                val opr = if signed then "sitofp" else "uitofp"
            in
                (mkconv (res, opr, llws ws, arg0, llrs rs), llrs rs)
            end
          | Word_rol ws =>
            let
                (* (arg0 >> (size - arg1)) | (arg0 << arg1) *)
                val ty = llws ws
                val tmp1 = nextLLVMReg ()
                val inst1 = mkinst (tmp1, "sub", ty, WordSize.toString ws, arg1)
                val tmp2 = nextLLVMReg ()
                val inst2 = mkinst (tmp2, "lshr", ty, arg0, tmp1)
                val tmp3 = nextLLVMReg ()
                val inst3 = mkinst (tmp3, "shl", ty, arg0, arg1)
                val inst4 = mkinst (res, "or", ty, tmp2, tmp3)
            in
                (concat [inst1, inst2, inst3, inst4], llws ws)
            end
          | Word_ror ws =>
            let
                (* (arg0 >> arg1) | (arg0 << (size - arg1)) *)
                val ty = llws ws
                val tmp1 = nextLLVMReg ()
                val inst1 = mkinst (tmp1, "lshr", ty, arg0, arg1)
                val tmp2 = nextLLVMReg ()
                val inst2 = mkinst (tmp2, "sub", ty, WordSize.toString ws, arg1)
                val tmp3 = nextLLVMReg ()
                val inst3 = mkinst (tmp3, "shl", ty, arg0, tmp2)
                val inst4 = mkinst (res, "or", ty, tmp1, tmp3)
            in
                (concat [inst1, inst2, inst3, inst4], llws ws)
            end
          | Word_rshift (ws, {signed}) =>
            let
                val opr = if signed then "ashr" else "lshr"
            in
                (mkinst (res, opr, llws ws, arg0, arg1), llws ws)
            end
          | Word_sub ws => (mkinst (res, "sub", llws ws, arg0, arg1), llws ws)
          | Word_subCheck (ws, {signed}) =>
            let
                val opr = if signed then "ssub" else "usub"
                val ty = llws ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.", opr,
                                   ".with.overflow.", llwsInt ws, "(", ty, " ", arg0,
                                   ", ", ty, " ", arg1, ")\n"]
            in
                (inst, concat ["{", ty, ", i1}"])
            end
          | Word_xorb ws => (mkinst (res, "xor", llws ws, arg0, arg1), llws ws)
          | _ => Error.bug "LLVM Codegen: Unsupported operation in outputPrim"
    end

fun outputPrimApp (cxt, p) =
    let
        datatype z = datatype Prim.Name.t
        val {args, dst, prim} = p
        fun typeOfArg0 () = (WordSize.fromBits o Type.width o Operand.ty o Vector.sub) (args, 0)
        val castArg1 = case Prim.name prim of
                           Word_rshift _ => SOME (typeOfArg0 ())
                         | Word_lshift _ => SOME (typeOfArg0 ())
                         | Word_rol _ => SOME (typeOfArg0 ())
                         | Word_ror _ => SOME (typeOfArg0 ())
                         | _ => NONE
        val operands = Vector.map (args, fn opr => getOperandValue (cxt, opr))
        val (arg0pre, arg0ty, arg0reg) = getArg (operands, 0)
        val (arg1pre, _, arg1) = getArg (operands, 1)
        val (cast, arg1reg) = case castArg1 of
                                  SOME ty =>
                                  let
                                      val reg = nextLLVMReg ()
                                      val opr = case WordSize.prim ty of
                                                    WordSize.W8 => "trunc"
                                                  | WordSize.W16 => "trunc"
                                                  | WordSize.W32 => "bitcast"
                                                  | WordSize.W64 => "zext"
                                      val inst = mkconv (reg, opr, "%Word32", arg1, llws ty)
                                  in
                                      (inst, reg)
                                  end
                                | NONE => ("", arg1)
        val (arg2pre, _, arg2reg) = getArg (operands, 2)
        val reg = nextLLVMReg ()
        val (inst, _) = outputPrim (prim, reg, arg0ty, arg0reg, arg1reg, arg2reg)
        val storeDest =
            case dst of
                NONE => ""
              | SOME dest =>
                let
                    val (destPre, destTy, destReg) = getOperandAddr (cxt, dest)
                    val store = mkstore (destTy, reg, destReg)
                in
                    concat [destPre, store]
                end
    in
        concat [arg0pre, arg1pre, cast, arg2pre, inst, storeDest]
    end

fun outputStatement (cxt: Context, stmt: Statement.t): string =
    let
        val comment = concat ["\t; ", Layout.toString (Statement.layout stmt), "\n"]
        val Context { printstmt, printmove, ...} = cxt
        val printcode = if printstmt
                        then "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @stmt, i32 0, i32 0))\n"
                        else ""
        val stmtcode =
            case stmt of
                Statement.Move {dst, src} =>
                let
                    val (srcpre, _, srcreg) = getOperandValue (cxt, src)
                    val (dstpre, dstty, dstreg) = getOperandAddr (cxt, dst)
                    val store = mkstore (dstty, srcreg, dstreg)
                    val gotlhs = if printmove
                                 then "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([9 x i8]* @gotlhs, i32 0, i32 0))\n"
                                 else ""
                    val gotrhs = if printmove
                                 then "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([9 x i8]* @gotrhs, i32 0, i32 0))\n"
                                 else ""
                in
                    concat [srcpre, gotrhs, dstpre, gotlhs, store]
                end
              | Statement.Noop => "\t; Noop\n"
              | Statement.PrimApp p => outputPrimApp (cxt, p)
              | Statement.ProfileLabel _ => "\t; ProfileLabel\n"
    in
        concat [comment, printcode, stmtcode]
    end

fun outputTransfer (cxt, transfer, sourceLabel) =
    let
        val comment = concat ["\t; ", Layout.toString (Transfer.layout transfer), "\n"]
        val Context { labelToStringIndex = labelToStringIndex,
                      chunkLabelToString = chunkLabelToString,
                      labelChunk = labelChunk,
                      printstmt = printstmt, ... } = cxt
        fun transferPush (return, size) =
            let
                val offset = llbytes (Bytes.- (size, Runtime.labelSize ()))
                val frameIndex = labelToStringIndex return
                val stackTop = nextLLVMReg ()
                val load = mkload (stackTop, "%Pointer*", "%stackTop")
                val gepReg = nextLLVMReg ()
                val gep = mkgep (gepReg, "%Pointer", stackTop, [("i32", offset)])
                val castreg = nextLLVMReg ()
                val cast = mkconv (castreg, "bitcast", "%Pointer", gepReg, "%uintptr_t*")
                val storeIndex = mkstore ("%uintptr_t", frameIndex, castreg)
                val pushcode = stackPush (llbytes size)
            in
                concat [load, gep, cast, storeIndex, pushcode]
            end
    in
        case transfer of
            Transfer.Arith {args, dst, overflow, prim, success} =>
            let
                val overflowstr = Label.toString overflow
                val successstr = Label.toString success
                val operands = Vector.map (args, fn opr => getOperandValue (cxt, opr))
                val (arg0pre, arg0ty, arg0reg) = getArg (operands, 0)
                val (arg1pre, _, arg1reg) = getArg (operands, 1)
                val (arg2pre, _, arg2reg) = getArg (operands, 2)
                val reg = nextLLVMReg ()
                val (inst, ty) = outputPrim (prim, reg, arg0ty, arg0reg, arg1reg, arg2reg)
                val res = nextLLVMReg ()
                val extractRes = concat ["\t", res, " = extractvalue ", ty, " ", reg, ", 0\n"]
                val obit = nextLLVMReg ()
                val extractObit = concat ["\t", obit, " = extractvalue ", ty, " ", reg, ", 1\n"]
                val (destPre, destTy, destReg) = getOperandAddr (cxt, dst)
                val store = mkstore (destTy, res, destReg)
                val br = concat ["\tbr i1 ", obit, ", label %",
                                 overflowstr, ", label %", successstr, "\n"]
            in
                concat [comment, arg0pre, arg1pre, arg2pre, inst,
                        extractRes, extractObit, destPre, store, br]
            end
          | Transfer.CCall {args, frameInfo, func, return} =>
            let
                val CFunction.T {return = returnTy,
                                 target, ...} = func
                val (paramPres, paramTypes, paramRegs) =
                    Vector.unzip3 (Vector.map (args, fn opr => getOperandValue (cxt, opr)))
                val push =
                    case frameInfo of
                        NONE => ""
                      | SOME fi =>
                        let
                            val Context { program = program, ... } = cxt
                            val size = Program.frameSize (program, fi)
                        in
                            transferPush (valOf return, size)
                        end
                val flushFrontierCode = if CFunction.modifiesFrontier func then flushFrontier () else ""
                val flushStackTopCode = if CFunction.readsStackTop func then flushStackTop () else ""
                val resultReg = if Type.isUnit returnTy then "" else nextLLVMReg ()
                val call = case target of
                               CFunction.Target.Direct name =>
                               let
                                   val (lhs, ty) = if Type.isUnit returnTy
                                                   then ("\t", "void")
                                                   else (concat ["\t", resultReg, " = "],
                                                         llty returnTy)
                                   val llparams = String.concatWith
                                                      (Vector.toListMap
                                                           (Vector.zip (paramTypes, paramRegs),
                                                            fn (t, p) => t ^ " " ^ p),
                                                       ", ")
                                   val cfunc = concat [ty, " @", name, "(",
                                                       String.concatWith
                                                           ((Vector.toList paramTypes), ", "),
                                                       ")"]
                                   val () = addCFunction cfunc
                               in
                                   concat [lhs, "call ", ty, " @", name, "(", llparams, ")\n"]
                               end
                             | CFunction.Target.Indirect => (* TODO *) ""
                val epilogue = case return of
                                   NONE => "\tunreachable\n"
                                 | SOME l =>
                                   let
                                       val storeResult = if Type.isUnit returnTy
                                                         then ""
                                                         else mkstore (llty returnTy, resultReg,
                                                                       "@CReturn" ^ CType.name (Type.toCType returnTy))
                                       val cacheFrontierCode = if CFunction.modifiesFrontier func
                                                               then cacheFrontier ()
                                                               else ""
                                       val cacheStackTopCode = if CFunction.writesStackTop func
                                                               then cacheStackTop ()
                                                               else ""
                                       val br = if CFunction.maySwitchThreads func
                                                then callReturn ()
                                                else concat ["\tbr label %", Label.toString l,
                                                             "\n"]
                                   in
                                       concat [storeResult, cacheFrontierCode, cacheStackTopCode,
                                               br]
                                   end
                val fcall = if printstmt
                            then "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @fcall, i32 0, i32 0))\n"
                            else ""
            in
                concat [comment,
                        "\t; GetOperands\n",
                        String.concatV paramPres,
                        push,
                        flushFrontierCode,
                        flushStackTopCode,
                        "\t; Call\n",
                        fcall,
                        call,
                        epilogue]
            end
          | Transfer.Call {label, return, ...} =>
            let
                val labelstr = Label.toString label
                val dstChunk = labelChunk label
                val push = case return of
                               NONE => ""
                             | SOME {return, size, ...} => transferPush (return, size)
                val goto = if ChunkLabel.equals (labelChunk sourceLabel, dstChunk)
                           then concat ["\tbr label %", labelstr, "\n"]
                           else let
                               val comment = "\t; FarJump\n"
                               (* cont.nextChunk = ChunkN *)
                               val funcname = "@Chunk" ^ chunkLabelToString dstChunk
                               val func = nextLLVMReg ()
                               val cast = mkconv (func, "bitcast", "%struct.cont ()*", funcname,
                                                  "i8*")
                               val nextchunkptr = nextLLVMReg ()
                               val gep = mkgep (nextchunkptr, "%struct.cont*", "%cont", [("i32", "0"), ("i32", "0")])
                               val storeNCP = mkstore ("i8*", func, nextchunkptr)
                               val () = addCFunction (concat ["%struct.cont ", funcname, "()"])
                               (* nextFun = l *)
                               val storeNF = mkstore ("%uintptr_t", labelToStringIndex label,
                                                      "@nextFun")
                               val br = "\tbr label %leaveChunk\n"
                           in
                               concat [comment, cast, gep, storeNCP, storeNF, br]
                           end
            in
                concat [push, goto]
            end
          | Transfer.Goto label =>
            let
                val labelString = Label.toString label
                val goto = concat ["\tbr label %", labelString, "\n"]
            in
                concat [comment, goto]
            end
          | Transfer.Raise =>
            let
                (* StackTop = StackBottom + ExnStack *)
                val (sbpre, sbreg) = offsetGCState (GCField.StackBottom, "%Pointer*")
                val stackBottom = nextLLVMReg ()
                val loadStackBottom = mkload (stackBottom, "%Pointer*", sbreg)
                val (espre, esreg) = offsetGCState (GCField.ExnStack, "i32*")
                val exnStack = nextLLVMReg ()
                val loadExnStack = mkload (exnStack, "i32*", esreg)
                val sum = nextLLVMReg ()
                val gep = mkgep (sum, "%Pointer", stackBottom, [("i32", exnStack)])
                val store = mkstore ("%Pointer", sum, "%stackTop")
                (* l_nextFun = *(uintptr_t* )(StackTop - sizeof(void* )); *)
                val stackTop = nextLLVMReg ()
                val loadStackTop = mkload (stackTop, "%Pointer*", "%stackTop")
                val sizeofptr = (Bytes.toString o Bits.toBytes o Control.Target.Size.cpointer) ()
                val offsetST = nextLLVMReg ()
                val subPtrSize = mkgep (offsetST, "%Pointer", stackTop, [("i32", "-" ^ sizeofptr)])
                val offsetIntPtr = nextLLVMReg ()
                val toint = mkconv (offsetIntPtr, "bitcast", "%Pointer", offsetST,
                                    "%uintptr_t*")
                val offsetInt = nextLLVMReg ()
                val loadOffset = mkload (offsetInt, "%uintptr_t*", offsetIntPtr)
                val storeLNF = mkstore ("%uintptr_t", offsetInt, "%l_nextFun")
                (* goto top *)
                val gotoTop = "\tbr label %top\n"
            in
                concat [comment, sbpre, loadStackBottom, espre, loadExnStack, gep, store,
                        loadStackTop, subPtrSize, toint, loadOffset, storeLNF, gotoTop]
            end
          | Transfer.Return => callReturn ()
          | Transfer.Switch switch =>
            let
                val Switch.T {cases, default, test, ...} = switch
                val (testpre, testty, testreg) = getOperandValue (cxt, test)
                fun branch (ifTrue, ifFalse) =
                    let
                        val testi1 = nextLLVMReg ()
                        val trunc = mkconv (testi1, "trunc", testty, testreg, "i1")
                        val br = concat ["\tbr i1 ", testi1,
                                         ", label %", Label.toString ifTrue,
                                         ", label %", Label.toString ifFalse, "\n"]
                    in
                        concat [comment, testpre, trunc, br]
                    end
                fun switch () =
                    let
                        val (switchCases, switchDefault) =
                            case default of
                                SOME d => (cases, "%" ^ Label.toString d)
                              | NONE => (Vector.dropPrefix (cases, 1),
                                         "%" ^ Label.toString (#2 (Vector.sub (cases, 0))))
                        val branches = String.concatV (Vector.map (switchCases, fn (w, l) =>
                                           concat ["\t\t", llws (WordX.size w), " ", llwordx w,
                                                   ", label %", Label.toString l, "\n"]))
                        val switch = concat ["\tswitch ", testty, " ", testreg,
                                             ", label ", switchDefault, " [\n", branches, "\t]\n"]
                    in
                        concat [comment, testpre, switch]
                    end
            in
                if Vector.length cases = 2 andalso Option.isNone default
                then
                    let
                        val (c0, l0) = Vector.sub (cases, 0)
                        val (c1, l1) = Vector.sub (cases, 1)
                    in
                        if WordX.isZero c0 andalso WordX.isOne c1
                        then branch (l1, l0)
                        else if WordX.isZero c1 andalso WordX.isZero c0
                             then branch (l0, l1)
                             else switch ()
                    end
                else switch ()
            end
    end

fun outputBlock (cxt, block) =
    let
        val Context { program = program, printblock = printblock, ... } = cxt
        val Block.T {kind, label, statements, transfer, ...} = block
        val labelstr = Label.toString label
        val labelstrLen = Int.toString (String.size labelstr + 1)
        val blockLabel = labelstr ^ ":\n"
        val printBlock = if printblock
            then concat ["\tcall i32 (i8*, ...)* @printf(",
            "i8* getelementptr inbounds ([19 x i8]* @enteringBlock, i32 0, i32 0), ",
            "i8* getelementptr inbounds ([", labelstrLen, " x i8]* @labelstr_", labelstr,
                 ", i32 0, i32 0))\n"]
            else ""
        fun pop fi = (stackPush o llbytes o Bytes.~ o Program.frameSize) (program, fi)
        val dopop = case kind of
                        Kind.Cont {frameInfo, ...} => pop frameInfo
                      | Kind.CReturn {dst, frameInfo, ...} =>
                        let
                            val popfi = case frameInfo of
                                            NONE => ""
                                          | SOME fi => pop fi
                            val move = case dst of
                                           NONE => ""
                                         | SOME x =>
                                           let
                                               val xop = Live.toOperand x
                                               val ty = Operand.ty xop
                                               val llvmTy = llty ty
                                               val reg = nextLLVMReg ()
                                               val load = mkload (reg, llvmTy ^ "*",
                                                                  "@CReturn" ^
                                                                  CType.name (Type.toCType ty))
                                               val (dstpre, dstty, dstreg) =
                                                   getOperandAddr (cxt, xop)
                                               val store = mkstore (dstty, reg, dstreg)
                                           in
                                               concat [dstpre, load, store]
                                           end
                        in
                            concat [popfi, move]
                        end
                      | Kind.Handler {frameInfo, ...} => pop frameInfo
                      | _ => ""
        val outputStatementWithCxt = fn s => outputStatement (cxt, s)
        val blockBody = String.concatV (Vector.map (statements, outputStatementWithCxt))
        val blockTransfer = outputTransfer (cxt, transfer, label)
    in
        concat [blockLabel, printBlock, dopop, blockBody, blockTransfer, "\n"]
    end

fun outputLLVMDeclarations (cxt, print, chunk) =
    let
        val Context { printblock = printblock, printstmt = printstmt, printmove = printmove,
                      chunkLabelIndex = chunkLabelIndex, ...} = cxt
        val Chunk.T { chunkLabel, ... } = chunk
        val globals = concat (List.map (CType.all, fn t =>
                          let
                              val s = CType.toString t
                          in
                              concat ["@global", s, " = external hidden global [",
                                      llint (Global.numberOfType t),
                                      " x %", s, "]\n@CReturn", CType.name t,
                                      " = external hidden global %", s, "\n"]
                          end))
        val nonroot = concat ["@globalObjptrNonRoot = external hidden global [",
                              llint (Global.numberOfNonRoot ()),
                              " x %Pointer]\n"]
        val printBlockStrings = if chunkLabelIndex chunkLabel = 0
                                then
"declare i32 @printf(i8*, ...)\n\
\@enteringChunk = global [16 x i8] c\"Entering chunk\\0A\\00\"\n\
\@enteringBlock = global [19 x i8] c\"Entering block %s\\0A\\00\"\n"
                                else
"declare i32 @printf(i8*, ...)\n\
\@enteringChunk = external hidden global [16 x i8]\n\
\@enteringBlock = external hidden global [19 x i8]\n"
        val printStmtStrings = if chunkLabelIndex chunkLabel = 0
                               then
"@fcall = global [15 x i8] c\"Function call\\0A\\00\"\n\
\@stmt = global [11 x i8] c\"statement\\0A\\00\"\n"
                               else
"@fcall = external hidden global [15 x i8]\n\
\@stmt = external hidden global [11 x i8]\n"
        val printMoveStrings = if chunkLabelIndex chunkLabel = 0
                               then
"@gotlhs = global [9 x i8] c\"got lhs\\0A\\00\"\n\
\@gotrhs = global [9 x i8] c\"got rhs\\0A\\00\"\n"
                               else
"gotlhs = external hidden global [9 x i8]\n\
\gotrhs = external hidden global [9 x i8]\n"
        val labelStrings = if printblock
                           then
                               let
                                   val Chunk.T { blocks = blocks, ... } = chunk
                               in
                                   String.concatV (Vector.map (blocks, fn b =>
                                       let
                                           val Block.T { label = label, ... } = b
                                           val labelstr = Label.toString label
                                           val len = Int.toString (String.size labelstr + 1)
                                       in
                                           concat ["@labelstr_", labelstr, " = global [", len,
                                                   " x i8] ", llstring labelstr, "\n"]
                                       end))
                               end
                           else ""
    in
        print (concat [llvmIntrinsics, "\n", mltypes, "\n", ctypes (),
                       "\n", globals, nonroot, "\n", globalDeclarations, "\n",
                       if printblock then printBlockStrings else "",
                       if printstmt then printStmtStrings else "",
                       if printmove then printMoveStrings else "",
                       labelStrings, "\n"])
    end

fun outputChunk (cxt, outputLL, chunk) =
    let
        val () = cFunctions := []
        val () = ffiSymbols := []
        val Context { labelToStringIndex, chunkLabelIndex, labelChunk,
                      chunkLabelToString, entryLabels, printblock, ... } = cxt
        val Chunk.T {blocks, chunkLabel, regMax} = chunk
        val { done, print, file=_ } = outputLL ()
        val () = outputLLVMDeclarations (cxt, print, chunk)
        val () = print (concat ["define hidden %struct.cont @",
                                "Chunk" ^ chunkLabelToString chunkLabel,
                                "() {\nentry:\n"])
        val () = if printblock
                 then print "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([16 x i8]* @enteringChunk, i32 0, i32 0))\n"
                 else ()
        val () = (print "\t%cont = alloca %struct.cont\n"
                 ; print "\t%frontier = alloca %Pointer\n"
                 ; print "\t%l_nextFun = alloca %uintptr_t\n")
        val tmp1 = nextLLVMReg ()
        val () = (print (mkload (tmp1, "%uintptr_t*", "@nextFun"))
                 ; print (mkstore ("%uintptr_t", tmp1, "%l_nextFun"))
                 ; print "\t%stackTop = alloca %Pointer\n"
                 ; print (cacheFrontier ())
                 ; print (cacheStackTop ()))
        val () = List.foreach (CType.all,
                               fn t =>
                                  let
                                      val pre = concat ["\t%reg", CType.name t, "_"]
                                      val post = concat [" = alloca %", CType.toString t, "\n"]
                                  in
                                      Int.for (0, 1 + regMax t,
                                               fn i => print (concat [pre, llint i, post]))
                                  end)
        val () = print "\tbr label %top\ntop:\n"
        val tmp2 = nextLLVMReg ()
        val () = print (mkload (tmp2, "%uintptr_t*", "%l_nextFun"))
        val entryLabelsInChunk = Vector.keepAll (entryLabels,
                                                 fn l => chunkLabelIndex chunkLabel =
                                                         chunkLabelIndex (labelChunk l))
        val branches = String.concatV (Vector.map (entryLabelsInChunk, fn label =>
                           let
                               val labelName = Label.toString label
                               val i = labelToStringIndex label
                           in
                               concat ["\t\t%uintptr_t ", i, ", label %", labelName, "\n"]
                           end))
        val () = (print (concat ["\tswitch %uintptr_t ", tmp2,
                                 ", label %default [\n", branches, "\t]\n"])
                 ; print (String.concatV (Vector.map (blocks, fn b => outputBlock (cxt, b))))
                 ; print "default:\n")
        val nextFun = nextLLVMReg ()
        val () = (print (mkload (nextFun, "%uintptr_t*", "%l_nextFun"))
                 ; print (mkstore ("%uintptr_t", nextFun, "@nextFun")))
        val nextChunks_nextFun_ptr = nextLLVMReg ()
        val () = print (mkgep (nextChunks_nextFun_ptr,
                              "[0 x void (%struct.cont*)*]*", "@nextChunks", [("i32", "0"), ("%uintptr_t", nextFun)]))
        val nextChunks_nextFun = nextLLVMReg ()
        val () = print (mkload (nextChunks_nextFun, "void (%struct.cont*)**",
                                nextChunks_nextFun_ptr))
        val nextChunks_nextFun_bc = nextLLVMReg ()
        val () = print (mkconv (nextChunks_nextFun_bc, "bitcast", "void (%struct.cont*)*",
                                nextChunks_nextFun, "i8*"))
        val cont_nextChunk_ptr = nextLLVMReg ()
        val () = (print (mkgep (cont_nextChunk_ptr, "%struct.cont*", "%cont", [("i32", "0"), ("i32", "0")]))
                 ; print (mkstore ("i8*", nextChunks_nextFun_bc, cont_nextChunk_ptr))
                 ; print "\tbr label %leaveChunk\n"
                 ; print "leaveChunk:\n"
                 ; print (flushFrontier ())
                 ; print (flushStackTop ()))
        val leaveRet = nextLLVMReg ()
        val () = (print (mkload (leaveRet, "%struct.cont*", "%cont"))
                 ; print (concat ["\tret %struct.cont ", leaveRet, "\n"])
                 ; print "}\n\n")
        val () = List.foreach (!cFunctions, fn f =>
                     print (concat ["declare ", f, "\n"]))
        val () = List.foreach (!ffiSymbols, fn {name, cty, symbolScope} =>
                    let
                        val ty = case cty of
                                        SOME t => "%" ^ CType.toString t
                                      | NONE => "void"
                        val visibility = case symbolScope of
                                             CFunction.SymbolScope.External => "default"
                                           | CFunction.SymbolScope.Private => "hidden"
                                           | CFunction.SymbolScope.Public => "default"
                    in
                        print (concat ["@", name, " = external ", visibility, " global ", ty,
                                       "\n"])
                    end)

    in
        done ()
    end

fun makeContext program =
    let
        val Program.T { chunks, frameLayouts, ...} = program
        val {get = labelInfo: Label.t -> {block: Block.t,
                                          chunkLabel: ChunkLabel.t,
                                          frameIndex: int option,
                                          layedOut: bool ref},
             set = setLabelInfo, ...} =
            Property.getSetOnce

                (Label.plist, Property.initRaise ("LLVMCodeGen.info", Label.layout))
        val entryLabels: (Label.t * int) list ref = ref []
        val indexCounter = Counter.new (Vector.length frameLayouts)
        val _ =
         List.foreach
         (chunks, fn Chunk.T {blocks, chunkLabel, ...} =>
          Vector.foreach
          (blocks, fn b as Block.T {kind, label, ...} =>
           let
              fun entry (index: int) =
                 List.push (entryLabels, (label, index))
              fun kindIsEntry kind =
                  case kind of
                      Kind.Cont _ => true
                    | Kind.CReturn {func, ...} => CFunction.mayGC func
                    | Kind.Func => true
                    | Kind.Handler _ => true
                    | _ => false
              val frameIndex =
                 case Kind.frameInfoOpt kind of
                    NONE => (if kindIsEntry kind
                                then entry (Counter.next indexCounter)
                             else ()
                             ; NONE)
                  | SOME (FrameInfo.T {frameLayoutsIndex, ...}) =>
                    (entry frameLayoutsIndex
                    ; SOME frameLayoutsIndex)
           in
              setLabelInfo (label, {block = b,
                                    chunkLabel = chunkLabel,
                                    frameIndex = frameIndex,
                                    layedOut = ref false})
           end))
        val a = Array.fromList (!entryLabels)
        val () = QuickSort.sortArray (a, fn ((_, i), (_, i')) => i <= i')
        val entryLabels = Vector.map (Vector.fromArray a, #1)
        val labelChunk = #chunkLabel o labelInfo
        val {get = chunkLabelIndex: ChunkLabel.t -> int, ...} =
            Property.getSet (ChunkLabel.plist,
                             Property.initFun (let
                                                  val c = Counter.new 0
                                              in
                                                  fn _ => Counter.next c
                                              end))
        val chunkLabelToString = llint o chunkLabelIndex
        val {get = labelIndex, set = setLabelIndex, ...} =
            Property.getSetOnce (Label.plist,
                                 Property.initRaise ("index", Label.layout))
        val _ =
            Vector.foreachi (entryLabels, fn (i, l) => setLabelIndex (l, i))
        (* NB: This should always return the same value as
         * (Int.toString o valOf o #frameIndex o labelInfo) l
         *)
        fun labelToStringIndex (l: Label.t): string = llint (labelIndex l)
    in
        Context { program = program,
                  labelToStringIndex = labelToStringIndex,
                  chunkLabelIndex = chunkLabelIndex,
                  chunkLabelToString = chunkLabelToString,
                  labelChunk = labelChunk,
                  entryLabels = entryLabels,
                  labelInfo = labelInfo,
                  printblock = !Control.Native.commented > 0,
                  printstmt = !Control.Native.commented > 1,
                  printmove = !Control.Native.commented > 2
                }
    end

fun transLLVM (cxt, outputLL) =
    let
        val Context { program, ... } = cxt
        val Program.T { chunks, ...} = program
        val () = List.foreach (chunks, fn chunk => outputChunk (cxt, outputLL, chunk))
    in
        ()
    end

fun transC (cxt, outputC) =
    let
        val Context { program, ... } = cxt
        val {print, done, file=_} = outputC ()
        val Program.T {main = main, chunks = chunks, ... } = program
        val Context { chunkLabelToString, labelToStringIndex, entryLabels, labelInfo, ... } = cxt
        val chunkLabel = chunkLabelToString (#chunkLabel main)
        val mainLabel = labelToStringIndex (#label main)
        val additionalMainArgs = [chunkLabel, mainLabel]
        fun callNoSemi (f: string, xs: string list, print: string -> unit): unit
            = (print f
              ; print " ("
              ; (case xs
                  of [] => ()
                   | x :: xs => (print x
                                ; List.foreach (xs,
                                                fn x => (print ", "; print x))))
              ; print ")")
        fun ccall (f, xs, print) =
            (callNoSemi (f, xs, print)
            ; print ";\n")
        fun declareChunk (Chunk.T {chunkLabel, ...}, print) =
            ccall ("DeclareChunk",
                 [chunkLabelToString chunkLabel],
                 print)
        fun rest () =
            (List.foreach (chunks, fn c => declareChunk (c, print))
            ; print "PRIVATE struct cont ( *nextChunks []) () = {\n"
            ; Vector.foreach (entryLabels, fn l =>
                             let
                                 val {chunkLabel, ...} = labelInfo l
                             in
                                 print "\t"
                               ; callNoSemi ("Chunkp",
                                             [chunkLabelToString chunkLabel],
                                             print)
                               ; print ",\n"
                             end)
            ; print "};\n")
    in
        CCodegen.outputDeclarations
            {additionalMainArgs = additionalMainArgs,
             includes = ["c-main.h"],
             print = print,
             program = program,
             rest = rest}
      ; done ()
    end

fun output {program, outputC, outputLL} =
    let
        val context = makeContext program
        val () = transC (context, outputC)
        val () = transLLVM (context, outputLL)
    in
        ()
    end

end

