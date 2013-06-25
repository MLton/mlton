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

fun ctypes () =
    let
        val inttype = case !Control.defaultWord of
                          "word8" => "i8"
                        | "word16" => "i16"
                        | "word32" => "i32"
                        | "word64" => "i64"
                        | _ => Error.bug "LLVMCodegen.ctypes"
    in
        concat ["%uintptr_t = type ", inttype, "\n"]
    end

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
\declare {i64, i1} @llvm.umul.with.overflow.i64(i64 %a, i64 %b)\n\
\declare float @llvm.fmuladd.f32(float %a, float %b, float %c) ; requires LLVM 3.2\n\
\declare double @llvm.fmuladd.f64(double %a, double %b, double %c) ; requires LLVM 3.2\n\
\declare i32 @printf(i8*, ...)\n"

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

(* WordX.toString converts to hexadecimal, this converts to base 10 *)
fun llwordx (w: WordX.t) =
    IntInf.format (WordX.toIntInf w, StringCvt.DEC)

fun llint (i: int) =
    if i >= 0
    then Int.toString i
    else "-" ^ Int.toString (~ i)

fun llbytes b = llint (Bytes.toInt b)

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
           
fun llstring s = concat ["c\"", escapeLLVM s, "\\00\""]

fun globalDeclarations cxt =
    let
        val Context { printblock, printstmt, printmove, ... } = cxt
    in
        concat [
"%struct.cont = type { i8* }\n\
\%struct.GC_state = type opaque\n\
\@nextFun = external hidden global %uintptr_t\n\
\@returnToC = external hidden global i32\n\
\@nextChunks = external hidden global [0 x void (%struct.cont*)*]\n\
\@gcState = external hidden global %struct.GC_state\n\
\@unreach = global [40 x i8] c\"Reached unreachable control flow path!\\0A\\00\"\n",
if printblock then
"@enteringChunk = global [16 x i8] c\"Entering chunk\\0A\\00\"\n\
\@enteringBlock = global [19 x i8] c\"Entering block %s\\0A\\00\"\n" else "",
if printstmt then
"@fcall = global [15 x i8] c\"Function call\\0A\\00\"\n\
\@stmt = global [11 x i8] c\"statement\\0A\\00\"\n" else "",
if printmove then
"@gotlhs = global [9 x i8] c\"got lhs\\0A\\00\"\n\
\@gotrhs = global [9 x i8] c\"got rhs\\0A\\00\"\n" else ""]
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

fun kindIsEntry kind =
    case kind of
        Kind.Cont _ => true
      | Kind.CReturn {func, ...} => CFunction.mayGC func
      | Kind.Func => true
      | Kind.Handler _ => true
      | _ => false

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
        concat ["\t", lhs, " = call ", ty, " @llvm.", f, ".", fx,
                "(", ty, " ", a0, ")\n"]
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
        val indices = String.concatWith (List.map (idcs, fn i => "i32 " ^ i), ", ")
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
       | Real_abs _ => false (* Requires LLVM 3.2 to use "llvm.fabs" intrinsic *)
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
       | Real_qequal _ => false
       | Real_rndToReal _ => true
       | Real_rndToWord _ => true
       | Real_round _ => false (* Requires LLVM 3.3 to use "llvm.rint" intrinsic *)
       | Real_sub _ => true
       | Word_add _ => true
       | Word_addCheck _ => true
       | Word_andb _ => true
       | Word_castToReal _ => true
       | Word_equal _ => true
       | Word_extdToWord _ => true
       | Word_lshift _ => true
       | Word_lt _ => true
       | Word_mul _ => true
       | Word_mulCheck _ => true
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

(* Reuse CType for LLVM type *)
fun llty (ty: Type.t): string = "%" ^ CType.toString (Type.toCType ty)

val cFunctions = ref []

fun addCFunction f = if not (List.contains (!cFunctions, f, String.equals))
                     then cFunctions := List.cons (f, !cFunctions)
                     else ()

val ffiSymbols = ref []

fun addFfiSymbol s = if not (List.contains (!ffiSymbols, s, fn ({name=n1, ...}, {name=n2, ...}) =>
                             String.equals (n1, n2)))
                     then ffiSymbols := List.cons (s, !ffiSymbols)
                     else ()

fun offsetGCState (gcfield, ty) =
    let
        val castreg = nextLLVMReg ()
        val cast = mkconv (castreg, "bitcast", "%struct.GC_state*", "@gcState", "%Pointer")
        val ptr = nextLLVMReg ()
        val gep = mkgep (ptr, "%Pointer", castreg, [llbytes (GCField.offset gcfield)])
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
        val ptrsize = llbytes (Bits.toBytes (Control.Target.Size.cpointer ()))
        val ptr = nextLLVMReg ()
        val gep = mkgep (ptr, "%Pointer", stacktop, ["-" ^ ptrsize])
        val castreg = nextLLVMReg ()
        val cast = mkconv (castreg, "bitcast", "%Pointer", ptr, "%uintptr_t*")
        val loadreg = nextLLVMReg ()
        val loadofs = mkload (loadreg, "%uintptr_t*", castreg)
        val store = mkstore ("%uintptr_t", loadreg, "%l_nextFun")
        val br = "\tbr label %top\n"
    in
        concat [comment, loadst, gep, cast, loadofs, store, br]
    end

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
            val gep = mkgep (ptr, baseTy, baseReg, [offsettedIndex])
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
            val gep = mkgep (ptr, ty ^ "*", globalID, ["0", llint globalIndex])
        in
            (gep, llvmTy, ptr)
        end
      | Operand.Offset {base, offset, ty} =>
        let
            val (basePre, baseTy, baseReg) = getOperandValue (cxt, base)
            val idx = llbytes offset
            val llvmTy = llty ty
            val ptr = nextLLVMReg ()
            val gep = mkgep (ptr, baseTy, baseReg, [idx])
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
            val gep = mkgep (gepReg, "%Pointer", stackTop, [idx])
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
        val Context { labelToStringIndex = labelToStringIndex, ... } = cxt
    in
        case operand of
            Operand.ArrayOffset _ => loadOperand ()
          | Operand.Cast (oper, ty) =>
            let
                val ((operPre, operTy, operReg), shouldLoad) =
                    case oper of
                        Operand.ArrayOffset _ => (getOperandAddr (cxt, oper), true)
                      | Operand.Cast _ => (getOperandValue (cxt, oper), false)
                      | Operand.Contents _ => (getOperandAddr (cxt, oper), true)
                      | Operand.Frontier => (getOperandAddr (cxt, oper), true)
                      | Operand.GCState => (getOperandValue (cxt, oper), false)
                      | Operand.Global _ => (getOperandAddr (cxt, oper), true)
                      | Operand.Label _ => (getOperandValue (cxt, oper), false)
                      | Operand.Null => (getOperandValue (cxt, oper), false)
                      | Operand.Offset _ => (getOperandAddr (cxt, oper), true)
                      | Operand.Real _ => (getOperandValue (cxt, oper), false)
                      | Operand.Register _ => (getOperandAddr (cxt, oper), true)
                      | Operand.StackOffset _ => (getOperandAddr (cxt, oper), true)
                      | Operand.StackTop => (getOperandAddr (cxt, oper), true)
                      | Operand.Word _ => (getOperandValue (cxt, oper), false)
                val llvmTy = llty ty
                val reg = nextLLVMReg ()
                val inst = if shouldLoad
                           then
                               let
                                   val castReg = nextLLVMReg ()
                                   val cast = mkconv (castReg, "bitcast", operTy ^ "*",
                                                      operReg, llvmTy ^ "*")
                                   val load = mkload (reg, llvmTy ^ "*", castReg)
                               in
                                   concat [cast, load]
                               end
                           else
                               let
                                   fun isIntegerType cty = case cty of
                                                               CType.Int8 => true
                                                             | CType.Int16 => true
                                                             | CType.Int32 => true
                                                             | CType.Int64 => true
                                                             | CType.Word8 => true
                                                             | CType.Word16 => true
                                                             | CType.Word32 => true
                                                             | CType.Word64 => true
                                                             | _ => false
                                   fun isPointerType cty = case cty of
                                                               CType.CPointer => true
                                                             | CType.Objptr => true
                                                             | _ => false
                                   val operIsInt = (isIntegerType o Type.toCType o Operand.ty) oper
                                   val operIsPtr = (isPointerType o Type.toCType o Operand.ty) oper
                                   val tyIsInt = (isIntegerType o Type.toCType) ty
                                   val tyIsPtr = (isPointerType o Type.toCType) ty
                                   val operation = if operIsInt andalso tyIsPtr
                                                   then "inttoptr"
                                                   else if operIsPtr andalso tyIsInt
                                                        then "ptrtoint"
                                                        else "bitcast"
                               in
                                   mkconv (reg, operation, operTy, operReg, llvmTy)
                               end
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
            in (* CPointer_diff returns an integer, not a pointer *)
                (concat [inst1, inst2, inst3], "%uintptr_t")
            end
          | CPointer_equal => (* icmp works with pointer types here *)
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, "icmp eq", "%Pointer", arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | CPointer_fromWord =>
            (mkconv (res, "inttoptr", "%Word32", arg0, "%Pointer"), "%Pointer")
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
            (mkconv (res, "ptrtoint", "%Pointer", arg0, "%Word32"), "%Pointer")
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
            let
                val pair =
                    case rs of
                        R32 => if WordSize.equals (ws, WordSize.word32)
                               then (mkconv (res, "bitcast", "float", arg0, "i32"), "i32")
                               else Error.bug "LLVM codegen: Real_castToWord"
                      | R64 => if WordSize.equals (ws, WordSize.word64)
                               then (mkconv (res, "bitcast", "double", arg0, "i64"), "i64")
                               else Error.bug "LLVM codegen: Real_castToWord"
            in
                pair
            end
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
          | Real_rndToReal rs =>
            let
                val pair =
                    case rs of
                        (RealSize.R64, RealSize.R32) =>
                        (mkconv (res, "fptrunc", "double", arg0, "float"), "float")
                      | (RealSize.R32, RealSize.R64) =>
                        (mkconv (res, "fpext", "float", arg0, "double"), "double")
                      | (RealSize.R32, RealSize.R32) => (* this is a no-op *)
                        (mkconv (res, "bitcast", "float", arg0, "float"), "float")
                      | (RealSize.R64, RealSize.R64) => (* this is a no-op *)
                        (mkconv (res, "bitcast", "double", arg0, "double"), "double")
            in
                pair
            end
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
          | Word_add ws =>
            let
                val llws = llws ws
            in 
                (mkinst (res, "add", llws, arg0, arg1), llws)
            end
          | Word_addCheck (ws, {signed}) =>
            let
                val opr = if signed then "sadd" else "uadd"
                val ty = llws ws
                val intty = llwsInt ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.", opr,
                                   ".with.overflow.", intty, "(", ty, " ", arg0, ", ", ty,
                                   " ", arg1, ")\n"]
                val resTy = concat ["{", ty, ", i1}"]
            in
                (inst, resTy)
            end
          | Word_andb ws => (mkinst (res, "and", llws ws, arg0, arg1), llws ws)
          | Word_castToReal (ws, rs) =>
            let
                val pair =
                    case rs of
                        R32 => if WordSize.equals (ws, WordSize.word32)
                               then (mkconv (res, "bitcast", "i32", arg0, "float"), "float")
                               else Error.bug "LLVM codegen: Word_castToReal"
                      | R64 => if WordSize.equals (ws, WordSize.word64)
                               then (mkconv (res, "bitcast", "i64", arg0, "double"), "double")
                               else Error.bug "LLVM codegen: Word_castToReal"
            in
                pair
            end
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
                val intty = llwsInt ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.", opr, ".with.overflow.",
                                   intty, "(", ty, " ", arg0, ", ", ty, " ", arg1, ")\n"]
                val resTy = concat ["{", ty, ", i1}"]
            in
                (inst, resTy)
            end
          | Word_neg ws => (mkinst (res, "sub", llws ws, "0", arg0), llws ws)
          | Word_negCheck ws =>
            let
                val ty = llws ws
                val intty = llwsInt ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.ssub.with.overflow.",
                                   intty, "(", ty, " 0, ", ty, " ", arg0, ")\n"]
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
            (mkinst (res, if signed then "ashr" else "lshr", llws ws, arg0, arg1), llws ws)
          | Word_sub ws => (mkinst (res, "sub", llws ws, arg0, arg1), llws ws)
          | Word_subCheck (ws, {signed}) =>
            let
                val opr = if signed then "ssub" else "usub"
                val ty = llws ws
                val intty = llwsInt ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.", opr, ".with.overflow.",
                                   intty, "(", ty, " ", arg0, ", ", ty, " ", arg1, ")\n"]
                val resTy = concat ["{", ty, ", i1}"]
            in
                (inst, resTy)
            end
          | Word_xorb ws => (mkinst (res, "xor", llws ws, arg0, arg1), llws ws)
          | _ => Error.bug "LLVM Codegen: Unsupported operation in outputPrim"
    end

(* argv - vector of (pre, ty, addr) triples
   i - index of argv
   returns: (pre, type, reg)
 *)
fun getArg (argv, i) =
    if Vector.length argv > i
    then Vector.sub (argv, i)
    else ("", "NO TYPE", "NO ARG " ^ Int.toString i)

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

fun push amt =
    let
        val stacktop = nextLLVMReg ()
        val load = mkload (stacktop, "%Pointer*", "%stackTop")
        val ptr = nextLLVMReg ()
        val gep = mkgep (ptr, "%Pointer", stacktop, [amt])
        val store = mkstore ("%Pointer", ptr, "%stackTop")
        val comment = concat ["\t; Push(", amt, ")\n"]
    in
        concat [comment, load, gep, store]
    end


fun outputStatement (cxt: Context, stmt: Statement.t): string =
    let
        val comment = concat ["\t; ", Layout.toString (Statement.layout stmt), "\n"]
        val Context { printstmt = printstmt, printmove = printmove, ...} = cxt
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
        fun tpush (return, size) =
            let
                val offset = llbytes (Bytes.- (size, Runtime.labelSize ()))
                val frameIndex = labelToStringIndex return
                val stackTop = nextLLVMReg ()
                val load = mkload (stackTop, "%Pointer*", "%stackTop")
                val gepReg = nextLLVMReg ()
                val gep = mkgep (gepReg, "%Pointer", stackTop, [offset])
                val castreg = nextLLVMReg ()
                val cast = mkconv (castreg, "bitcast", "%Pointer", gepReg, "%uintptr_t*")
                val storeIndex = mkstore ("%uintptr_t", frameIndex, castreg)
                val pushcode = push (llbytes size)
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
                val CFunction.T {maySwitchThreads,
                                 modifiesFrontier,
                                 readsStackTop,
                                 return = returnTy,
                                 target,
                                 writesStackTop, ...} = func
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
                            tpush (valOf return, size)
                        end
                val flushFrontierCode = if modifiesFrontier then flushFrontier () else ""
                val flushStackTopCode = if readsStackTop then flushStackTop () else ""
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
                val storeResult = if Type.isUnit returnTy
                                  then ""
                                  else  mkstore (llty returnTy, resultReg,
                                                 "@CReturn" ^ CType.name (Type.toCType returnTy))
                val cacheFrontierCode = if modifiesFrontier then cacheFrontier () else ""
                val cacheStackTopCode = if writesStackTop then cacheStackTop () else ""
                val returnCode = if maySwitchThreads
                                 then callReturn ()
                                 else case return of
                                          NONE => "\tbr label %unreachable\n"
                                        | SOME l => concat ["\tbr label %", Label.toString l, "\n"]
                val fcall = if printstmt
                            then "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @fcall, i32 0, i32 0))\n"
                            else ""
            in
                concat [comment,
                        "\t; GetOperands\n",
                        Vector.concatV paramPres,
                        push,
                        flushFrontierCode,
                        flushStackTopCode,
                        "\t; Call\n",
                        fcall,
                        call,
                        storeResult,
                        cacheFrontierCode,
                        cacheStackTopCode,
                        returnCode]
            end
          | Transfer.Call {label, return, ...} =>
            let
                val labelstr = Label.toString label
                val dstChunk = labelChunk label
                val push = case return of
                               NONE => ""
                             | SOME {return, size, ...} => tpush (return, size)
                val goto = if ChunkLabel.equals (labelChunk sourceLabel, dstChunk)
                           then concat ["\tbr label %", labelstr, "\n"]
                           else let
                               (* cont.nextChunk = ChunkN *)
                               val funcname = "@Chunk" ^ chunkLabelToString dstChunk
                               val func = nextLLVMReg ()
                               val cast = mkconv (func, "bitcast", "%struct.cont ()*", funcname,
                                                  "i8*")
                               val nextchunkptr = nextLLVMReg ()
                               val gep = mkgep (nextchunkptr, "%struct.cont*", "%cont", ["0", "0"])
                               val storeNCP = mkstore ("i8*", func, nextchunkptr)
                               (* nextFun = l *)
                               val storeNF = mkstore ("%uintptr_t", labelToStringIndex label,
                                                      "@nextFun")
                               val br = "\tbr label %leaveChunk\n"
                           in
                               concat [cast, gep, storeNCP, storeNF, br]
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
                val gep = mkgep (sum, "%Pointer", stackBottom, [exnStack])
                val store = mkstore ("%Pointer", sum, "%stackTop")
                (* l_nextFun = *(uintptr_t* )(StackTop - sizeof(void* )); *)
                val stackTop = nextLLVMReg ()
                val loadStackTop = mkload (stackTop, "%Pointer*", "%stackTop")
                val sizeofptr = case !Control.defaultWord of
                                    "word8" => "-1"
                                  | "word16" => "-2"
                                  | "word32" => "-4"
                                  | "word64" => "-8"
                                  | _ => Error.bug "LLVMCodegen.Raise"
                val offsetST = nextLLVMReg ()
                val subPtrSize = mkgep (offsetST, "%Pointer", stackTop, [sizeofptr])
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
                val defaultLabel = case default of
                                       SOME d => "%" ^ Label.toString d
                                     | NONE => "%unreachable"
                val branches = Vector.concatV (Vector.map (cases, fn (w, l) =>
                                   concat ["\t\t", llws (WordX.size w), " ", llwordx w,
                                           ", label %", Label.toString l, "\n"]))
                val (testpre, testty, testreg) = getOperandValue (cxt, test)
                val inst = concat ["\tswitch ", testty, " ", testreg,
                                   ", label ", defaultLabel, " [\n", branches, "\t]\n"]
            in
                concat [comment, testpre, inst]
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
        fun pop fi = push (llbytes (Bytes.~ (Program.frameSize (program, fi))))
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
                                                                  CType.name(Type.toCType ty))
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
        val blockBody = Vector.concatV (Vector.map (statements, outputStatementWithCxt))
        val blockTransfer = outputTransfer (cxt, transfer, label)
    in
        concat [blockLabel, printBlock, dopop, blockBody, blockTransfer, "\n"]
    end
        
fun outputChunk (cxt, print, chunk) =
    let
        val Context { labelToStringIndex, chunkLabelIndex, labelChunk,
                      chunkLabelToString, entryLabels, printblock, ... } = cxt
        val Chunk.T {blocks, chunkLabel, regMax} = chunk
        val chunkName = "Chunk" ^ chunkLabelToString chunkLabel
        val () = print (concat ["define %struct.cont @",
                                chunkName,
                                "() {\nentry:\n"])
        val () = if printblock
                 then print "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([16 x i8]* @enteringChunk, i32 0, i32 0))\n"
                 else ()
        val () = print "\t%cont = alloca %struct.cont\n"
        val () = print "\t%frontier = alloca %Pointer\n"
        val () = print "\t%l_nextFun = alloca %uintptr_t\n"
        val t1 = nextLLVMReg ()
        val () = print (mkload (t1, "%uintptr_t*", "@nextFun"))
        val () = print (mkstore ("%uintptr_t", t1, "%l_nextFun"))
        val () = print "\t%stackTop = alloca %Pointer\n"
        val () = print (cacheFrontier ())
        val () = print (cacheStackTop ())
        val () = List.foreach (CType.all,
                               fn t =>
                                  let
                                      val pre = concat ["\t%reg", CType.name t, "_"]
                                      val post = concat [" = alloca %", CType.toString t, "\n"]
                                  in
                                      Int.for (0, 1 + regMax t,
                                               fn i => print (concat [pre, llint i, post]))
                                  end)
        val () = print "\tbr label %top\n"
        val () = print "top:\n"
        val t2 = nextLLVMReg ()
        val () = print (mkload (t2, "%uintptr_t*", "%l_nextFun"))
        val entryLabelsInChunk = Vector.keepAll (entryLabels,
                                                 fn l => chunkLabelIndex chunkLabel =
                                                         chunkLabelIndex (labelChunk l))
        val branches = Vector.concatV (Vector.map (entryLabelsInChunk, fn label =>
                           let
                               val labelName = Label.toString label
                               val i = labelToStringIndex label
                           in
                               concat ["\t\t%uintptr_t ", i, ", label %", labelName, "\n"]
                           end))
        val () = print (concat ["\tswitch %uintptr_t ", t2,
                                ", label %default [\n", branches, "\t]\n"])
        val () = print (Vector.concatV (Vector.map (blocks, fn b => outputBlock (cxt, b))))
        val () = print "default:\n"
        val nextFun = nextLLVMReg ()
        val () = print (mkload (nextFun, "%uintptr_t*", "%l_nextFun"))
        val () = print (mkstore ("%uintptr_t", nextFun, "@nextFun"))
        val nextChunks_nextFun_ptr = nextLLVMReg ()
        val () = print (mkgep (nextChunks_nextFun_ptr,
                              "[0 x void (%struct.cont*)*]*", "@nextChunks", ["0", nextFun]))
        val nextChunks_nextFun = nextLLVMReg ()
        val () = print (mkload (nextChunks_nextFun, "void (%struct.cont*)**",
                                nextChunks_nextFun_ptr))
        val nextChunks_nextFun_bc = nextLLVMReg ()
        val () = print (mkconv (nextChunks_nextFun_bc, "bitcast", "void (%struct.cont*)*",
                                nextChunks_nextFun, "i8*"))
        val cont_nextChunk_ptr = nextLLVMReg ()
        val () = print (mkgep (cont_nextChunk_ptr, "%struct.cont*", "%cont", ["0", "0"]))
        val () = print (mkstore ("i8*", nextChunks_nextFun_bc, cont_nextChunk_ptr))
        val () = print "\tbr label %leaveChunk\n"
        val () = print "leaveChunk:\n"
        val () = print (flushFrontier ())
        val () = print (flushStackTop ())
        val leaveRet = nextLLVMReg ()
        val () = print (mkload (leaveRet, "%struct.cont*", "%cont"))
        val () = print (concat ["\tret %struct.cont ", leaveRet, "\n"])
        val () = print "unreachable:\n"
        val () = print "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([40 x i8]* @unreach, i32 0, i32 0))\n"
        val () = print "\tbr label %leaveChunk\n"
        val () = print "}\n\n"
    in
        ()
    end

fun outputGlobals () =
    let
        val globals =
            concat
                (List.map (CType.all,
                      fn t =>
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
    in
        concat [globals, nonroot]
    end

fun outputLLVMDeclarations (cxt, print) =
    let
        val Context { program = program, printblock = printblock, ...} = cxt
        val Program.T { chunks = chunks, ... } = program
        val globals = outputGlobals ()
        val labelStrings = if printblock
                           then concat (List.map (chunks, fn c =>
            let
                val Chunk.T { blocks = blocks, ... } = c
            in
                Vector.concatV (Vector.map (blocks, fn b =>
                    let
                        val Block.T { label = label, ... } = b
                        val labelstr = Label.toString label
                        val len = Int.toString (String.size labelstr + 1)
                    in
                        concat ["@labelstr_", labelstr, " = global [", len, " x i8] ",
                                llstring labelstr, "\n"]
                    end))
            end))
                           else ""
    in
        print (concat [llvmIntrinsics, "\n", mltypes, "\n", ctypes (),
                       "\n", globals, "\n", globalDeclarations cxt, "\n", labelStrings, "\n"])
    end

fun annotate (frameLayouts, chunks) =
    let
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
        (chunkLabelIndex, chunkLabelToString, labelToStringIndex, entryLabels, labelChunk,
         labelInfo)
    end

fun makeContext program =
    let
        val Program.T { chunks, frameLayouts, ...} = program
        val (chunkLabelIndex, chunkLabelToString, labelToStringIndex, entryLabels,
             labelChunk, labelInfo) = annotate (frameLayouts, chunks)
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
        val { done, print, file=_ } = outputLL ()
        val () = outputLLVMDeclarations (cxt, print)
        val () = List.foreach (chunks, fn chunk => outputChunk (cxt, print, chunk))
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

fun transC (cxt, outputC) =
    let
        val Context { program, ... } = cxt
        local val Machine.Program.T
                      {chunks, 
                       frameLayouts, 
                       frameOffsets, 
                       handlesSignals, 
                       intInfs, 
                       main, 
                       maxFrameSize, 
                       objectTypes, 
                       reals, 
                       vectors, ...} = program
        in
        val machineProgram =
            Machine.Program.T
                {chunks = chunks,
                 frameLayouts = frameLayouts,
                 frameOffsets = frameOffsets,
                 handlesSignals = handlesSignals,
                 intInfs = intInfs,
                 main = main,
                 maxFrameSize = maxFrameSize,
                 objectTypes = objectTypes,
                 profileInfo = NONE,
                 reals = reals,
                 vectors = vectors}
        end

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
             program = machineProgram,
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
