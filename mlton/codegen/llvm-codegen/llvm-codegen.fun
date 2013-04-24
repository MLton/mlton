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
\%PointerAux = type i8\n\
\%Pointer = type %PointerAux*\n\
\\n\
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
\\n\
\%CPointer = type i8*\n\
\%Objptr = type %Pointer\n"

val llvmIntrinsics =
"declare float @llvm.sqrt.f32(float %Val)\n\
\declare double @llvm.sqrt.f64(double %Val)\n\
\declare float @llvm.sin.f32(float %Val)\n\
\declare double @llvm.sin.f64(double %Val)\n\
\declare float @llvm.cos.f32(float %Val)\n\
\declare double @llvm.cos.f64(double %Val)\n\
\declare float @llvm.tan.f32(float %Val)\n\
\declare double @llvm.tan.f64(double %Val)\n\
\declare float @llvm.exp.f32(float %Val)\n\
\declare double @llvm.exp.f64(double %Val)\n\
\declare float @llvm.log.f32(float %Val)\n\
\declare double @llvm.log.f64(double %Val)\n\
\declare float @llvm.log10.f32(float %Val)\n\
\declare double @llvm.log10.f64(double %Val)\n\
\declare float @llvm.fabs.f32(float %Val)\n\
\declare double @llvm.fabs.f64(double %Val)\n\
\declare float @llvm.rint.f32(float %Val)\n\
\declare double @llvm.rint.f64(double %Val)\n\
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
\declare float @llvm.fmuladd.f32(float %a, float %b, float %c)\n\
\declare double @llvm.fmuladd.f64(double %a, double %b, double %c)\n\
\declare i32 @printf(i8*, ...)\n"

(* LLVM codegen context. Contains various values/functions that should
   be shared amongst all codegen functions. *)
datatype Context = Context of {
    program: Program.t,
    print: string -> unit,
    labelToStringIndex: Label.t -> string,
    chunkLabelToString: ChunkLabel.t -> string
}

(* WordX.toString converts to hexadecimal, this converts to base 10 *)
fun llwordx (w: WordX.t) =
    IntInf.format (WordX.toIntInf w, StringCvt.DEC)

fun llword (w: Word.t) =
    IntInf.format (Word.toIntInf w, StringCvt.DEC)

fun llint (i: int) =
    if i >= 0
    then Int.toString i
    else "-" ^ Int.toString (~ i)

fun llbits b = llint (Bits.toInt b)

fun llbytes b = llint (Bytes.toInt b)

fun llbool b = if b then "1" else "0"

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

val globalDeclarations =
"%struct.cont = type { i8* }\n\
\%struct.GC_state = type opaque\n\
\@nextFun = external global %uintptr_t\n\
\@returnToC = external global i32\n\
\@nextChunks = external global [0 x i8* ()*]\n\
\@gcState = external global %struct.GC_state\n\
\@enteringChunk = global [16 x i8] c\"Entering chunk\\0A\\00\"\n\
\@enteringBlock = global [19 x i8] c\"Entering block %s\\0A\\00\"\n\
\@fcall = global [15 x i8] c\"Function call\\0A\\00\"\n\
\"

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
        RealSize.R32 => "Real32"
      | RealSize.R64 => "Real64"

fun llrsFloat (rs: RealSize.t): string =
    case rs of
        RealSize.R32 => "float"
      | RealSize.R64 => "double"

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

(* Makes an alloca instruction:
 * <lhs> = alloca <ty>
 *)
fun mkalloca (lhs, ty) = concat ["\t", lhs, " = alloca ", ty, "\n"]

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
       | Real_Math_tan _ => true
       | Real_abs _ => true
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
       | Real_round _ => true
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
    concat ["%", CType.name ty, "_", Int.toString index]

(* Reuse CType for LLVM type *)
fun llty (ty: Type.t): string = "%" ^ CType.toString (Type.toCType ty)

val cFunctions = ref []

fun addCFunction f = if not (List.contains (!cFunctions, f, String.equals))
                     then cFunctions := List.cons (f, !cFunctions)
                     else ()


fun offsetGCState gcfield =
    let
        val castreg = nextLLVMReg ()
        val cast = mkconv (castreg, "bitcast", "%struct.GC_state*", "@gcState", "%Pointer*")
        (* val ptr = nextLLVMReg () *)
        (* val load = concat ["\t", ptr, " = load %Pointer* ", castreg, "\n"] *)
        val intreg = nextLLVMReg ()
        val toint = mkconv (intreg, "ptrtoint", "%Pointer*", castreg, "%uintptr_t")
        val offsetted = nextLLVMReg ()
        val offset = mkinst (offsetted, "add", "%uintptr_t", intreg,
                             llbytes (GCField.offset gcfield))

        val offsettedptr = nextLLVMReg ()
        val toptr = mkconv (offsettedptr, "inttoptr", "%uintptr_t", offsetted, "%Pointer*")
    in
        (concat [cast, toint, offset, toptr], offsettedptr)
    end

(* FrontierMem = Frontier *)
fun flushFrontier () =
    let
        val comment = "\t; FlushFrontier\n"
        val (pre, reg) = offsetGCState GCField.Frontier
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
        val (pre, reg) = offsetGCState GCField.StackTop
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
        val (pre, reg) = offsetGCState GCField.Frontier
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
        val (pre, reg) = offsetGCState GCField.StackTop
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
        val load = mkload (stacktop, "%Pointer*", "%stackTop")
        val int = nextLLVMReg ()
        val toint = mkconv (int, "ptrtoint", "%Pointer", stacktop, "%uintptr_t")
        val offsetted = nextLLVMReg ()
        val ptrsize = llbits (Control.Target.Size.cpointer ())
        val sub = mkinst (offsetted, "sub", "%uintptr_t", int, ptrsize)
        val store = mkstore ("%uintptr_t", offsetted, "%l_nextFun")
    in
        concat [comment, load, toint, sub, store]
    end

(* Converts an operand into its LLVM representation. Returns a triple
 (pre, ty, reg) where

 pre - A string containing preliminary statements that must be
 executed before the register can be referenced

 ty - A string containing the LLVM representation of the register's
 type when dereferenced (meaning reg is really a pointer)

 reg - The register containing a pointer to the value of the operand
 *)

fun getOperand (cxt, operand) =
    case operand of
        Operand.ArrayOffset {base, index, offset, scale, ty} =>
        let (* result = base + (index * scale) + offset *)
            val (basePre, baseTy, baseReg) = getOperand (cxt, base)
            val (indexPre, indexTy, indexReg) = getOperand (cxt, index)
            val loadedIndex = nextLLVMReg ()
            val loadIndex = mkload (loadedIndex, indexTy ^ "*", indexReg)
            val scl = Scale.toString scale (* "1", "2", "4", or "8" *)
            val scaledIndex = nextLLVMReg ()
            val scaleIndex = mkinst(scaledIndex, "mul nsw", indexTy, loadedIndex, scl)
            val ofs = llbytes offset
            val offsettedIndex = nextLLVMReg ()
            val offsetIndex = mkinst (offsettedIndex, "add nsw", indexTy, scaledIndex, ofs)
            val llvmTy = llty ty
            val ptr = nextLLVMReg ()
            val gep = mkgep (ptr, baseTy ^ "*", baseReg, [offsettedIndex])
            val castedPtr = nextLLVMReg ()
            val cast = mkconv (castedPtr, "bitcast", baseTy ^ "*", ptr, llvmTy ^ "*")
        in
            (concat [basePre, indexPre, loadIndex, scaleIndex, offsetIndex, gep, cast],
             llvmTy, castedPtr)
        end
      | Operand.Cast (oper, ty) =>
        let
            val (operPre, operTy, operReg) = getOperand (cxt, oper)
            val llvmTy = llty ty
            val reg = nextLLVMReg ()
            val inst = mkconv (reg, "bitcast", operTy ^ "*", operReg, llvmTy ^ "*")
        in
            (concat [operPre, inst], llvmTy, reg)
        end
      | Operand.Contents {oper, ty} =>
        let
            val (operPre, operTy, operReg) = getOperand (cxt, oper)
            val llvmTy = llty ty
            val reg = nextLLVMReg ()
            val inst = mkconv (reg, "bitcast", operTy ^ "*", operReg, llvmTy ^ "*")
        in
            (concat [operPre, inst], llvmTy, reg)
        end
      | Operand.Frontier => ("", "%Pointer", "%frontier")
      | Operand.GCState =>
        let
            val reg = nextLLVMReg ()
            val cast = mkconv (reg, "bitcast", "%struct.GC_state*", "@gcState", "%Pointer")
            val reg2 = nextLLVMReg ()
            val alloca = mkalloca (reg2, "%Pointer")
            val store = mkstore ("%Pointer", reg, reg2)
        in
            (concat [cast, alloca, store], "%Pointer", reg2)
        end
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
      | Operand.Label label =>
        let
            val Context { labelToStringIndex = labelToStringIndex, ...} = cxt
            val labelVal = labelToStringIndex label
            val reg = nextLLVMReg ()
            val alloca = mkalloca (reg, "%Word32")
            val store = mkstore ("%Word32", labelVal, reg)
            val reg2 = nextLLVMReg ()
            val cast = mkconv (reg2, "bitcast", "%Word32*", reg, "%CPointer*")
        in
            (concat [alloca, store, cast], "%CPointer", reg2)
        end
      | Operand.Null =>
        let
            val reg = nextLLVMReg ()
            val alloca = mkalloca (reg, "i8*")
            val store = mkstore ("i8*", "null", reg)
        in
            (concat [alloca, store], "i8*", reg)
        end
      | Operand.Offset {base, offset, ty} =>
        let
            val (basePre, baseTy, baseReg) = getOperand (cxt, base)
            val idx = llbytes offset
            val llvmTy = llty ty
            val base = nextLLVMReg ()
            val load = mkload (base, baseTy ^ "*", baseReg)
            val intreg = nextLLVMReg ()
            val conv = mkconv (intreg, "ptrtoint", baseTy, base, "%uintptr_t")
            val offsetreg = nextLLVMReg ()
            val add = mkinst (offsetreg, "add", "%uintptr_t", intreg, idx)
            val ptrreg = nextLLVMReg ()
            val convback = mkconv (ptrreg, "inttoptr", "%uintptr_t", offsetreg, baseTy ^ "*")
            val reg = nextLLVMReg ()
            val cast = mkconv (reg, "bitcast", baseTy ^ "*", ptrreg, llvmTy ^ "*")
        in
            (concat [basePre, load, conv, add, convback, cast], llvmTy, reg)
        end
      | Operand.Real real =>
        let
            val reg = nextLLVMReg ()
            val ty = llrs (RealX.size real)
            val realval = RealX.toString real
            val alloca = mkalloca (reg, ty)
            val store = mkstore (ty, realval, reg)
        in
            (concat [alloca, store], ty, reg)
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
            val gepReg = nextLLVMReg ()
            val gep = mkgep (gepReg, "%Pointer*", "%stackTop", [idx])
            val llvmTy = llty ty
            val reg = nextLLVMReg ()
            val cast = mkconv (reg, "bitcast", "%Pointer*", gepReg, llvmTy ^ "*")
        in
            (concat [gep, cast], llvmTy, reg)
        end  
      | Operand.StackTop => ("", "%Pointer", "%stackTop")
      | Operand.Word word =>
        let
            val reg = nextLLVMReg ()
            val ty = llws (WordX.size word)
            val wordval = llwordx word
            val alloca = mkalloca (reg, ty)
            val store = mkstore (ty, wordval, reg)
        in
            (concat [alloca, store], ty, reg)
        end

(* Returns (instruction, ty) pair for the given prim operation *)
fun outputPrim (prim, res, arg0, arg1, arg2) =
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
            in (* CPointer_diff returns a Word32, not a Pointer *)
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
            (mkinst (res, "ptrtoint", "%Pointer", arg0, "%Word32"), "%Pointer")
          | FFI_Symbol _ =>
            let
            in
                ("", "") (* TODO *)
            end
          | Real_Math_cos rs => (mkmath (res, "cos", rs, arg0), llrs rs)
          | Real_Math_exp rs => (mkmath (res, "exp", rs, arg0), llrs rs)
          | Real_Math_ln rs => (mkmath (res, "log", rs, arg0), llrs rs)
          | Real_Math_log10 rs => (mkmath (res, "log10", rs, arg0), llrs rs)
          | Real_Math_sin rs => (mkmath (res, "sin", rs, arg0), llrs rs)
          | Real_Math_sqrt rs => (mkmath (res, "sqrt", rs, arg0), llrs rs)
          | Real_Math_tan rs => (mkmath (res, "tan", rs, arg0), llrs rs)
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
          | Real_equal rs => (mkinst (res, "fcmp oeq", llrs rs, arg0, arg1), llrs rs)
          | Real_le rs => (mkinst (res, "fcmp ole", llrs rs, arg0, arg1), llrs rs)
          | Real_lt rs => (mkinst (res, "fcmp olt", llrs rs, arg0, arg1), llrs rs)
          | Real_mul rs => (mkinst (res, "fmul", llrs rs, arg0, arg1), llrs rs)
          | Real_muladd rs =>
            let
                val size = case rs of
                               RealSize.R32 => "f32"
                             | RealSize.R64 => "f64"
                val llsize = llrs rs
                val inst = concat ["\t", res, " = call ", llsize, " @llvm.fmuladd.", size, "(",
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
                val inst2 = concat ["\t", res, " = call ", llsize, " @llvm.fmuladd.", size, "(",
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
          | Word_equal ws =>
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, "icmp eq", llws ws, arg0, arg1)
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
                val inst = concat ["\t", res, " = call {", ty, ", } @llvm.ssub.with.overflow.",
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
   returns: (pre, reg)
 *)
fun getArg (argv, i) =
    if Vector.length argv > i
    then
        let
            val (pre, ty, addr) = Vector.sub (argv, i)
            val reg = nextLLVMReg ()
            val load = mkload (reg, ty ^ "*", addr)
        in
            (concat [pre, load], reg)
        end
    else
        ("", "NO ARG " ^ Int.toString i)

fun outputPrimApp (cxt, p) =
    let
        datatype z = datatype Prim.Name.t
        val {args, dst, prim} = p
        val operands = Vector.map (args, fn opr => getOperand (cxt, opr))
        val (arg0pre, arg0reg) = getArg (operands, 0)
        val (arg1pre, arg1reg) = getArg (operands, 1)
        val (arg2pre, arg2reg) = getArg (operands, 2)
        val reg = nextLLVMReg ()
        val (inst, instTy) = outputPrim (prim, reg, arg0reg, arg1reg, arg2reg)
        val storeDest =
            case dst of
                NONE => ""
              | SOME dest =>
                let
                    val (destPre, destTy, destReg) = getOperand (cxt, dest)
                    val store = mkstore (destTy, reg, destReg)
                in
                    concat [destPre, store]
                end
    in
        concat [arg0pre, arg1pre, arg2pre, inst, storeDest]
    end

fun outputStatement (cxt, stmt) =
    let
        val comment = concat ["\t; ", Layout.toString (Statement.layout stmt), "\n"]
    in
        case stmt of
            Statement.Move {dst, src} =>
            let
                val (srcpre, srcty, srcreg) = getOperand (cxt, src)
                val (dstpre, dstty, dstreg) = getOperand (cxt, dst)
                val reg = nextLLVMReg ()
                val load = mkload (reg, srcty ^ "*", srcreg)
                val store = mkstore (dstty, reg, dstreg)
            in
                concat [comment, srcpre, load, dstpre, store]
            end
          | Statement.Noop => "\t; Noop\n"
          | Statement.PrimApp p =>
            let
                val primapp = outputPrimApp (cxt, p)
            in
                concat [comment, primapp]
            end
          | Statement.ProfileLabel _ => comment
    end

fun outputTransfer (cxt, transfer, sourceLabel) =
    let
        val comment = concat ["\t; ", Layout.toString (Transfer.layout transfer), "\n"]
        val Context { labelToStringIndex = labelToStringIndex, ... } = cxt
    in
        case transfer of
            Transfer.Arith {args, dst, overflow, prim, success} =>
            let
                val overflowstr = Label.toString overflow
                val successstr = Label.toString success
                val operands = Vector.map (args, fn opr => getOperand (cxt, opr))
                val (arg0pre, arg0reg) = getArg (operands, 0)
                val (arg1pre, arg1reg) = getArg (operands, 1)
                val (arg2pre, arg2reg) = getArg (operands, 2)
                val reg = nextLLVMReg ()
                val (inst, ty) = outputPrim (prim, reg, arg0reg, arg1reg, arg2reg)
                val res = nextLLVMReg ()
                val extractRes = concat ["\t", res, " = extractvalue ", ty, " ", reg, ", 0\n"]
                val obit = nextLLVMReg ()
                val extractObit = concat ["\t", obit, " = extractvalue ", ty, " ", reg, ", 1\n"]
                val (destPre, destTy, destReg) = getOperand (cxt, dst)
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
                                 writesStackTop,
                                 args = cFuncArgs, ...} = func
                val params = Vector.map (args, fn opr => getOperand (cxt, opr))
                val (_, paramTypes, _) = Vector.unzip3 params
                val (paramPres, paramRegs) = Vector.unzip (Vector.mapi (params, fn (i, _) =>
                                                 getArg (params, i)))
                val push =
                    case frameInfo of
                        NONE => ""
                      | SOME finfo =>
                            let
                                val Context { program = program, ... } = cxt
                                val size = Program.frameSize (program, finfo)
                                val offset = llbytes (Bytes.- (size, Runtime.labelSize ()))
                                val labelIndex = labelToStringIndex (valOf return)
                                val gepreg = nextLLVMReg ()
                                val gep = mkgep (gepreg, "%Pointer*", "%stackTop", [offset])
                                val castreg = nextLLVMReg ()
                                val cast = mkconv (castreg, "bitcast", "%Pointer*", gepreg, "i32*")
                                val storeIndex = mkstore ("i32", labelIndex, castreg)
                                val stacktop = nextLLVMReg ()
                                val loadStackTop = mkload (stacktop, "%Pointer*", "%stackTop")
                                val convreg = nextLLVMReg ()
                                val conv = mkconv (convreg, "ptrtoint", "%Pointer", stacktop,
                                                   "%uintptr_t")
                                val pushreg = nextLLVMReg ()
                                val push = mkinst (pushreg, "add", "%uintptr_t", convreg,
                                                   llbytes size)
                                val convreg2 = nextLLVMReg ()
                                val conv2 = mkconv (convreg2, "inttoptr", "%uintptr_t", pushreg,
                                                    "%Pointer")
                                val store = mkstore ("%Pointer", convreg2, "%stackTop")
                            in
                                concat [gep, cast, storeIndex, loadStackTop, conv, push, conv2,
                                        store]
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
                             | CFunction.Target.Indirect =>
                               let
                                   (* val (fptr, args) = *)
                                   (*     case args of *)
                                   (*         (fptr::args) => (fptr, args) *)
                                   (*       | _ => Error.bug "LLVMCodegen.outputTransfer: CCall, indirect" *)
                               in
                                   (* TODO *) ""
                               end
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
            in
                concat [comment,
                        push,
                        flushFrontierCode,
                        flushStackTopCode,
                        "\t; GetOperands\n",
                        Vector.concatV paramPres,
                        "\t; Call\n",
                        "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @fcall, i32 0, i32 0))\n",
                        call,
                        "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @fcall, i32 0, i32 0))\n",
                        storeResult,
                        cacheFrontierCode,
                        cacheStackTopCode,
                        returnCode]
            end
          | Transfer.Call {label, return, ...} =>
            let
                val labelstr = Label.toString label
                val push = case return of
                               NONE => ""
                             | SOME {return, size, ...} => "\t; push\n" (* TODO *)
                val goto = concat ["\tbr label %", labelstr, "\n"]
                (* TODO check if it leaves the chunk *)
            in
                concat [comment, push, goto]
            end
          | Transfer.Goto label =>
            let
                val labelString = Label.toString label
                val goto = concat ["\tbr label %", labelString, "\n"]
            in
                concat [comment, goto]
            end
          | Transfer.Raise =>
            (* StackTop = StackBottom + ExnStack TODO *)
            (* Return TODO *)
            concat [comment, "\tbr label %unreachable\n"]
          | Transfer.Return => concat [comment, "\tbr label %unreachable\n"]
          | Transfer.Switch switch =>
            let
                val Switch.T {cases, default, test, ...} = switch
                val defaultLabel = case default of
                                       SOME d => "%" ^ Label.toString d
                                     | NONE => "%unreachable"
                val branches =
                    Vector.concatV 
                        (Vector.map
                             (cases, fn (w, l) => concat ["\t\t", llws (WordX.size w), " ",
                                                          llwordx w,
                                                          ", label %", Label.toString l, "\n"]))
                val (testpre, testty, testreg) = getOperand (cxt, test)
                val loadedTestReg = nextLLVMReg ()
                val loadTest = mkload (loadedTestReg, testty ^ "*", testreg)
                val inst = concat ["\tswitch ", testty, " ", loadedTestReg, ", label ", defaultLabel,
                                   " [\n", branches, "\t]\n"]
            in
                concat [comment, testpre, loadTest, inst]
            end
    end

fun outputBlock (cxt, block) =
    let
        val Context { program = program, ... } = cxt
        val Block.T {kind, label, statements, transfer, ...} = block
        val labelstr = Label.toString label
        val labelstrLen = Int.toString (String.size labelstr + 1)
        val blockLabel = labelstr ^ ":\n"
        val printBlock = concat ["\tcall i32 (i8*, ...)* @printf(",
            "i8* getelementptr inbounds ([19 x i8]* @enteringBlock, i32 0, i32 0), ",
            "i8* getelementptr inbounds ([", labelstrLen, " x i8]* @labelstr_", labelstr,
                 ", i32 0, i32 0))\n"]
        fun pop fi =
            let
                val offset = llbytes (Bytes.~ (Program.frameSize (program, fi)))
                val stacktop = nextLLVMReg ()
                val load = mkload (stacktop, "%Pointer*", "%stackTop")
                val convreg = nextLLVMReg ()
                val conv = mkconv (convreg, "ptrtoint", "%Pointer", stacktop, "%uintptr_t")
                val pushreg = nextLLVMReg ()
                val push = mkinst (pushreg, "add", "%uintptr_t", convreg, offset)
                val convreg2 = nextLLVMReg ()
                val conv2 = mkconv (convreg2, "inttoptr", "%uintptr_t", convreg, "%Pointer")
                val store = mkstore ("%Pointer ", convreg2, "%stackTop")
            in
                concat [load, conv, push, conv2, store]
            end
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
                                               val (dstpre, dstty, dstreg) = getOperand (cxt, xop)
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
        
fun outputChunk (cxt, chunk) =
    let
        val Context { print, labelToStringIndex, chunkLabelToString, ... } = cxt
        val Chunk.T {blocks, chunkLabel, regMax} = chunk
        val chunkName = "Chunk" ^ chunkLabelToString chunkLabel
        val () = print (concat ["define i8* @",
                                chunkName,
                                "() {\nentry:\n"])
        val () = print "\tcall i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([16 x i8]* @enteringChunk, i32 0, i32 0))\n"
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
                                      val pre = concat ["\t%", CType.name t, "_"]
                                      val post = concat [" = alloca %", CType.toString t, "\n"]
                                  in
                                      Int.for (0, 1 + regMax t,
                                               fn i => print (concat [pre, llint i, post]))
                                  end)
        val entryBlocks = Vector.keepAll (blocks, fn Block.T {kind, ...} => kindIsEntry kind)
        val branches =
            Vector.concatV
                (Vector.map
                     (entryBlocks, fn b =>
                                      let
                                          val label = Block.label b
                                          val labelName = Label.toString label
                                          val i = labelToStringIndex label
                                      in
                                          concat ["\t\t%uintptr_t ", i, ", label %", labelName, "\n"]
                                      end))
        val () = print (concat ["\tswitch %uintptr_t ", t1,
                                ", label %default [\n", branches, "\t]\n"])
        val () = print (Vector.concatV (Vector.map (blocks, fn b => outputBlock (cxt, b))))
        val () = print "default:\n"
        val () = print "\tret i8* null\n\n"
        val () = print "unreachable:\n"
        val () = print "\tret i8* null\n" (* TODO handle this better *)
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
                             concat ["@global", s, " = external global [",
                                     llint (Global.numberOfType t),
                                     " x %", s, "]\n@CReturn", CType.name t,
                                     " = external global %", s, "\n"]
                         end))
        val nonroot = concat ["@globalObjptrNonRoot = external global [",
                              llint (Global.numberOfNonRoot ()),
                              " x %Pointer]\n"]
    in
        concat [globals, nonroot]
    end

fun outputLLVMDeclarations cxt =
    let
        val Context { print = print, program = program, ...} = cxt
        val Program.T { chunks = chunks, ... } = program
        val globals = outputGlobals ()
(*        val globalDecs = declareGlobals cxt *)
        val labelStrings = concat (List.map (chunks, fn c =>
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
                                
    in
        print (concat [llvmIntrinsics, "\n", mltypes, "\n", ctypes (),
                       "\n", globals, "\n", globalDeclarations, "\n", labelStrings, "\n"])
    end

fun annotate (frameLayouts, chunks) =
    let
        val {get = labelInfo: Label.t -> {block: Block.t,
                                          chunkLabel: ChunkLabel.t,
                                          frameIndex: int option,
                                          layedOut: bool ref},
             set = setLabelInfo, ...} =
            Property.getSetOnce
                (Label.plist, Property.initRaise ("CCodeGen.info", Label.layout))
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
        (* val labelChunk = #chunkLabel o labelInfo *)
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
        fun labelToStringIndex (l: Label.t): string = llint (labelIndex l)
                
    in
        (chunkLabelToString, labelToStringIndex, entryLabels, labelInfo)
    end

fun transLLVM (program, outputLL) =
    let
        val Program.T { chunks, frameLayouts, ...} = program
        val { done, print, file=_ } = outputLL ()
        val (chunkLabelToString, labelToStringIndex, _, _) = annotate (frameLayouts, chunks)
        val cxt = Context { program = program,
                            print = print,
                            labelToStringIndex = labelToStringIndex,
                            chunkLabelToString = chunkLabelToString }
        val () = outputLLVMDeclarations cxt
        val () = List.foreach (chunks, fn chunk => outputChunk (cxt, chunk))
        val () = List.foreach (!cFunctions, fn f =>
                     print (concat ["declare ", f, "\n"]))
    in
        done ()
    end

fun transC (program, outputC) =
    let
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
        val Program.T {main = main, chunks = chunks,
                       frameLayouts, ... } = program
        val (chunkLabelToString, labelToStringIndex, entryLabels, labelInfo) =
            annotate (frameLayouts, chunks)
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
        val () = transC (program, outputC)
        val () = transLLVM (program, outputLL)
    in
        ()
    end

end
