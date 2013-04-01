functor LLVMCodegen(S: LLVM_CODEGEN_STRUCTS): LLVM_CODEGEN =
struct

open S

open Machine

datatype z = datatype RealSize.t
datatype z = datatype WordSize.prim

val ctypes =
"; stdint.h\n\
\%uintptr_t = type i64\n"

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
\declare double @llvm.fmuladd.f64(double %a, double %b, double %c)\n"

(* LLVM codegen context. Contains various values/functions that should
   be shared amongst all codegen functions. *)
datatype Context = Context of {
    program: Program.t,
    print: string -> unit,
    indexer: Label.t -> string
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
        else Char.toString c))
           
fun llstring s = concat ["\"", escapeLLVM s, "\\00\""]

val globalDeclarations =
"%struct.cont = type { i8* }\n\
\%pointerAux = type i8\n\
\%pointer = type %pointerAux*\n\
\%size_t = type i32\n\
\%bool = type i8\n\
\%objptr = type i32 ; or i64, defined in gc/objptr.h\n\
\%struct.GC_state = type { i8*, i8*, i8*, i8*, i32, i32, i8, i8, i8**, i32, i32, i32, %struct.GC_callStackState, i8, %struct.GC_controls, %struct.GC_cumulativeStatistics, i32, %struct.GC_forwardState, %struct.GC_frameLayout*, i32, %struct.GC_generationalMaps, i32*, i32, i8, %struct.GC_heap, %struct.GC_intInfInit*, i32, %struct.GC_lastMajorStatistics, i8*, i32 (%struct._IO_FILE*)*, i32, i32, i8, %struct.GC_objectHashTable*, %struct.GC_objectType*, i32, %struct.GC_profiling, i32 (i32)*, i32, i32 (%struct._IO_FILE*)*, i8, %struct.GC_heap, i32, %struct.GC_signalsInfo, %struct.GC_sourceMaps, i8*, %struct.GC_sysvals, %struct.GC_translateState, %struct.GC_vectorInit*, i32, %struct.GC_weak* }\n\
\%struct.GC_callStackState = type { i32, i32* }\n\
\%struct.GC_controls = type { i32, i32, i8, i8, i8, i8, i32, %struct.GC_ratios, i8, i8 }\n\
\%struct.GC_ratios = type { float, float, float, float, float, float, float, float, float, float, float, float, float, float, float }\n\
\%struct.GC_cumulativeStatistics = type { i64, i64, i64, i64, i64, i64, i32, i32, i64, i32, i64, i64, i64, i64, i64, i64, %struct.rusage, %struct.rusage, %struct.rusage, %struct.rusage }\n\
\%struct.rusage = type { %struct.timeval, %struct.timeval, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }\n\
\%struct.timeval = type { i32, i32 }\n\
\%struct.GC_forwardState = type { i8, i8*, i8*, i8* }\n\
\%struct.GC_frameLayout = type { i32, i16*, i16 }\n\
\%struct.GC_generationalMaps = type { i8*, i8*, i32, i8*, i32, i32 }\n\
\%struct.GC_heap = type { i8*, i32, i32, i8*, i32 }\n\
\%struct.GC_intInfInit = type { i32, i8* }\n\
\%struct.GC_lastMajorStatistics = type { i32, i32, i32, i64 }\n\
\%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i32, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i32, i32, [40 x i8] }\n\
\%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }\n\
\%struct.GC_objectHashTable = type { %struct.GC_objectHashElement*, i8, i32, i32, i32, i8 }\n\
\%struct.GC_objectHashElement = type { i32, i8* }\n\
\%struct.GC_objectType = type { i32, i8, i16, i16 }\n\
\%struct.GC_profiling = type { %struct.GC_profileData*, i8, i32, i8 }\n\
\%struct.GC_profileData = type { i64*, %struct.GC_profileStack*, i64, i64 }\n\
\%struct.GC_profileStack = type { i64, i64, i64, i64, i64 }\n\
\%struct.GC_signalsInfo = type { i8, i8, i8, i32, %struct.__sigset_t, %struct.__sigset_t }\n\
\%struct.__sigset_t = type { [32 x i32] }\n\
\%struct.GC_sourceMaps = type { i32, i32*, i32, %struct.GC_sourceLabel*, i32, i8**, i32, i32**, i32, %struct.GC_source*, i32 }\n\
\%struct.GC_sourceLabel = type { i8*, i32 }\n\
\%struct.GC_source = type { i32, i32 }\n\
\%struct.GC_sysvals = type { i32, i32, i64 }\n\
\%struct.GC_translateState = type { i8*, i8* }\n\
\%struct.GC_vectorInit = type { i8*, i32, i32, i32 }\n\
\%struct.GC_weak = type <{ %struct.GC_weak*, i32 }>\n\
\@nextFun = common global %uintptr_t zeroinitializer\n\
\@returnToC = common global i32 zeroinitializer\n\
\@nextChunks = common global [0 x i8* ()*] zeroinitializer\n\
\@gcState = common global %struct.GC_state zeroinitializer\n"

fun declareAtMLtons () =
    let
        val atMLtons = !Control.atMLtons
        val atMLtonsNames = Vector.mapi (atMLtons, fn (i, _) => "@atMLton" ^ llint i)
        val atMLtonsLengths = Vector.map (atMLtons, fn a => llint ((String.length a) + 1)) (* +1 for trailing null *)
        val atMLtonsDecs = Vector.mapi (atMLtons, fn (i, a) =>
            let
                val name = Vector.sub (atMLtonsNames, i)
                val len = Vector.sub (atMLtonsLengths, i)
            in
                (concat [name, " = private unnamed_addr constant [",
                                    len, " x i8] c", llstring a, "\n"])
            end)
        val atMLtonsLen = llint (Vector.length atMLtons)
        val atMLtonsGEPsVec = Vector.mapi (atMLtonsNames, fn (i, name) =>
            let
                val len = Vector.sub (atMLtonsLengths, i)
            in
                concat ["\ti8* getelementptr inbounds ([", len, " x i8]* ", name, ", i32 0, i32 0)"]
            end)
        val atMLtonsGEPs = String.concatWith (Vector.toList atMLtonsGEPsVec, ",\n")
        val atMLtonsArray = concat ["@atMLtons = global [", atMLtonsLen, " x i8*] [\n",
                               atMLtonsGEPs, "\n]\n"]
    in
        concat [globalDeclarations,
                Vector.concatV atMLtonsDecs,
                atMLtonsArray]
    end

fun declareFrameOffsets cxt =
    let
        val Context { program = program, ... } = cxt
        val Program.T { frameOffsets = frameOffsets, ... } = program
        val frameOffsetsDecs = Vector.mapi (frameOffsets, fn (i, v) =>
            let
                val values = Vector.map (v, fn j => "i16 " ^ llbytes j)
                val valuesStr = String.concatWith (Vector.toList values, ", ")
            in
                concat ["@frameOffsets", llint i, " = global [", llint (Vector.length v),
                        " x i16] [", valuesStr, "]\n"]
            end)
    in
        Vector.concatV frameOffsetsDecs
    end

fun declareFrameLayouts cxt =
    let
        val Context { program = program, ... } = cxt
        val Program.T { frameLayouts = frameLayouts, frameOffsets = frameOffsets, ... } = program
        val len = llint (Vector.length frameLayouts)
        val beginDec = concat ["@frameLayouts = global [", len, " x %struct.GC_frameLayout] [\n"]
        val layoutsElems = Vector.map (frameLayouts, fn {frameOffsetsIndex, isC, size} =>
            let
                val isCVal = if isC then "0" else "1" (* from GC_frameKind in gc/frame.h *)
                val frameOffset = "@frameOffsets" ^ llint frameOffsetsIndex
                val frameOffsetLen = llint (Vector.length (Vector.sub (frameOffsets, frameOffsetsIndex)))
            in
                concat ["\t%struct.GC_frameLayout { i32 ", isCVal, ", i16* getelementptr ([",
                        frameOffsetLen, " x i16]* ", frameOffset, ", i32 0, i32 0), i16 ",
                        llbytes size, " }"]
            end)
        val layouts = String.concatWith (Vector.toList layoutsElems, ",\n")
        val endDec = "\n]\n"
    in
        concat [beginDec, layouts, endDec]
    end

fun declareIntInfInits cxt =
    let
        val Context { program = program, ... } = cxt
        val Program.T { intInfs = intInfs, ... } = program (* intInfs : (Global.t * IntInf.t) list *)
        val len = llint (List.length intInfs)
        val intInfConstants = concat (List.map (intInfs, fn (g, i) => 
            let
                val intInfStr = IntInf.toString i
                val strlen = String.length intInfStr
                val constant = concat ["@IntInfConstant", llint (Global.index g), " = global [",
                                      llint strlen, " x i8] ", llstring intInfStr, "\n"]
            in
                constant
            end))
        val beginDec = concat ["@intInfInits = global [", len, " x %struct.GC_intInfInit] [\n"]
        val intInfsElems = List.map (intInfs, fn (g, i) =>
            let
                val strlen = llint (String.length (IntInf.toString i))
                val constantName = concat ["@intInfConstant", llint (Global.index g)]
            in
                concat ["%struct.GC_intInfElem { i32 ", llint (Global.index g),
                        ", i8* getelementptr ([", strlen, " x i8]* ", constantName,
                       ", i32 0, i32 0) }"]
            end)
        val intInfs = String.concatWith (intInfsElems, ",\n")
        val endDec = "\n]\n"
    in
        concat [intInfConstants, beginDec, intInfs, endDec]
    end

fun declareLoadGlobals () =
    let
        val declareFread = "declare %size_t @fread(i8*, %size_t, %size_t, %struct._IO_FILE*)\n"
        fun loadArray a =
            let
                val aStr = "global" ^ CType.toString a
                val aNumber = Global.numberOfType a
                val aSize = llint ((Bytes.toInt (CType.size a)) * aNumber)
                val aLen = llint aNumber
                val ptr = concat ["%", aStr, "_ptr"]
                val gep = concat ["\t", ptr, " = getelementptr [", aLen, " x %",
                                   CType.toString a, "]* @", aStr, ", i32 0, i32 0\n"]
                val vptr = concat ["%", aStr, "_vptr"]
                val cast = concat ["\t", vptr, " = bitcast %", CType.toString a, "* ",
                                   ptr, " to i8*\n"]
                val callReg = concat ["%", aStr, "_call"]
                val call = concat ["\t", callReg, " = call %size_t @fread(i8* ", vptr,
                                   ", %size_t ", aSize, ", %size_t ", aLen,
                                   ", %struct._IO_FILE* %f)\n"]
                val testReg = concat ["%", aStr, "_test"]
                val test = concat ["\t", testReg, " = icmp ne %size_t ", callReg, ", ", aLen, "\n"]
                val ifTrueLabel = aStr ^ "_ifTrue"
                val ifFalseLabel = aStr ^ "_ifFalse"
                val br = concat ["\tbr i1 ", testReg, ", label %", ifTrueLabel,
                                 ", label %", ifFalseLabel, "\n"]
                val ifTrue = ifTrueLabel ^ ":\n\tret i32 -1\n"
                val ifFalse = ifFalseLabel ^ ":\n" (* fallthrough to next loadArray *)
            in
                concat [gep, cast, call, test, br, ifTrue, ifFalse]
            end
        val func = concat ["define i32 @loadGlobals(%struct._IO_FILE* %f) {\n",
                           "entry:\n"]
        val loadArrays = concat (List.map (CType.all, fn t => loadArray t))
        val funcEnd = "\tret i32 0\n}\n"
    in
        concat [declareFread, func, loadArrays, funcEnd]
    end

fun declareGlobals cxt =
    let
        val atMLtons = declareAtMLtons ()
        val frameOffsets = declareFrameOffsets cxt
        val frameLayouts = declareFrameLayouts cxt
        val intInfInits = declareIntInfInits cxt
        val loadGlobals = declareLoadGlobals ()
    in
        concat [atMLtons, frameOffsets, frameLayouts, intInfInits, loadGlobals]
    end

val mltonMainBegin =
"define i32 @MLtonMain(i32 %argc, i8** %argv) nounwind uwtable ssp {\n\
\entry:\n\
\\t%cont = alloca %struct.cont\n"

val mltonMainEnd = "}\n"

fun mltonMain (cxt: Context) =
    let
        val align = case !Control.align of
                        Control.Align4 => 4
                      | Control.Align8 => 8
        val al = llint align
        val magic = llword (case Random.useed () of
                                NONE => String.hash (!Control.inputFile)
                              | SOME w => w)
        val Context { program = program, ...} = cxt
        val Program.T { maxFrameSize = maxFrameSize,
                        frameLayouts = frameLayouts,
                        intInfs = intInfs, ...} = program
        val mfs = llbytes maxFrameSize
        val mmc = llbool (!Control.markCards)
        val profile = "2" (* PROFILE_NONE in runtime/gc/profiling.h *)
        val ps = "0" (* no profiling *)
        val atMLtLen = llint (Vector.length (!Control.atMLtons))
        val frameLayoutsLen = llint (Vector.length frameLayouts)
        val globalObjPtrLen = llint (Global.numberOfType CType.Objptr)
        val intInfInitsLen = llint (List.length intInfs)
        val initialize = concat [
            (* gcState.alignment = al; *)
            "\t%alignmentptr = getelementptr %struct.GC_state* @gcState, i32 0, i32 5\n",
            "\tstore i32 ", al, ", i32* %alignmentptr\n",
            (* gcState.atMLtons = atMLtons; *)
            "\t%atMLtonsptr = getelementptr %struct.GC_state* @gcState, i32 0, i32 8\n",
            "\t%global_atMLtonsptr = getelementptr [", atMLtLen, " x i8*]* @atMLtons, i32 0, i32 0\n",
            "\tstore i8** %global_atMLtonsptr, i8*** %atMLtonsptr\n",
            (* gcState.atMLtonsLength = atMLthonsLength; *)
            "\t%atMLtonsLengthptr = getelementptr %struct.GC_state* @gcState, i32 0, i32 9\n",
            "\tstore i32 ", atMLtLen, ", i32* %atMLtonsLengthptr\n",
            (* gcState.frameLayouts = frameLayouts; *)
            "\t%frameLayoutsptr = getelementptr %struct.GC_state* @gcState, i32 0, i32 18\n",
            "\t%frameLayouts0 = getelementptr [", frameLayoutsLen,
                " x %struct.GC_frameLayout]* @frameLayouts, i32 0, i32 0\n",
            "\tstore %struct.GC_frameLayout* %frameLayouts0, %struct.GC_frameLayout** %frameLayoutsptr\n",
            (* gcState.frameLayoutsLength = cardof(globalObjptr); *)
            "\t%frameLayoutsLengthptr = getelementptr %struct.GC_state* @gcState, i32 0, i32 19\n",
            "\tstore i32 ", frameLayoutsLen, ", i32* %frameLayoutsLengthptr\n",
            (* gcState.globals = (objptr* )globalObjptr; *)
            "\t%globalsptr = getelementptr %struct.GC_state* @gcState, i32 0, i32 21\n",
            "\t%globalObjptr0 = getelementptr [", globalObjPtrLen,
                " x %Objptr]* @globalObjptr, i32 0, i32 0\n",
            "\t%globalObjptrAsobjptr = bitcast %Objptr* %globalObjptr0 to %objptr*\n",
            "\tstore %objptr* %globalObjptrAsobjptr, %objptr** %globalsptr\n",
            (* gcState.globalsLength = cardof(globalObjptr); *)
            "\t%globalsLengthptr = getelementptr %struct.GC_state* @gcState, i32 0, i32 22\n",
            "\tstore i32 ", globalObjPtrLen, ", i32* %globalsLengthptr\n",
            (* gcState.infInfInits = intInfInits; *)
            "\t%intInfInitsptr = getelementptr %struct.GC_state* @gcState, i32 0, i32 25\n",
            "\t%intInfInits0 = getelementptr [", intInfInitsLen, " x %struct.GC_intInfInit]* ",
                "@intInfInits, i32 0, i32 0\n",
            "\tstore %struct.GC_intInfInit* %intInfInits0, %struct.GC_intInfInit** %intInfInitsptr\n",
            (* gcState.intInfInitsLength = cardof(intInfInits); *)
            "\t%intInfInitsLengthptr = getelementptr %struct.GC_state* @gcState, i32 0, i32 26\n",
            "\tstore i32 ", intInfInitsLen, ", i32* %intInfInitsLengthptr\n",
            (* gcState.loadGlobals = loadGlobals; *)
            "\t%loadGlobalsptr = getelementptr %struct.GC_state* @gcState, i32 0, i32 29\n",
            "\tstore i32 (%struct._IO_FILE*)* @loadGlobals, i32 (%struct._IO_FILE*)** %loadGlobalsptr\n",
            (* gcState.magic = mg; *)
            (* gcState.maxFrameSize = mfs; *)
            (* gcState.mutatorMarksCards = mmc; *)
            (* gcState.objectTypes = objectTypes; *)
            (* gcState.objectTypesLength = cardof(objectTypes); *)
            (* gcState.returnAddressToFrameIndex = returnAddressToFrameIndex; *)
            (* gcState.saveGlobals = saveGlobals; *)
            (* gcState.vectorInits = vectorInits; *)
            (* gcState.vectorInitsLength = cardof(vectorInits); *)
            (* gcState.sourceMaps.frameSources = frameSources; *)
            (* gcState.sourceMaps.frameSourcesLength = cardof(frameSources); *)
            (* gcState.sourceMaps.sourceLabels = sourceLabels; *)
            (* gcState.sourceMaps.sourceLabelsLength = cardof(sourceLabels); *)
            (* gcState.sourceMaps.sourceNames = sourceNames; *)
            (* gcState.sourceMaps.sourceNamesLength = cardof(sourceNames); *)
            (* gcState.sourceMaps.sourceSeqs = sourceSeqs; *)
            (* gcState.sourceMaps.sourceSeqsLength = cardof(sourceSeqs); *)
            (* gcState.sourceMaps.sources = sources; *)
            (* gcState.sourceMaps.sourcesLength = cardof(sources); *)
            (* gcState.profiling.kind = pk; *)
            (* gcState.profiling.stack = ps; *)

            "\tret i32 0\n"]
    in
        concat [mltonMainBegin, initialize, mltonMainEnd]
    end
        
val mainFunc = 
"define i32 @main(i32 %argc, i8** %argv) nounwind uwtable ssp {\n\
\entry:\n\
\\t%result = call i32 @MLtonMain(i32 %argc, i8** %argv)\n\
\\tret i32 %result\n\
\}\n"

fun wsToLLVM (ws: WordSize.t): string =
    case WordSize.prim ws of
        WordSize.W8 => "%Word8"
      | WordSize.W16 => "%Word16"
      | WordSize.W32 => "%Word32"
      | WordSize.W64 => "%Word64"

fun wsToLLVMInt (ws: WordSize.t): string =
    case WordSize.prim ws of
        WordSize.W8 => "i8"
      | WordSize.W16 => "i16"
      | WordSize.W32 => "i32"
      | WordSize.W64 => "i64"

fun rsToLLVM (rs: RealSize.t): string =
    case rs of
        RealSize.R32 => "Real32"
      | RealSize.R64 => "Real64"

fun rsToLLVMFloat (rs: RealSize.t): string =
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
        val ty = rsToLLVM rs
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
fun tyToLLVM ty = "%" ^ CType.toString (Type.toCType ty)

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
            val loadIndex = concat ["\t", loadedIndex, " = load ", indexTy, "* ",
                                    indexReg, "\n"]
            val scl = Scale.toString scale (* "1", "2", "4", or "8" *)
            val scaledIndex = nextLLVMReg ()
            val scaleIndex = mkinst(scaledIndex, "mul nsw", indexTy, loadedIndex, scl)
            val ofs = llbytes offset
            val offsettedIndex = nextLLVMReg ()
            val offsetIndex = mkinst (offsettedIndex, "add nsw", indexTy, scaledIndex, ofs)
            val llvmTy = tyToLLVM ty
            val ptr = nextLLVMReg ()
            val gep = concat ["\t", ptr, " = getelementptr inbounds ",
                              baseTy, "* ", baseReg, ", ", indexTy, " ",
                              offsettedIndex, "\n"]
            val castedPtr = nextLLVMReg ()
            val cast = mkconv (castedPtr, "bitcast", baseTy ^ "*", ptr, llvmTy ^ "*")
        in
            (concat [basePre, indexPre, loadIndex, scaleIndex, offsetIndex, gep, cast],
             llvmTy, castedPtr)
        end
      | Operand.Cast (oper, ty) =>
        let
            val (operPre, operTy, operReg) = getOperand (cxt, oper)
            val llvmTy = tyToLLVM ty
            val reg = nextLLVMReg ()
            val inst = mkconv (reg, "bitcast", operTy ^ "*", operReg, llvmTy ^ "*")
        in
            (concat [operPre, inst], llvmTy, reg)
        end
      | Operand.Contents {oper, ty} =>
        let
            val (operPre, operTy, operReg) = getOperand (cxt, oper)
            val llvmTy = tyToLLVM ty
            val reg = nextLLVMReg ()
            val inst = mkconv (reg, "bitcast", operTy ^ "*", operReg, llvmTy ^ "*")
        in
            (concat [operPre, inst], llvmTy, reg)
        end
      | Operand.Frontier => ("", "%Pointer", "%frontier")
      | Operand.GCState =>
        let
            val reg = nextLLVMReg ()
            val cast = concat ["\t", reg, " = bitcast %struct.GC_state* @gcState to %Pointer*\n"]
        in
            (cast, "%Pointer", reg)
        end
      | Operand.Global global =>
        let
            val globalType = Global.ty global
            val globalIsRoot = Global.isRoot global
            val globalIndex = Global.index global
            val llvmTy = tyToLLVM globalType
            val ty = typeOfGlobal global
            val globalID = if globalIsRoot
                           then "@global" ^ String.dropFirst llvmTy (* drop '%' *)
                           else "@globalObjptrNonRoot"
            val ptr = nextLLVMReg ()
            val gep = concat ["\t", ptr, " = getelementptr inbounds ",
                              ty, "* ", globalID, ", i32 0, i32 ",
                              llint globalIndex, "\n"]
        in
            (gep, llvmTy, ptr)
        end
      | Operand.Label label =>
        let
            val Context { indexer = indexer, ...} = cxt
            val labelVal = indexer label
            val reg = nextLLVMReg ()
            val alloca = concat ["\t", reg, " = alloca %Word32\n"]
            val store = concat ["\tstore %Word32 ", labelVal, ", %Word32* ", reg, "\n"]
            val reg2 = nextLLVMReg ()
            val cast = mkconv (reg2, "bitcast", "%Word32*", reg, "%CPointer*")
        in
            (concat [alloca, store, cast], "%CPointer", reg2)
        end
      | Operand.Null =>
        let
            val reg = nextLLVMReg ()
            val alloca = concat ["\t", reg, " = alloca i8*\n"]
            val store = concat ["\tstore i8* null, i8** ", reg, "\n"]
        in
            (concat [alloca, store], "i8*", reg)
        end
      | Operand.Offset {base, offset, ty} =>
        let
            val (basePre, baseTy, baseReg) = getOperand (cxt, base)
            val idx = llbytes offset
            val llvmTy = tyToLLVM ty
            val ptr = nextLLVMReg ()
            val gep = concat ["\t", ptr, " = getelementptr inbounds ", baseTy,
                              "* ", baseReg, ", i8 ", idx, "\n"]
            val reg = nextLLVMReg ()
            val cast = mkconv (reg, "bitcast", baseTy ^ "*", ptr, llvmTy ^ "*")
        in
            (concat [basePre, gep, cast], llvmTy, reg)
        end
      | Operand.Real real =>
        let
            val reg = nextLLVMReg ()
            val ty = rsToLLVM (RealX.size real)
            val realval = RealX.toString real
            val alloca = concat ["\t", reg, " = alloca ", ty, "\n"]
            val store = concat ["\tstore ", ty, " ", realval, ", ", ty, "* ", reg, "\n"]
        in
            (concat [alloca, store], ty, reg)
        end
      | Operand.Register register =>
        let
            val regty = Register.ty register
            val reg = regName (Type.toCType regty, Register.index register)
            val ty = tyToLLVM regty
        in
            ("", ty, reg)
        end
      | Operand.StackOffset stackOffset =>
        let
            val StackOffset.T {offset, ty} = stackOffset
            val idx = llbytes offset
            val gepReg = nextLLVMReg ()
            val gep = concat ["\t", gepReg, " = getelementptr inbounds %Pointer* %stackTop, i8 ",
                              idx, "\n"]
            val llvmTy = tyToLLVM ty
            val reg = nextLLVMReg ()
            val cast = mkconv (reg, "bitcast", "%Pointer*", gepReg, llvmTy ^ "*")
        in
            (concat [gep, cast], llvmTy, reg)
        end  
      | Operand.StackTop => ("", "%Pointer", "%stackTop")
      | Operand.Word word =>
        let
            val reg = nextLLVMReg ()
            val ty = wsToLLVM (WordX.size word)
            val wordval = llwordx word
            val alloca = concat ["\t", reg, " = alloca ", ty, "\n"]
            val store = concat ["\tstore ", ty, " ", wordval, ", ", ty, "* ", reg, "\n"]
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
                val inst1 = mkconv (tmp1, "ptrtoint", "%Pointer", arg0, "i32")
                val tmp2 = nextLLVMReg ()
                val inst2 = mkinst (tmp2, "add", "i32", tmp1, arg1)
                val inst3 = mkconv (res, "inttoptr", "i32", tmp2, "%Pointer")
            in
                (concat [inst1, inst2, inst3], "%Pointer")
            end
          | CPointer_diff =>
            let
                val tmp1 = nextLLVMReg ()
                val inst1 = mkconv (tmp1, "ptrtoint", "%Pointer", arg0, "%Word32")
                val tmp2 = nextLLVMReg ()
                val inst2 = mkconv (tmp2, "ptrtoint", "%Pointer", arg1, "%Word32")
                val inst3 = mkinst (res, "sub", "%Word32", tmp1, tmp2)
            in (* CPointer_diff returns a Word32, not a Pointer *)
                (concat [inst1, inst2, inst3], "%Word32")
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
                val inst1 = mkconv (tmp1, "ptrtoint", "%Pointer", arg0, "i32")
                val tmp2 = nextLLVMReg ()
                val inst2 = mkinst (tmp2, "sub", "i32", tmp1, arg1)
                val inst3 = mkconv (res, "inttoptr", "i32", tmp2, "%Pointer")
            in
                (concat [inst1, inst2, inst3], "%Pointer")
            end
          | CPointer_toWord =>
            (mkinst (res, "ptrtoint", "%Pointer", arg0, "%Word32"), "%Pointer")
          | FFI_Symbol _ => ("", "") (* TODO *)
          | Real_Math_cos rs => (mkmath (res, "cos", rs, arg0), rsToLLVM rs)
          | Real_Math_exp rs => (mkmath (res, "exp", rs, arg0), rsToLLVM rs)
          | Real_Math_ln rs => (mkmath (res, "log", rs, arg0), rsToLLVM rs)
          | Real_Math_log10 rs => (mkmath (res, "log10", rs, arg0), rsToLLVM rs)
          | Real_Math_sin rs => (mkmath (res, "sin", rs, arg0), rsToLLVM rs)
          | Real_Math_sqrt rs => (mkmath (res, "sqrt", rs, arg0), rsToLLVM rs)
          | Real_Math_tan rs => (mkmath (res, "tan", rs, arg0), rsToLLVM rs)
          | Real_abs rs => (mkmath (res, "fabs", rs, arg0), rsToLLVM rs)
          | Real_add rs => (mkinst (res, "fadd", rsToLLVM rs, arg0, arg1), rsToLLVM rs)
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
          | Real_div rs => (mkinst (res, "fdiv", rsToLLVM rs, arg0, arg1), rsToLLVM rs)
          | Real_equal rs => (mkinst (res, "fcmp oeq", rsToLLVM rs, arg0, arg1), rsToLLVM rs)
          | Real_le rs => (mkinst (res, "fcmp ole", rsToLLVM rs, arg0, arg1), rsToLLVM rs)
          | Real_lt rs => (mkinst (res, "fcmp olt", rsToLLVM rs, arg0, arg1), rsToLLVM rs)
          | Real_mul rs => (mkinst (res, "fmul", rsToLLVM rs, arg0, arg1), rsToLLVM rs)
          | Real_muladd rs =>
            let
                val size = case rs of
                               RealSize.R32 => "f32"
                             | RealSize.R64 => "f64"
                val llsize = rsToLLVM rs
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
                val llsize = rsToLLVM rs
                val tmp1 = nextLLVMReg ()
                val inst1 = mkinst (tmp1, "fsub", llsize, "-0.0", arg2)
                val inst2 = concat ["\t", res, " = call ", llsize, " @llvm.fmuladd.", size, "(",
                                    llsize, " ", arg0, ", ", llsize, " ",
                                    arg1, ", ", llsize, " ", tmp1, ")\n"]
            in
                (concat [inst1, inst2], llsize)
            end
          | Real_neg rs => (mkinst (res, "fsub", rsToLLVM rs, "-0.0", arg0), rsToLLVM rs)
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
                (mkconv (res, opr, rsToLLVM rs, arg0, wsToLLVM ws), wsToLLVM ws)
            end
          | Real_round rs => (mkmath (res, "rint", rs, arg0), rsToLLVM rs)
          | Real_sub rs => (mkinst (res, "fsub", rsToLLVM rs, arg0, arg1), rsToLLVM rs)
          | Thread_returnToC =>
            let
                val store = "\tstore i32 1, i32* @returnToC\n"
                val ret = "\tret %struct.cont %cont\n"
            in
                (concat [store, ret], "")
            end
          | Word_add ws =>
            let
                val llws = wsToLLVM ws
            in 
                (mkinst (res, "add", llws, arg0, arg1), llws)
            end
          | Word_addCheck (ws, {signed}) =>
            let
                val opr = if signed then "sadd" else "uadd"
                val ty = wsToLLVM ws
                val intty = wsToLLVMInt ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.", opr, ".with.overflow.",
                                   intty, "(", ty, " ", arg0, ", ", ty, " ", arg1, ")\n"]
                val resTy = concat ["{", ty, ", i1}"]
            in
                (inst, resTy)
            end
          | Word_andb ws => (mkinst (res, "and", wsToLLVM ws, arg0, arg1), wsToLLVM ws)
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
                val cmp = mkinst (reg, "icmp eq", wsToLLVM ws, arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | Word_extdToWord (ws1, ws2, {signed}) =>
            let
                val opr = case WordSize.compare (ws1, ws2) of
                              LESS => if signed then "sext" else "zext"
                            | EQUAL => "" (* MLton Bug *)
                            | GREATER => "trunc"
            in
                (mkconv (res, opr, wsToLLVM ws1, arg0, wsToLLVM ws2), wsToLLVM ws2)
            end
          | Word_lshift ws => (mkinst (res, "shl", wsToLLVM ws, arg0, arg1), wsToLLVM ws)
          | Word_lt (ws, {signed}) =>
            let
                val reg = nextLLVMReg ()
                val cmp = mkinst (reg, if signed then "icmp slt" else "icmp ult",
                                  wsToLLVM ws, arg0, arg1)
                val ext = mkconv (res, "zext", "i1", reg, "%Word32")
            in
                (concat [cmp, ext], "%Word32")
            end
          | Word_mul (ws, _) => (mkinst (res, "mul", wsToLLVM ws, arg0, arg1), wsToLLVM ws)
          | Word_mulCheck (ws, {signed}) =>
            let
                val opr = if signed then "smul" else "umul"
                val ty = wsToLLVM ws
                val intty = wsToLLVMInt ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.", opr, ".with.overflow.",
                                   intty, "(", ty, " ", arg0, ", ", ty, " ", arg1, ")\n"]
                val resTy = concat ["{", ty, ", i1}"]
            in
                (inst, resTy)
            end
          | Word_neg ws => (mkinst (res, "sub", wsToLLVM ws, "0", arg0), wsToLLVM ws)
          | Word_negCheck ws =>
            let
                val ty = wsToLLVM ws
                val intty = wsToLLVMInt ws
                val inst = concat ["\t", res, " = call {", ty, ", } @llvm.ssub.with.overflow.",
                                   intty, "(", ty, " 0, ", ty, " ", arg0, ")\n"]
                val resTy = concat ["{", ty, ", i1}"]
            in
                (inst, resTy)
            end
          | Word_notb ws => (mkinst (res, "xor", wsToLLVM ws, arg0, "-1"), wsToLLVM ws)
          | Word_orb ws => (mkinst (res, "or", wsToLLVM ws, arg0, arg1), wsToLLVM ws)
          | Word_quot (ws, {signed}) =>
            (mkinst (res, if signed then "sdiv" else "udiv", wsToLLVM ws, arg0, arg1), wsToLLVM ws)
          | Word_rem (ws, {signed}) =>
            (mkinst (res, if signed then "srem" else "urem", wsToLLVM ws, arg0, arg1), wsToLLVM ws)
          | Word_rndToReal (ws, rs, {signed}) =>
            let
                val opr = if signed then "sitofp" else "uitofp"
            in
                (mkconv (res, opr, wsToLLVM ws, arg0, rsToLLVM rs), rsToLLVM rs)
            end
          | Word_rol ws =>
            let
                val ty = wsToLLVM ws
                val tmp1 = nextLLVMReg ()
                val inst1 = mkinst (tmp1, "sub", ty, WordSize.toString ws, arg1)
                val tmp2 = nextLLVMReg ()
                val inst2 = mkinst (tmp2, "lshr", ty, arg0, tmp1)
                val tmp3 = nextLLVMReg ()
                val inst3 = mkinst (tmp3, "shl", ty, arg0, arg1)
                val inst4 = mkinst (res, "or", ty, tmp2, tmp3)
            in
                (concat [inst1, inst2, inst3, inst4], wsToLLVM ws)
            end
          | Word_ror ws =>
            let
                val ty = wsToLLVM ws
                val tmp1 = nextLLVMReg ()
                val inst1 = mkinst (tmp1, "lshr", ty, arg0, arg1)
                val tmp2 = nextLLVMReg ()
                val inst2 = mkinst (tmp2, "sub", ty, WordSize.toString ws, arg1)
                val tmp3 = nextLLVMReg ()
                val inst3 = mkinst (tmp3, "shl", ty, arg0, tmp2)
                val inst4 = mkinst (res, "or", ty, tmp1, tmp3)
            in
                (concat [inst1, inst2, inst3, inst4], wsToLLVM ws)
            end
          | Word_rshift (ws, {signed}) =>
            (mkinst (res, if signed then "ashr" else "lshr", wsToLLVM ws, arg0, arg1), wsToLLVM ws)
          | Word_sub ws => (mkinst (res, "sub", wsToLLVM ws, arg0, arg1), wsToLLVM ws)
          | Word_subCheck (ws, {signed}) =>
            let
                val opr = if signed then "ssub" else "usub"
                val ty = wsToLLVM ws
                val intty = wsToLLVMInt ws
                val inst = concat ["\t", res, " = call {", ty, ", i1} @llvm.", opr, ".with.overflow.",
                                   intty, "(", ty, " ", arg0, ", ", ty, " ", arg1, ")\n"]
                val resTy = concat ["{", ty, ", i1}"]
            in
                (inst, resTy)
            end
          | Word_xorb ws => (mkinst (res, "xor", wsToLLVM ws, arg0, arg1), wsToLLVM ws)
          | _ => ("\t<Unsupported operation!>\n", "")
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
            val load = concat ["\t", reg, " = load ", ty, "* ", addr, "\n"]
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
                    val store =
                        concat [destPre, "\tstore ", instTy, " ", reg, ", ",
                                destTy, "* ", destReg, "\n"]
                in
                    store
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
                val move = concat [srcpre, "\t", reg, " = load ", srcty,
                                   "* ", srcreg, "\n", dstpre,
                                   "\tstore ", srcty, " ", reg, ", ",
                                   dstty, "* ", dstreg, "\n"]
            in
                concat [comment, move]
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
                val store = concat ["\tstore ", destTy, " ", res, ", ", destTy, "* ", destReg, "\n"]
                val br = concat ["\tbr i1 ", obit, ", label %",
                                 overflowstr, ", label %", successstr, "\n"]
            in
                concat [comment, arg0pre, arg1pre, arg2pre, inst,
                        extractRes, extractObit, destPre, store, br]
            end
          | Transfer.CCall {args, frameInfo, func, return} =>
            concat [comment, "\tbr label %unreachable\n"]
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
                val Switch.T {cases, default, size, test} = switch
                val defaultLabel = case default of
                                       SOME d => "%" ^ Label.toString d
                                     | NONE => "%unreachable"
                val branches =
                    Vector.concatV 
                        (Vector.map
                             (cases, fn (w, l) => concat ["\t\t", wsToLLVM (WordX.size w), " ",
                                                          llwordx w,
                                                          ", label %", Label.toString l, "\n"]))
                val (testpre, testty, testreg) = getOperand (cxt, test)
                val loadedTestReg = nextLLVMReg ()
                val loadTest = concat ["\t", loadedTestReg, " = load ", testty, "* ", testreg, "\n"]
                val inst = concat ["\tswitch ", testty, " ", loadedTestReg, ", label ", defaultLabel,
                                   " [\n", branches, "\t]\n"]
            in
                concat [comment, testpre, loadTest, inst]
            end
    end

fun outputBlock (cxt, block) =
    let
        val Block.T {kind, label, live, raises, returns, statements, transfer} = block
        val blockLabel = Label.toString label ^ ":\n"
        val outputStatementWithCxt = fn s => outputStatement (cxt, s)
        val blockBody = Vector.concatV (Vector.map (statements, outputStatementWithCxt))
        val blockTransfer = outputTransfer (cxt, transfer, label)
    in
        concat [blockLabel, blockBody, blockTransfer, "\n"]
    end
        
fun outputChunk (cxt, chunk) =
    let
        val Context { print, indexer, ... } = cxt
        val Chunk.T {blocks, chunkLabel, regMax} = chunk
        val chunkName = ChunkLabel.toString chunkLabel
        val () = print (concat ["define i8* @",
                                ChunkLabel.toString chunkLabel,
                                "() {\nentry:\n"])
        val () = print "\t%cont = alloca %struct.cont\n"
        val () = print "\t%frontier = alloca %Pointer\n"
        val () = print "\t%l_nextFun = alloca %uintptr_t\n"
        val t1 = nextLLVMReg ()
        val () = print (concat ["\t", t1, " = load %uintptr_t* @nextFun\n"])
        val () = print (concat ["\tstore %uintptr_t ", t1, ", %uintptr_t* %l_nextFun\n"])
        val () = print "\t%stackTop = alloca %Pointer\n"
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
                                          val i = indexer label
                                      in
                                          concat ["\t\ti64 ", i, ", label %", labelName, "\n"]
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
                             concat ["@global", s, " = global [", llint (Global.numberOfType t),
                                     " x %", s, "] zeroinitializer\n@CReturn", CType.name t,
                                     " = global %", s, " zeroinitializer\n"]
                         end))
        val nonroot = concat ["@globalObjptrNonRoot = global [", llint (Global.numberOfNonRoot ()),
                              " x %Pointer] zeroinitializer\n"]
    in
        concat [globals, nonroot]
    end

fun outputDeclarations cxt =
    let
        val Context { print = print, ...} = cxt
        val globals = outputGlobals ()
        val globalDecs = declareGlobals cxt
    in
        print (concat [llvmIntrinsics, "\n", mltypes, "\n", ctypes,
                       "\n", globals, "\n", globalDecs, "\n"])
    end

fun annotate (frameLayouts, chunks) =
    let
        datatype status = None | One | Many
        val {get = labelInfo: Label.t -> {block: Block.t,
                                          chunkLabel: ChunkLabel.t,
                                          frameIndex: int option,
                                          status: status ref,
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
                                    layedOut = ref false,
                                    status = ref None})
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
        fun labelToStringIndex (l: Label.t): string = llint (labelIndex l)
                
    in
        labelToStringIndex
    end

fun output {program, outputLL} =
    let
        val Program.T { chunks, frameLayouts, ...} = program
        val { done, print, file=_ } = outputLL ()
        val indexer = annotate (frameLayouts, chunks)
        val cxt = Context { program = program, print = print, indexer = indexer }
        val () = outputDeclarations cxt
        val () = List.foreach (chunks, fn chunk => outputChunk (cxt, chunk))
        val () = print (mltonMain cxt)
        val () = print mainFunc
    in
        done ()
    end
    
end
