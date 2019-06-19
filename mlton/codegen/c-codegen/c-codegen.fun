(* Copyright (C) 2009,2014-2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor CCodegen (S: C_CODEGEN_STRUCTS): C_CODEGEN =
struct

open S

open Machine

datatype z = datatype RealSize.t
datatype z = datatype WordSize.prim

local
   open Runtime
in
   structure GCField = GCField
end

structure C =
   struct
      val truee = "TRUE"
      val falsee = "FALSE"

      fun bool b = if b then truee else falsee

      fun args (ss: string list): string =
         concat ("(" :: List.separate (ss, ", ") @ [")"])

      fun callNoSemi (f: string, xs: string list, print: string -> unit): unit =
         (print f; print " "; print (args xs))

      fun call (f, xs, print) =
         (callNoSemi (f, xs, print); print ";\n")

      fun int (i: int) =
         if i >= 0
            then Int.toString i
         else concat ["-", Int.toString (~ i)]

      val bytes = int o Bytes.toInt

      fun string s =
         let val quote = "\""
         in concat [quote, String.escapeC s, quote]
         end

      fun word (w: Word.t) = "0x" ^ Word.toString w

      fun push (size: Bytes.t, print) =
         call ("\tPush", [bytes size], print)
   end

structure RealX =
   struct
      open RealX

      fun toC (r: t): string =
         let
            (* The main difference between SML reals and C floats/doubles is that
             * SML uses "~" while C uses "-".
             *)
            val s =
               String.translate (toString (r, {suffix = false}),
                                 fn #"~" => "-" | c => String.fromChar c)
            (* Also, inf is spelled INFINITY and nan is NAN in C. *)
            val s =
               case s of
                  "-inf" => "-INFINITY"
                | "inf"  => "INFINITY"
                | "nan"  => "NAN"
                | other  => other
         in
            case size r of
               R32 => concat ["(Real32)", s]
             | R64 => s
         end
   end

structure WordX =
   struct
      open WordX

      fun toC (w: t): string =
         let
            fun doit s =
               concat ["(Word", s, ")(", toString (w, {suffix = false}), "ull)"]
         in
            case WordSize.prim (size w) of
               W8 => doit "8"
             | W16 => doit "16"
             | W32 => doit "32"
             | W64 => doit "64"
         end
   end

structure WordXVector =
   struct
      local
         structure Z = WordX
      in
         open WordXVector
         structure WordX = Z
      end

      fun toC (v: t): string =
         let
            fun string () =
               concat ["(pointer)",
                       C.string (String.implode (toListMap (v, WordX.toChar)))]
            fun vector s =
               concat ["(pointer)((Word", s, "[]){",
                       String.concatWith (toListMap (v, WordX.toC), ","),
                       "})"]
         in
            case WordSize.prim (elementSize v) of
               W8 => string ()
             | W16 => vector "16"
             | W32 => vector "32"
             | W64 => vector "64"
         end
   end

structure Operand =
   struct
      open Operand

      fun isMem (z: t): bool =
         case z of
            Cast (z, _) => isMem z
          | Contents _ => true
          | Offset _ => true
          | SequenceOffset _ => true
          | StackOffset _ => true
          | _ => false
   end

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
       | Real_Math_acos _ => true
       | Real_Math_asin _ => true
       | Real_Math_atan _ => true
       | Real_Math_atan2 _ => true
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
       | Real_ldexp _ => true
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
       | Word_addCheckP _ => true
       | Word_andb _ => true
       | Word_castToReal _ => true
       | Word_equal _ => true
       | Word_extdToWord _ => true
       | Word_lshift _ => true
       | Word_lt _ => true
       | Word_mul _ => true
       | Word_mulCheckP _ => true
       | Word_neg _ => true
       | Word_negCheckP _ => true
       | Word_notb _ => true
       | Word_orb _ => true
       | Word_quot _ => true
       | Word_rem _ => true
       | Word_rndToReal _ =>
            (* Real coercions depend on rounding mode and can't be
             * inlined where gcc might constant-fold them.
             *)
            false
       | Word_rol _ => true
       | Word_ror _ => true
       | Word_rshift _ => true
       | Word_sub _ => true
       | Word_subCheckP _ => true
       | Word_xorb _ => true
       | _ => false
   end

fun outputIncludes (includes, print) =
   List.foreach (includes, fn i => (print "#include <";
                                    print i;
                                    print ">\n"))

fun declareProfileLabel (l, print) =
   C.call ("DeclareProfileLabel", [ProfileLabel.toString l], print)

fun declareGlobals (prefix: string, print) =
   let
      val _ =
         List.foreach
         (CType.all, fn t =>
          let
             val s = CType.toString t
             val n = Global.numberOfType t
          in
             if n > 0
                then print (concat [prefix, s, " global", s, " [", C.int n, "];\n"])
                else ()
          end)
   in
      ()
   end

fun outputDeclarations
   {additionalMainArgs: string list,
    includes: string list,
    print: string -> unit,
    program = (Program.T
               {frameInfos, frameOffsets, maxFrameSize,
                objectTypes, reals, sourceMaps, vectors, ...}),
    rest: unit -> unit
    }: unit =
   let
      fun declareExports () =
         Ffi.declareExports {print = print}
      fun declareLoadSaveGlobals () =
         let
            val _ =
               (print "static int saveGlobals (FILE *f) {\n"
                ; (List.foreach
                   (CType.all, fn t =>
                    if Global.numberOfType t > 0
                       then print (concat ["\tSaveArray (global",
                                           CType.toString t, ", f);\n"])
                          else ()))
                ; print "\treturn 0;\n}\n")
            val _ =
               (print "static int loadGlobals (FILE *f) {\n"
                ; (List.foreach
                   (CType.all, fn t =>
                    if Global.numberOfType t > 0
                       then print (concat ["\tLoadArray (global",
                                           CType.toString t, ", f);\n"])
                       else ()))
                ; print "\treturn 0;\n}\n")
         in
            ()
         end
      fun declareVectors () =
         (print "BeginVectorInits\n"
          ; (List.foreach
             (vectors, fn (g, v) =>
              (C.callNoSemi ("VectorInitElem",
                             [C.int (Bytes.toInt
                                     (WordSize.bytes
                                      (WordXVector.elementSize v))),
                              C.int (Global.index g),
                              C.int (WordXVector.length v),
                              WordXVector.toC v],
                             print)
                 ; print "\n")))
          ; print "EndVectorInits\n")
      fun declareReals () =
         (print "static void real_Init() {\n"
          ; List.foreach (reals, fn (g, r) =>
                          print (concat ["\tglobalReal",
                                         RealSize.toString (RealX.size r),
                                         "[", C.int (Global.index g), "] = ",
                                         RealX.toC r, ";\n"]))
          ; print "}\n")
      fun declareArray (ty: string,
                        name: string,
                        {firstElemLen: bool, oneline: bool},
                        data: 'a vector,
                        elemToString: int * 'a -> string) =
         (print "static "; print ty; print " "; print name; print "["
          ; print (C.int (if firstElemLen
                             then 1 + Vector.length data
                             else Vector.length data))
          ; print "] = {"
          ; if oneline then () else print "\n"
          ; if firstElemLen
               then (print (C.int (Vector.length data))
                     ; print ",")
               else ()
          ; Vector.foreachi
            (data, fn (i, x) =>
             (if oneline
                 then ()
                 else (print "\t /* "; print (C.int i); print ": */ ")
              ; print (elemToString (i, x)); print ","
              ; if oneline then () else print "\n"))
          ; print "};\n")
      fun declareFrameInfos () =
         (Vector.foreachi
          (frameOffsets, fn (i, fo) =>
           declareArray ("uint16_t", concat ["frameOffsets", C.int i],
                         {firstElemLen = true, oneline = true},
                         FrameOffsets.offsets fo,
                         fn (_, offset) => C.bytes offset))
          ; declareArray ("struct GC_frameInfo", "frameInfos",
                          {firstElemLen = false, oneline = false},
                          frameInfos, fn (_, fi) =>
                          concat ["{",
                                  FrameInfo.Kind.toString (FrameInfo.kind fi),
                                  ", frameOffsets", C.int (FrameOffsets.index (FrameInfo.frameOffsets fi)),
                                  ", ", C.bytes (FrameInfo.size fi),
                                  ", ", (case FrameInfo.sourceSeqIndex fi of
                                            NONE => C.int 0
                                          | SOME ssi => C.int ssi),
                                  "}"]))
      fun declareAtMLtons () =
         declareArray ("char*", "atMLtons",
                       {firstElemLen = false, oneline = true},
                       !Control.atMLtons, fn (_, s) => C.string s)
      fun declareObjectTypes () =
         declareArray
         ("struct GC_objectType", "objectTypes",
          {firstElemLen = false, oneline = false},
          objectTypes, fn (_, ty) =>
          let
             datatype z = datatype Runtime.RObjectType.t
             val (tag, hasIdentity, bytesNonObjptrs, numObjptrs) =
                case ObjectType.toRuntime ty of
                   Normal {hasIdentity, bytesNonObjptrs, numObjptrs} =>
                      ("NORMAL_TAG", hasIdentity,
                       Bytes.toInt bytesNonObjptrs, numObjptrs)
                 | Sequence {hasIdentity, bytesNonObjptrs, numObjptrs} =>
                      ("SEQUENCE_TAG", hasIdentity,
                       Bytes.toInt bytesNonObjptrs, numObjptrs)
                 | Stack =>
                      ("STACK_TAG", false, 0, 0)
                 | Weak {gone} =>
                      let
                         val bytesObjptr =
                            Bits.toBytes (Control.Target.Size.objptr ())
                         val bytesNonObjptrs =
                            let
                               val align =
                                  case !Control.align of
                                     Control.Align4 => Bytes.fromInt 4
                                   | Control.Align8 => Bytes.fromInt 8
                               val bytesMetaData =
                                  Bits.toBytes (Control.Target.Size.normalMetaData ())
                               val bytesCPointer =
                                  Bits.toBytes (Control.Target.Size.cpointer ())

                               val bytesObject =
                                  Bytes.+ (bytesMetaData,
                                  Bytes.+ (bytesCPointer,
                                           bytesObjptr))
                               val bytesTotal =
                                  Bytes.align (bytesObject, {alignment = align})
                               val bytesPad = Bytes.- (bytesTotal, bytesObject)
                            in
                               Bytes.+ (bytesPad, bytesCPointer)
                            end
                         val (bytesNonObjptrs, bytesObjptr) =
                            (Bytes.toInt bytesNonObjptrs,
                             Bytes.toInt bytesObjptr)
                         val (bytesNonObjptrs, numObjptrs) =
                            if gone
                               then (bytesNonObjptrs + bytesObjptr, 0)
                            else (bytesNonObjptrs, 1)
                      in
                         ("WEAK_TAG", false, bytesNonObjptrs, numObjptrs)
                      end
          in
             concat ["{", tag, ", ",
                     C.bool hasIdentity, ", ",
                     C.int bytesNonObjptrs, ", ",
                     C.int numObjptrs, "}"]
          end)
      fun declareMLtonMain () =
         let
            val align =
               case !Control.align of
                  Control.Align4 => 4
                | Control.Align8 => 8
            val magic =
               let
                  val version = String.hash Version.version
                  val random = Random.word ()
               in
                  Word.orb
                  (Word.<< (version, Word.fromInt (Word.wordSize - 8)),
                   Word.>> (random, Word.fromInt 8))
               end
            val profile =
               case !Control.profile of
                  Control.ProfileNone => "PROFILE_NONE"
                | Control.ProfileAlloc => "PROFILE_ALLOC"
                | Control.ProfileCallStack => "PROFILE_NONE"
                | Control.ProfileCount => "PROFILE_COUNT"
                | Control.ProfileDrop => "PROFILE_NONE"
                | Control.ProfileLabel => "PROFILE_NONE"
                | Control.ProfileTimeField => "PROFILE_TIME_FIELD"
                | Control.ProfileTimeLabel => "PROFILE_TIME_LABEL"
         in
            C.callNoSemi (case !Control.format of
                             Control.Archive => "MLtonLibrary"
                           | Control.Executable => "MLtonMain"
                           | Control.LibArchive => "MLtonLibrary"
                           | Control.Library => "MLtonLibrary",
                          [C.int align,
                           C.word magic,
                           C.bytes maxFrameSize,
                           C.bool (!Control.markCards),
                           profile,
                           C.bool (!Control.profileStack)]
                          @ additionalMainArgs,
                          print)
            ; print "\n"
         end
      fun declareMain () =
         if !Control.emitMain andalso !Control.format = Control.Executable
            then print "int main (int argc, char* argv[]) { return (MLton_main (argc, argv)); }\n"
         else ()
      fun declareSourceMaps () =
         let
            fun doit (SourceMaps.T {profileLabelInfos, sourceNames, sourceSeqs, sources}) =
               (Vector.foreach (profileLabelInfos, fn {profileLabel, ...} =>
                                declareProfileLabel (profileLabel, print))
                ; declareArray ("struct GC_profileLabelInfo", "profileLabelInfos",
                                {firstElemLen = false, oneline = false},
                                profileLabelInfos, fn (_, {profileLabel, sourceSeqIndex}) =>
                                concat ["{(pointer)&", ProfileLabel.toString profileLabel, ", ",
                                        C.int sourceSeqIndex, "}"])
                ; declareArray ("char*", "sourceNames",
                                {firstElemLen = false, oneline = false},
                                sourceNames, fn (_, s) => C.string s)
                ; Vector.foreachi (sourceSeqs, fn (i, ss) =>
                                   declareArray ("GC_sourceIndex", concat ["sourceSeq", C.int i],
                                                 {firstElemLen = true, oneline = true},
                                                 ss, fn (_, {sourceIndex}) => C.int sourceIndex))
                ; declareArray ("uint32_t*", "sourceSeqs",
                                {firstElemLen = false, oneline = false},
                                sourceSeqs, fn (i, _) => concat ["sourceSeq", Int.toString i])
                ; declareArray ("struct GC_source", "sources",
                                {firstElemLen = false, oneline = false},
                                sources, fn (_, {sourceNameIndex, successorSourceSeqIndex}) =>
                                concat ["{ ", Int.toString sourceNameIndex, ", ",
                                        Int.toString successorSourceSeqIndex, " }"]))
         in
            case sourceMaps of
               NONE => doit SourceMaps.empty
             | SOME z => doit z
         end
   in
      outputIncludes (includes, print); print "\n"
      ; declareGlobals ("PRIVATE ", print); print "\n"
      ; declareLoadSaveGlobals (); print "\n"
      ; declareVectors (); print "\n"
      ; declareReals (); print "\n"
      ; declareFrameInfos (); print "\n"
      ; declareObjectTypes (); print "\n"
      ; declareSourceMaps (); print "\n"
      ; declareAtMLtons (); print "\n"
      ; rest (); print "\n"
      ; declareMLtonMain (); declareMain (); print "\n"
      ; declareExports ()
   end

structure Type =
   struct
      open Type

      fun toC (t: t): string =
         CType.toString (Type.toCType t)
   end

structure StackOffset =
   struct
      open StackOffset

      fun toString (T {offset, ty}): string =
         concat ["S", C.args [Type.toC ty, C.bytes offset]]
   end

fun declareFFI (chunks, print) =
   let
      val empty = ref true
      val seen = String.memoize (fn _ => ref false)
      fun doit (name: string, declare: unit -> string): unit =
         let
            val r = seen name
         in
            if !r
               then ()
            else (r := true; empty := false; print (declare ()))
         end
   in
      List.foreach
      (chunks, fn Chunk.T {blocks, ...} =>
       Vector.foreach
       (blocks, fn Block.T {statements, transfer, ...} =>
        let
           datatype z = datatype CFunction.SymbolScope.t
           val _ =
              Vector.foreach
              (statements, fn s =>
               case s of
                  Statement.PrimApp {prim, ...} =>
                     (case Prim.name prim of
                         Prim.Name.FFI_Symbol {name, cty, symbolScope} =>
                            doit
                            (name, fn () =>
                             concat [case symbolScope of
                                        External => "EXTERNAL "
                                      | Private => "PRIVATE "
                                      | Public => "PUBLIC ",
                                     "extern ",
                                     case cty of
                                        SOME x => CType.toString x
                                      | NONE => "void",
                                     " ",
                                     name,
                                     ";\n"])
                       | _ => ())
                | _ => ())
           val _ =
              case transfer of
                 Transfer.CCall {func, return, ...} =>
                    let
                       datatype z = datatype CFunction.Target.t
                       val CFunction.T {target, ...} = func
                    in
                       case target of
                          Direct "Thread_returnToC" => ()
                        | Direct name =>
                             doit (name, fn () =>
                                   concat [case return of
                                              NONE => "NORETURN "
                                            | SOME _ => "",
                                           CFunction.cPrototype func, ";\n"])
                        | Indirect => ()
                    end
               | _ => ()
        in
           ()
        end))
      ; if !empty then () else print "\n"
   end

fun output {program as Machine.Program.T {chunks, frameInfos, main, ...},
            outputC: unit -> {file: File.t,
                              print: string -> unit,
                              done: unit -> unit}} =
   let
      val numChunks = List.length chunks
      val {get = chunkLabelInfo: ChunkLabel.t -> {index: int},
           set = setChunkLabelInfo, ...} =
         Property.getSetOnce
         (ChunkLabel.plist, Property.initRaise ("CCodegen.chunkLabelInfo", ChunkLabel.layout))
      val {get = labelInfo: Label.t -> {block: Block.t,
                                        chunkLabel: ChunkLabel.t,
                                        index: int option,
                                        marked: bool ref},
           set = setLabelInfo, ...} =
         Property.getSetOnce
         (Label.plist, Property.initRaise ("CCodeGen.labelInfo", Label.layout))
      val nextChunks = Array.new (Vector.length frameInfos, NONE)
      val _ =
         List.foreachi
         (chunks, fn (i, Chunk.T {blocks, chunkLabel, ...}) =>
          (setChunkLabelInfo (chunkLabel, {index = i});
           Vector.foreach
           (blocks, fn block as Block.T {kind, label, ...} =>
            let
               val index =
                  case Kind.frameInfoOpt kind of
                     NONE => NONE
                   | SOME fi =>
                        let
                           val index = FrameInfo.index fi
                        in
                           if Kind.isEntry kind
                              then Array.update (nextChunks, index, SOME label)
                              else ()
                           ; SOME index
                        end
            in
               setLabelInfo (label, {block = block,
                                     chunkLabel = chunkLabel,
                                     index = index,
                                     marked = ref false})
            end)))
      val nextChunks = Vector.keepAllMap (Vector.fromArray nextChunks, fn lo => lo)
      val labelChunk = #chunkLabel o labelInfo
      val labelIndex = #index o labelInfo
      fun labelIndexAsString (l, {pretty}) =
         let
            val s = C.int (valOf (labelIndex l))
         in
            if pretty
               then concat ["/* ", Label.toString l, " */ ", s]
               else s
         end
      val chunkLabelIndex = #index o chunkLabelInfo
      val chunkLabelIndexAsString = C.int o chunkLabelIndex

      fun declareChunk (chunkLabel, print) =
         C.call ("DeclareChunk",
                 [chunkLabelIndexAsString chunkLabel],
                 print)
      fun defineNextChunks print =
         (List.foreach (chunks, fn Chunk.T {chunkLabel, ...} =>
                        declareChunk (chunkLabel, print))
          ; print "PRIVATE uintptr_t (*nextChunks["
          ; print (C.int (Vector.length nextChunks))
          ; print "]) (CPointer, CPointer, CPointer, uintptr_t) = {\n"
          ; Vector.foreachi
            (nextChunks, fn (i, label) =>
             let
                val {chunkLabel, ...} = labelInfo label
             in
                print "\t"
                ; print "/* "
                ; print (C.int i)
                ; print ": */ "
                ; print "/* "
                ; print (Label.toString label)
                ; print " */ "
                ; C.callNoSemi ("Chunkp",
                                [chunkLabelIndexAsString chunkLabel],
                                print)
                ; print ",\n"
             end)
          ; print "};\n")
      fun declareNextChunks (chunks, print) =
         let
            val seen = Array.new (numChunks, false)
            val declareChunk = fn chunkLabel =>
               let
                  val index = chunkLabelIndex chunkLabel
               in
                  if Array.sub (seen, index)
                     then ()
                     else (Array.update (seen, index, true)
                           ; declareChunk (chunkLabel, print))
               end
         in
            List.foreach
            (chunks, fn Chunk.T {chunkLabel, blocks, ...} =>
             (declareChunk chunkLabel
              ; Vector.foreach
                (blocks, fn Block.T {transfer, ...} =>
                 case transfer of
                    Transfer.Call {label, ...} =>
                       declareChunk (labelChunk label)
                  |  _ => ())))
            ; print "PRIVATE extern uintptr_t (*nextChunks[]) (CPointer, CPointer, CPointer, uintptr_t);\n"
         end

      val handleMisaligned =
         let
            open Control
         in
            !align = Align4
            andalso (case !Control.Target.arch of
                        Target.HPPA => true
                      | Target.Sparc => true
                      | _ => false)
         end
      val handleMisaligned =
         fn ty =>
         handleMisaligned
         andalso (Type.equals (ty, Type.real R64)
                  orelse Type.equals (ty, Type.word WordSize.word64))
      fun addr z = concat ["&(", z, ")"]
      fun fetch (z, ty) =
         concat [CType.toString (Type.toCType ty),
                 "_fetch(", addr z, ")"]
      fun move' ({dst, src}, ty) =
         concat [CType.toString (Type.toCType ty),
                 "_move(", addr dst, ", ", addr src, ");\n"]
      fun store ({dst, src}, ty) =
         concat [CType.toString (Type.toCType ty),
                 "_store(", addr dst, ", ", src, ");\n"]
      fun move {dst: string, dstIsMem: bool,
                src: string, srcIsMem: bool,
                ty: Type.t}: string =
         if handleMisaligned ty
            then (case (dstIsMem, srcIsMem) of
                     (false, false) => concat [dst, " = ", src, ";\n"]
                   | (false, true) => concat [dst, " = ", fetch (src, ty), ";\n"]
                   | (true, false) => store ({dst = dst, src = src}, ty)
                   | (true, true) => move' ({dst = dst, src = src}, ty))
            else concat [dst, " = ", src, ";\n"]
      local
         datatype z = datatype Operand.t
         fun toString (z: Operand.t): string =
            case z of
               Cast (z, ty) => concat ["(", Type.toC ty, ")", toString z]
             | Contents {oper, ty} =>
                  concat ["C", C.args [Type.toC ty,
                                       toString oper]]
             | Frontier => "Frontier"
             | GCState => "GCState"
             | Global g =>
                  concat ["G", C.args [Type.toC (Global.ty g),
                                       Int.toString (Global.index g)]]
             | Label l => labelIndexAsString (l, {pretty = true})
             | Null => "NULL"
             | Offset {base, offset, ty} =>
                  concat ["O", C.args [Type.toC ty,
                                       toString base,
                                       C.bytes offset]]
             | Real r => RealX.toC r
             | Register r =>
                  concat [Type.name (Register.ty r), "_",
                          Int.toString (Register.index r)]
             | SequenceOffset {base, index, offset, scale, ty} =>
                  concat ["X", C.args [Type.toC ty,
                                       toString base,
                                       toString index,
                                       Scale.toString scale,
                                       C.bytes offset]]
             | StackOffset s => StackOffset.toString s
             | StackTop => "StackTop"
             | Word w => WordX.toC w
      in
         val operandToString = toString
      end
      fun fetchOperand (z: Operand.t): string =
         if handleMisaligned (Operand.ty z) andalso Operand.isMem z
            then fetch (operandToString z, Operand.ty z)
            else operandToString z
      fun creturn (t: Type.t): string =
         concat ["CReturn", CType.name (Type.toCType t)]

      val amTimeProfiling =
         !Control.profile = Control.ProfileTimeField
         orelse !Control.profile = Control.ProfileTimeLabel

      fun outputChunkFn (Chunk.T {chunkLabel, blocks, regMax, ...}, print) =
         let
            fun declareCReturns () =
               List.foreach
               (CType.all, fn t =>
                let
                   val s = CType.toString t
                in
                   print (concat ["\tUNUSED ", s, " CReturn", CType.name t, ";\n"])
                end)
            fun declareRegisters () =
               List.foreach
               (CType.all, fn t =>
                let
                   val pre = concat ["\t", CType.toString t, " ",
                                     CType.name t, "_"]
                in
                   Int.for (0, 1 + regMax t, fn i =>
                            print (concat [pre, C.int i, ";\n"]))
                end)
            fun pop (fi: FrameInfo.t) =
               (C.push (Bytes.~ (FrameInfo.size fi), print)
                ; if amTimeProfiling
                     then print "\tFlushStackTop();\n"
                     else ())
            fun outputStatement s =
               let
                  datatype z = datatype Statement.t
               in
                  case s of
                     Move {dst, src} =>
                        (print "\t"
                         ; print (move {dst = operandToString dst,
                                        dstIsMem = Operand.isMem dst,
                                        src = operandToString src,
                                        srcIsMem = Operand.isMem src,
                                        ty = Operand.ty dst}))
                   | Noop => ()
                   | PrimApp {args, dst, prim} =>
                        let
                           fun call (): string =
                              concat
                              [Prim.toString prim, " ",
                               C.args (Vector.toListMap (args, fetchOperand))]
                           fun app (): string =
                              case Prim.name prim of
                                 Prim.Name.FFI_Symbol {name, ...} =>
                                    concat
                                    ["((",CType.toString CType.CPointer,
                                     ")(&", name, "))"]
                               | _ => call ()
                           val _ = print "\t"
                        in
                           case dst of
                              NONE => (print (app ())
                                       ; print ";\n")
                            | SOME dst =>
                                 print (move {dst = operandToString dst,
                                              dstIsMem = Operand.isMem dst,
                                              src = app (),
                                              srcIsMem = false,
                                              ty = Operand.ty dst})
                        end
                   | ProfileLabel l =>
                        (print "\t"
                         ; C.call ("ProfileLabel", [ProfileLabel.toString l], print))
               end
            fun push (return: Label.t, size: Bytes.t) =
               (print "\t"
                ; print (move {dst = (StackOffset.toString
                                      (StackOffset.T
                                       {offset = Bytes.- (size, Runtime.labelSize ()),
                                        ty = Type.label return})),
                               dstIsMem = true,
                               src = labelIndexAsString (return, {pretty = true}),
                               srcIsMem = false,
                               ty = Type.label return})
                ; C.push (size, print)
                ; if amTimeProfiling
                     then print "\tFlushStackTop();\n"
                     else ())
            fun copyArgs (args: Operand.t vector): string list * (unit -> unit) =
               let
                  fun usesStack z =
                     case z of
                        Operand.Cast (z, _) =>
                           (usesStack z)
                      | Operand.Contents {oper, ...} =>
                           (usesStack oper)
                      | Operand.Offset {base, ...} =>
                           (usesStack base)
                      | Operand.SequenceOffset {base, index, ...} =>
                           (usesStack base) orelse (usesStack index)
                      | Operand.StackOffset _ => true
                      | _ => false
               in
                  if Vector.exists (args, usesStack)
                     then
                        let
                           val _ = print "\t{\n"
                           val c = Counter.new 0
                           val args =
                              Vector.toListMap
                              (args, fn z =>
                               if usesStack z
                                  then
                                     let
                                        val ty = Operand.ty z
                                        val tmp =
                                           concat ["tmp",
                                                   Int.toString (Counter.next c)]
                                        val _ =
                                           print
                                           (concat
                                            ["\t", Type.toC ty, " ", tmp, " = ",
                                             fetchOperand z, ";\n"])
                                     in
                                        tmp
                                     end
                               else fetchOperand z)
                        in
                           (args, fn () => print "\t}\n")
                        end
                  else (Vector.toListMap (args, fetchOperand),
                        fn () => ())
               end
            fun gotoLabel (l, {tab}) =
               print (concat [if tab then "\tgoto " else "goto ", Label.toString l, ";\n"])
            fun outputTransfer t =
               let
                  datatype z = datatype Transfer.t
               in
                  case t of
                     CCall {func =
                            CFunction.T
                            {target =
                             CFunction.Target.Direct "Thread_returnToC", ...},
                            return = SOME {return, size = SOME size}, ...} =>
                        (push (return, size);
                         print "\tFlushFrontier ();\n";
                         print "\tFlushStackTop ();\n";
                         print "\tThread_returnToC ();\n")
                   | CCall {args, func, return} =>
                        let
                           val CFunction.T {return = returnTy,
                                            target, ...} = func
                           val (args, afterCall) =
                              case return of
                                 NONE =>
                                    (Vector.toListMap (args, fetchOperand),
                                     fn () => ())
                               | SOME {size = NONE, ...} =>
                                    (Vector.toListMap (args, fetchOperand),
                                     fn () => ())
                               | SOME {return, size = SOME size} =>
                                    let
                                       val res = copyArgs args
                                       val _ = push (return, size)
                                    in
                                       res
                                    end
                           val _ =
                              if CFunction.modifiesFrontier func
                                 then print "\tFlushFrontier ();\n"
                              else ()
                           val _ =
                              if CFunction.readsStackTop func
                                 then print "\tFlushStackTop ();\n"
                              else ()
                           val _ = print "\t"
                           val _ =
                              if Type.isUnit returnTy
                                 then ()
                              else print (concat [creturn returnTy, " = "])
                           datatype z = datatype CFunction.Target.t
                           val _ =
                              case target of
                                 Direct name => C.call (name, args, print)
                               | Indirect =>
                                    let
                                       val (fptr,args) =
                                          case args of
                                             (fptr::args) => (fptr, args)
                                           | _ => Error.bug "CCodegen.outputTransfer: CCall,Indirect"
                                       val name =
                                          concat ["(*(",
                                                  CFunction.cPointerType func,
                                                  " ", fptr, "))"]
                                    in
                                       C.call (name, args, print)
                                    end
                           val _ = afterCall ()
                           val _ =
                              if CFunction.modifiesFrontier func
                                 then print "\tCacheFrontier ();\n"
                              else ()
                           val _ =
                              if CFunction.writesStackTop func
                                 then print "\tCacheStackTop ();\n"
                              else ()
                           val _ =
                              if CFunction.maySwitchThreadsFrom func
                                 then print "\tReturn();\n"
                              else (case return of
                                       NONE => print "\tUnreachable ();\n"
                                     | SOME {return, ...} => gotoLabel (return, {tab = true}))
                        in
                           ()
                        end
                   | Call {label, return, ...} =>
                        let
                           val dstChunk = labelChunk label
                           val _ =
                              case return of
                                 NONE => ()
                               | SOME {return, size, ...} =>
                                    push (return, size)
                        in
                           if ChunkLabel.equals (chunkLabel, dstChunk)
                              then C.call ("\tNearCall",
                                           [Label.toString label],
                                           print)
                              else C.call ("\tFarCall",
                                           [chunkLabelIndexAsString dstChunk,
                                            labelIndexAsString (label, {pretty = true}),
                                            C.bool (!Control.chunkTailCall)],
                                           print)
                        end
                   | Goto dst => gotoLabel (dst, {tab = true})
                   | Raise => C.call ("\tRaise", [], print)
                   | Return => C.call ("\tReturn", [], print)
                   | Switch switch =>
                        let
                           val Switch.T {cases, default, test, ...} = switch
                           val test = operandToString test
                           fun bnz (lnz, lz) =
                              C.call ("\tBNZ", [test, Label.toString lnz, Label.toString lz], print)
                           fun switch () =
                              (print "\tswitch ("
                               ; print test
                               ; print ") {\n"
                               ; Vector.foreach
                                 (cases, fn (w, l) => (print "\tcase "
                                                       ; print (WordX.toC w)
                                                       ; print ": "
                                                       ; gotoLabel (l, {tab = false})))
                               ; print "\tdefault: "
                               ; (case default of
                                     NONE => print "\tUnreachable();\n"
                                   | SOME default => gotoLabel (default, {tab = false}))
                               ; print "\t}\n")
                        in
                           case (Vector.length cases, default) of
                              (0, NONE) => Error.bug "CCodegen.outputTransfers: Switch"
                            | (0, SOME ld) => gotoLabel (ld, {tab = true})
                            | (1, NONE) => gotoLabel (#2 (Vector.sub (cases, 0)), {tab = true})
                            | (1, SOME ld) =>
                                 let
                                    val (w, l) = Vector.sub (cases, 0)
                                 in
                                    if WordX.isZero w
                                       then bnz (ld, l)
                                       else switch ()
                                 end
                            | (2, NONE) =>
                                 let
                                    val (wa, la) = Vector.sub (cases, 0)
                                    val (wb, lb) = Vector.sub (cases, 1)
                                 in
                                    if WordX.isZero wa
                                       then bnz (lb, la)
                                    else if WordX.isZero wb
                                       then bnz (la, lb)
                                    else switch ()
                                 end
                            | _ => switch ()
                        end
               end
            fun outputBlock (Block.T {kind, label, statements, transfer, ...}) =
               let
                  val _ = print (concat [Label.toString label, ":\n"])
                  val _ =
                     case kind of
                        Kind.Cont {frameInfo, ...} => pop frameInfo
                      | Kind.CReturn {dst, frameInfo, ...} =>
                           (Option.app (frameInfo, pop)
                            ; (Option.app
                               (dst, fn x =>
                                let
                                   val x = Live.toOperand x
                                   val ty = Operand.ty x
                                in
                                   print
                                   (concat
                                    ["\t",
                                     move {dst = operandToString x,
                                           dstIsMem = Operand.isMem x,
                                           src = creturn ty,
                                           srcIsMem = false,
                                           ty = ty}])
                                end)))
                      | Kind.Func _ => ()
                      | Kind.Handler {frameInfo, ...} => pop frameInfo
                      | Kind.Jump => ()
                  val _ = Vector.foreach (statements, outputStatement)
                  val _ = outputTransfer transfer
                  val _ = print "\n"
               in
                  ()
               end

            val dfsBlocks = ref []
            fun visit label =
               let
                  val {block as Block.T {transfer, ...}, marked, ...} = labelInfo label
                  datatype z = datatype Transfer.t
               in
                  if !marked
                     then ()
                     else (marked := true;
                           List.push (dfsBlocks, block);
                           case transfer of
                              CCall {return, ...} =>
                                 Option.app (return, visit o #return)
                            | Call _ => ()
                            | Goto dst => visit dst
                            | Raise => ()
                            | Return => ()
                            | Switch (Switch.T {cases, default, ...}) =>
                                 (Vector.foreach (cases, visit o #2);
                                  Option.app (default, visit)))
               end
            fun declareProfileLabels () =
               let
                  val empty = ref true
               in
                  Vector.foreach
                  (blocks, fn Block.T {statements, ...} =>
                   Vector.foreach
                   (statements, fn s =>
                    case s of
                       Statement.ProfileLabel l => (empty := false
                                                    ; declareProfileLabel (l, print))
                     | _ => ()))
                  ; if !empty then () else print "\n"
               end
         in
            declareProfileLabels ()
            ; C.callNoSemi ("Chunk", [chunkLabelIndexAsString chunkLabel], print); print "\n"
            ; declareCReturns (); print "\n"
            ; declareRegisters (); print "\n"
            ; C.callNoSemi ("ChunkSwitch", [chunkLabelIndexAsString chunkLabel], print); print "\n"
            ; Vector.foreach (blocks, fn Block.T {kind, label, ...} =>
                              if Kind.isEntry kind
                                 then (print "case "
                                       ; print (labelIndexAsString (label, {pretty = false}))
                                       ; print ": "
                                       ; gotoLabel (label, {tab = false})
                                       ; visit label)
                              else ())
            ; print "EndChunkSwitch\n\n"
            ; List.foreach (List.rev (!dfsBlocks), outputBlock)
            ; C.callNoSemi ("EndChunk", [chunkLabelIndexAsString chunkLabel, C.bool (!Control.chunkTailCall)], print); print "\n\n"
         end

      fun outputChunks chunks =
         let
            val {done, print, ...} = outputC ()
            fun outputOffsets () =
               List.foreach
               ([("ExnStackOffset", GCField.ExnStack),
                 ("FrontierOffset", GCField.Frontier),
                 ("StackBottomOffset", GCField.StackBottom),
                 ("StackTopOffset", GCField.StackTop)],
                fn (name, f) =>
                print (concat ["#define ", name, " ",
                               Bytes.toString (GCField.offset f), "\n"]))
         in
            outputIncludes (["c-chunk.h"], print); print "\n"
            ; outputOffsets (); print "\n"
            ; declareGlobals ("PRIVATE extern ", print); print "\n"
            ; declareNextChunks (chunks, print); print "\n"
            ; declareFFI (chunks, print)
            ; List.foreach (chunks, fn chunk => outputChunkFn (chunk, print))
            ; done ()
         end
      val chunks =
         List.revMap
         (chunks, fn chunk as Chunk.T {blocks, ...} =>
          (chunk,
           Vector.fold
           (blocks, 0, fn (Block.T {statements, ...}, n) =>
            n + Vector.length statements + 1)))
      fun batch (chunks, acc, n) =
         case chunks of
            [] => outputChunks acc
          | (chunk, s)::chunks' =>
               let
                  val m = n + s
               in
                  if List.isEmpty acc orelse m <= !Control.chunkBatch
                     then batch (chunks', chunk::acc, m)
                     else (outputChunks acc;
                           batch (chunks, [], 0))
               end
      val () = batch (chunks, [], 0)

      val {print, done, ...} = outputC ()
      val _ =
         outputDeclarations
         {additionalMainArgs = [labelIndexAsString (#label main, {pretty = true})],
          includes = ["c-main.h"],
          program = program,
          print = print,
          rest = fn () => defineNextChunks print}
      val _ = done ()
   in
      ()
   end

end
