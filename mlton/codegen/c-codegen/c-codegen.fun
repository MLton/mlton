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

      fun callNoSemi (f: string, xs: string list): string =
         concat [f, " ", args xs]

      fun call (f, xs) =
         concat [f, " ", args xs, ";\n"]

      fun int (i: int) =
         if i >= 0
            then Int.toString i
         else concat ["-", Int.toString (~ i)]

      val bytes = int o Bytes.toInt

      fun string s =
         let val quote = "\"" (* " *)
         in concat [quote, String.escapeC s, quote]
         end

      fun word (w: Word.t) = "0x" ^ Word.toString w
   end

structure RealX =
   struct
      open RealX

      fun toCType r = CType.real (size r)
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

      fun toCType w = CType.word (size w, {signed = false})
      fun toC (w: t): string =
         concat ["(", CType.toString (toCType w), ")(",
                 toString (w, {suffix = false}), "ull)"]
   end

structure WordXVector =
   struct
      local
         structure WordX' = WordX
      in
         open WordXVector
         structure WordX = WordX'
      end

      local
         fun string v =
            concat [C.string (String.implode (toListMap (v, WordX.toChar)))]
         fun vector v =
            concat ["{",
                    String.concatWith (toListMap (v, WordX.toC), ","),
                    "}"]
      in
         fun toC (v: t): string =
            case WordSize.prim (elementSize v) of
               W8 => string v
             | _ => vector v
      end
   end

structure Const =
   struct
      local
         structure RealX' = RealX
         structure WordX' = WordX
      in
         open Const
         structure RealX = RealX'
         structure WordX = WordX'
      end

      fun toCType (c: t): CType.t =
         case c of
            Null => CType.cpointer
          | Real r => RealX.toCType r
          | Word w => WordX.toCType w
          | _ => Error.bug "CCodegen.Const.toC"

      fun toC (c: t): string =
         case c of
            Null => "NULL"
          | Real r => RealX.toC r
          | Word w => WordX.toC w
          | _ => Error.bug "CCodegen.Const.toC"
   end

structure Static =
   struct
      local
         structure Const' = Const
         structure WordX' = WordX
         structure WordXVector' = WordXVector
      in
         open Static
         structure Const = Const'
         structure WordX = WordX'
         structure WordXVector = WordXVector'
      end

      structure Data =
      struct
         open Data

         structure Elem =
            struct
               open Elem

               val toCType =
                  fn Address _ => CType.objptr
                   | Const c => Const.toCType c
               fun toC indexToC =
                  fn Address i => indexToC i
                   | Const c => Const.toC c
            end

         fun toC indexToC =
            fn Empty _ => NONE
             | Vector v => SOME (WordXVector.toC v)
             | Object es => (SOME o String.concatWith) (List.map (es, Elem.toC indexToC), ", ")
      end

      fun metadataToC (Static.T {metadata, ...}) =
         let
            val decl =
               String.concatWith
               (List.mapi (metadata, fn (i, w) =>
                           concat [CType.toString (WordX.toCType w),
                                   " meta_", C.int i]),
                "; ")
            val init =
               String.concatWith
               (List.map (metadata, WordX.toC),
                ", ")
         in
            {decl = decl, init = init}
         end
   end

structure Operand =
   struct
      open Operand

      fun isMem (z: t): bool =
         case z of
            Cast (z, _) => isMem z
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
       | CSymbol _ => true
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
       | Real_qequal _ => true
       | Real_rndToReal _ => true
       | Real_rndToWord _ => true
       | Real_round _ => true
       | Real_sub _ => true
       | Thread_returnToC => false
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
       | _ => Error.bug ("CCodegen.implementsPrim: " ^ Prim.toString p)
   end

fun outputIncludes (includes, print) =
   List.foreach (includes, fn i => (print "#include <";
                                    print i;
                                    print ">\n"))

fun declareGlobals (prefix: string, print) =
   let
      fun prints ss = List.foreach (ss, print)
      val _ =
         List.foreach
         (CType.all, fn t =>
          let
             val s = CType.toString t
             val n = Global.numberOfType t
          in
             if n > 0 orelse CType.equals (t, CType.Objptr)
                then prints [prefix, s, " global", s, " [", C.int n, "];\n"]
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
                objectTypes, reals, sourceMaps, statics, ...}),
    rest: unit -> unit
    }: unit =
   let
      fun prints ss = List.foreach (ss, print)
      fun declareExports () =
         Ffi.declareExports {print = print}
      fun declareLoadSaveGlobals () =
         let
            val _ =
               (print "static int saveGlobals (FILE *f) {\n"
                ; (List.foreach
                   (CType.all, fn t =>
                    if Global.numberOfType t > 0
                       then prints ["\tSaveArray (global",
                                    CType.toString t, ", f);\n"]
                       else ()))
                ; print "\treturn 0;\n}\n")
            val _ =
               (print "static int loadGlobals (FILE *f) {\n"
                ; (List.foreach
                   (CType.all, fn t =>
                    if Global.numberOfType t > 0
                       then prints ["\tLoadArray (global",
                                    CType.toString t, ", f);\n"]
                       else ()))
                ; print "\treturn 0;\n}\n")
         in
            ()
         end

      fun staticVar i =
         "static_" ^ Int.toString i
      fun metadataSize i =
         Bytes.toInt (Static.metadataSize (#1 (Vector.sub (statics, i))))
      fun staticAddress i = concat
         ["((Pointer)(&", staticVar i, ") + ",
          C.int (metadataSize i), ")"]
      fun declareStatics () =
         (Vector.foreachi
          (statics, fn (i, (static as Machine.Static.T {data, location, ...}, _)) =>
             let
                val dataC = Static.Data.toC staticAddress data
                datatype dataType =
                   TObject of string list
                 | TVector of string * int
                val dataType =
                   case data of
                      Static.Data.Object es =>
                         (TObject o List.map) (es, CType.toString o Static.Data.Elem.toCType)
                    | Static.Data.Vector v =>
                         TVector ("Word" ^ WordSize.toString (WordXVector.elementSize v), WordXVector.length v)
                    | Static.Data.Empty b =>
                         TVector ("Word" ^ WordSize.toString WordSize.byte, Bytes.toInt b)
                val dataDescr =
                   case dataType of
                      TObject strings => (concat o List.mapi) (strings,
                           fn (i, s) => concat [s, " data_", C.int i, "; "])
                    | TVector (str, length) => concat [str, " data[", C.int length, "];"]
                val {decl = mdecl, init = minit} =
                   Static.metadataToC static
                val qualifier =
                   let datatype z = datatype Machine.Static.Location.t in
                   case location of
                        MutStatic => ""
                      | ImmStatic =>
                           (case data of
                                (* Requires initialization, and is likely an array anyway *)
                                Machine.Static.Data.Empty _ => ""
                              | _ => "const ")
                      | Heap => "const static " (* Will just be handed to GC by address *)
                   end

                val decl = concat
                   [ qualifier, "struct {",
                     mdecl, "; ",
                     dataDescr,
                     "}\n",
                     staticVar i ]
             in
                case dataC of
                     SOME dataC =>
                       (print o concat)
                       [decl, " = {", minit, ", ", dataC, "};\n"]
                    (* needs code initialization *)
                   | NONE => print (decl ^ ";\n")
             end))
      fun declareHeapStatics () =
         (print "static struct GC_objectInit objectInits[] = {\n"
          ; (Vector.foreachi
             (statics, fn (i, (static, g)) =>
             let
                val dataBytes = Bytes.toInt (Static.dataSize static)
                val metadataBytes = Bytes.toInt (Static.metadataSize static)
             in
                case g of
                     NONE => ()
                   | SOME g' =>
                      (print o concat) ["\t{ ",
                              C.int (Global.index g'), ", ",
                              C.int metadataBytes, ", ",
                              C.int (metadataBytes + dataBytes), ", ",
                              "((Pointer) &", staticVar i, ")",
                              " },\n"]
             end))
          ; print "};\n")
      fun declareStaticInits () =
         (print "static void static_Init() {\n"
          ; (Vector.foreachi
             (statics, fn (i, (static as Machine.Static.T {data, location, ...}, _)) =>
              let
                 val shouldInit =
                    (case location of
                        Machine.Static.Location.Heap => false
                      | _ => true)
                    andalso
                    (case data of
                        Machine.Static.Data.Empty _ => true
                      | _ => false)
                 val metadataBytes = Machine.Static.metadataSize static
                 val {decl = mdecl, init = minit} =
                    Static.metadataToC static
              in
                 if shouldInit
                    then print (C.call ("\tmemcpy",
                                        ["&" ^ staticVar i,
                                         concat ["&((struct {", mdecl, "}){", minit, "})"],
                                         C.bytes metadataBytes]))
                    else ()
              end))
          ; print "};\n")

      fun declareReals () =
         (print "static void real_Init() {\n"
          ; List.foreach (reals, fn (r, g) =>
                          prints ["\tglobalReal",
                                  RealSize.toString (RealX.size r),
                                  "[", C.int (Global.index g), "] = ",
                                  RealX.toC r, ";\n"])
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
           declareArray ("const uint16_t", concat ["frameOffsets", C.int i],
                         {firstElemLen = true, oneline = true},
                         FrameOffsets.offsets fo,
                         fn (_, offset) => C.bytes offset))
          ; declareArray ("const struct GC_frameInfo", "frameInfos",
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
         declareArray ("char *", "atMLtons",
                       {firstElemLen = false, oneline = true},
                       !Control.atMLtons, fn (_, s) => C.string s)
      fun declareObjectTypes () =
         declareArray
         ("const struct GC_objectType", "objectTypes",
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
            print (C.callNoSemi (case !Control.format of
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
                                  @ additionalMainArgs))
            ; print "\n"
         end
      fun declareMain () =
         if !Control.emitMain andalso !Control.format = Control.Executable
            then print "int main (int argc, char* argv[]) { return (MLton_main (argc, argv)); }\n"
         else ()
      fun declareSourceMaps () =
         let
            fun declareProfileLabel (l, print) =
               print (C.call ("DeclareProfileLabel", [ProfileLabel.toString l]))
            fun doit (SourceMaps.T {profileLabelInfos, sourceNames, sourceSeqs, sources}) =
               (Vector.foreach (profileLabelInfos, fn {profileLabel, ...} =>
                                declareProfileLabel (profileLabel, print))
                ; declareArray ("struct GC_profileLabelInfo", "profileLabelInfos",
                                {firstElemLen = false, oneline = false},
                                profileLabelInfos, fn (_, {profileLabel, sourceSeqIndex}) =>
                                concat ["{(pointer)&", ProfileLabel.toString profileLabel, ", ",
                                        C.int sourceSeqIndex, "}"])
                ; declareArray ("const char * const", "sourceNames",
                                {firstElemLen = false, oneline = false},
                                sourceNames, fn (_, s) => C.string s)
                ; Vector.foreachi (sourceSeqs, fn (i, ss) =>
                                   declareArray ("const GC_sourceIndex", concat ["sourceSeq", C.int i],
                                                 {firstElemLen = true, oneline = true},
                                                 ss, fn (_, {sourceIndex}) => C.int sourceIndex))
                ; declareArray ("const uint32_t * const", "sourceSeqs",
                                {firstElemLen = false, oneline = false},
                                sourceSeqs, fn (i, _) => concat ["sourceSeq", Int.toString i])
                ; declareArray ("const struct GC_source", "sources",
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
      ; declareStatics (); print "\n"
      ; declareHeapStatics (); print "\n"
      ; declareStaticInits (); print "\n"
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
           datatype z = datatype CSymbolScope.t
           val _ =
              Vector.foreach
              (statements, fn s =>
               case s of
                  Statement.PrimApp {prim, ...} =>
                     (case Prim.name prim of
                         Prim.Name.CSymbol (CSymbol.T {cty, name, symbolScope}) =>
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
                 Transfer.CCall {func, ...} =>
                    let
                       datatype z = datatype CFunction.Target.t
                       val CFunction.T {target, ...} = func
                    in
                       case target of
                          Direct "Thread_returnToC" => ()
                        | Direct name =>
                             doit (name, fn () =>
                                   concat [CFunction.cPrototype func, ";\n"])
                        | Indirect => ()
                    end
               | _ => ()
        in
           ()
        end))
      ; if !empty then () else print "\n"
   end

fun output {program as Machine.Program.T {chunks, frameInfos, main, statics, ...},
            outputC: unit -> {file: File.t,
                              print: string -> unit,
                              done: unit -> unit}} =
   let
      val {get = labelInfo: Label.t -> {block: Block.t,
                                        chunkLabel: ChunkLabel.t,
                                        index: int option,
                                        marked: bool ref},
           set = setLabelInfo, ...} =
         Property.getSetOnce
         (Label.plist, Property.initRaise ("CCodeGen.labelInfo", Label.layout))
      val nextChunks = Array.new (Vector.length frameInfos, NONE)
      val _ =
         List.foreach
         (chunks, fn Chunk.T {blocks, chunkLabel, ...} =>
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
           end))
      val nextChunks = Vector.keepAllMap (Vector.fromArray nextChunks, fn lo => lo)
      val labelChunk = #chunkLabel o labelInfo
      val labelIndex = valOf o #index o labelInfo
      fun labelIndexAsString (l, {pretty}) =
         let
            val s = C.int (labelIndex l)
         in
            if pretty
               then concat ["/* ", Label.toString l, " */ ", s]
               else s
         end

      val amTimeProfiling =
         !Control.profile = Control.ProfileTimeField
         orelse !Control.profile = Control.ProfileTimeLabel

      fun declareChunk (chunkLabel, print: string -> unit) =
         (print "PRIVATE extern ChunkFn_t "
          ; print (ChunkLabel.toString chunkLabel)
          ; print ";\n")
      fun declareNextChunks (chunks, print) =
         let
            val {destroy, get} =
               Property.destGet
               (ChunkLabel.plist, Property.initFun (fn _ => ref false))
            val declareChunk = fn chunkLabel =>
               let
                  val seen = get chunkLabel
               in
                  if !seen
                     then ()
                     else (seen := true
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
                  | Transfer.Raise {raisesTo, ...} =>
                       List.foreach (raisesTo, declareChunk o labelChunk)
                  | Transfer.Return {returnsTo, ...} =>
                       List.foreach (returnsTo, declareChunk o labelChunk)
                  |  _ => ())))
            ; destroy ()
            ; print "PRIVATE extern const ChunkFnPtr_t nextChunks[];\n"
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

      fun creturnName (ct: CType.t): string = concat ["CReturn", CType.name ct]
      fun temporaryName (ct, i) =
         concat ["T", C.args [CType.name ct, Int.toString i]]
      local
         datatype z = datatype Operand.t
         fun toString (z: Operand.t): string =
            case z of
               Cast (z, ty) => concat ["(", Type.toC ty, ")", toString z]
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
             | SequenceOffset {base, index, offset, scale, ty} =>
                  concat ["X", C.args [Type.toC ty,
                                       toString base,
                                       toString index,
                                       Scale.toString scale,
                                       C.bytes offset]]
             | StackOffset s => StackOffset.toString s
             | StackTop => "StackTop"
             | Static {index, offset, ty} =>
                  concat ["M", C.args [Type.toC ty, C.int index, C.bytes offset]]
             | Temporary t =>
                  temporaryName (Type.toCType (Temporary.ty t), Temporary.index t)
             | Word w => WordX.toC w
      in
         val operandToString = toString
      end
      val chunkArgs = [Operand.GCState, Operand.StackTop, Operand.Frontier]
      fun fetchOperand (z: Operand.t): string =
         if handleMisaligned (Operand.ty z) andalso Operand.isMem z
            then fetch (operandToString z, Operand.ty z)
            else operandToString z

      fun outputChunkFn (Chunk.T {chunkLabel, blocks, tempsMax, ...}, print) =
         let
            val selfChunk = chunkLabel

            fun prints ss = List.foreach (ss, print)
            fun declareVar' (name, ty, unused, init) =
               (print "\t"
                ; if unused then print "UNUSED " else ()
                ; print ty
                ; print " "
                ; print name
                ; case init of NONE => () | SOME v => (print " = "; print v)
                ; print ";\n")
            fun declareVar (name, ct, unused, init) =
               declareVar' (name, CType.toString ct, unused, init)

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
                   | PrimApp {args, dst, prim} =>
                        let
                           fun call (): string =
                              concat
                              [Prim.toString prim, " ",
                               C.args (Vector.toListMap (args, fetchOperand))]
                           fun app (): string =
                              case Prim.name prim of
                                 Prim.Name.CSymbol (CSymbol.T {name, ...}) =>
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
                   | ProfileLabel _ => Error.bug "CCodegen.outputStatement: ProfileLabel"
               end
            local
               fun mk (dst, src) () =
                  outputStatement (Statement.Move {dst = dst, src = src})
               val stackTop = Operand.StackTop
               val gcStateStackTop = Operand.gcField GCField.StackTop
               val frontier = Operand.Frontier
               val gcStateFrontier = Operand.gcField GCField.Frontier
            in
               val cacheStackTop = mk (stackTop, gcStateStackTop)
               val flushStackTop = mk (gcStateStackTop, stackTop)
               val cacheFrontier = mk (frontier, gcStateFrontier)
               val flushFrontier = mk (gcStateFrontier, frontier)
            end
            (* StackTop += size *)
            fun adjStackTop (size: Bytes.t) =
               (outputStatement (Statement.PrimApp
                                 {args = Vector.new2
                                         (Operand.StackTop,
                                          Operand.Word
                                          (WordX.fromBytes
                                           (size,
                                            WordSize.cptrdiff ()))),
                                  dst = SOME Operand.StackTop,
                                  prim = Prim.cpointerAdd})
                ; if amTimeProfiling
                     then flushStackTop ()
                     else ())
            fun pop (fi: FrameInfo.t) =
               adjStackTop (Bytes.~ (FrameInfo.size fi))
            fun push (return: Label.t, size: Bytes.t) =
               (outputStatement (Statement.Move
                                 {dst = Operand.stackOffset
                                        {offset = Bytes.- (size, Runtime.labelSize ()),
                                         ty = Type.label return},
                                  src = Operand.Label return})
                ; adjStackTop size)
            fun copyArgs (args: Operand.t vector): string list * (unit -> unit) =
               let
                  fun usesStack z =
                     case z of
                        Operand.Cast (z, _) =>
                           (usesStack z)
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
                           val nextTmp = Counter.generator 0
                           val args =
                              Vector.toListMap
                              (args, fn arg =>
                               if usesStack arg
                                  then
                                     let
                                        val ty = Operand.ty arg
                                        val tmp = concat ["tmp", Int.toString (nextTmp ())]
                                        val _ = declareVar (tmp, Type.toCType ty, false, SOME (fetchOperand arg))
                                     in
                                        tmp
                                     end
                               else fetchOperand arg)
                        in
                           (args, fn () => print "\t}\n")
                        end
                  else (Vector.toListMap (args, fetchOperand),
                        fn () => ())
               end
            fun gotoLabel (l, {tab}) =
               prints [if tab then "\tgoto " else "goto ", Label.toString l, ";\n"]
            (* LeaveChunk(nextChunk, nextBlock)
                 if (TailCall) {
                   return nextChunk(gcState, stackTop, frontier, nextBlock);
                 } else {
                   flushFrontier();
                   flushStackTop();
                   return nextBlock;
                }
            *)
            fun leaveChunk (nextChunk, nextBlock) =
               if !Control.chunkTailCall
                  then (print "\treturn "
                        ; print (C.call (nextChunk,
                                         List.map (chunkArgs, operandToString)
                                         @ [nextBlock])))
                  else (flushFrontier ()
                        ; flushStackTop ()
                        ; print "\treturn "
                        ; print nextBlock
                        ; print ";\n")
            (* IndJump(mustReturnToSelf, mayReturnToSelf, mustReturnToOther)
                 nextBlock = *(uintptr_t* )(StackTop - sizeof(uintptr_t));
                 if (mustReturnToSelf) {
                   goto doSwitchNextBlock;
                 } else {
                   ChunkFnPtr_t nextChunk = nextChunks[nextBlock];
                   if (mayReturnToSelf && (nextChunk == selfChunk)) {
                     goto doSwitchNextBlock;
                   }
                   if (mustReturnToOther != NULL) {
                     LeaveChunk( *mustReturnToOther, nextBlock);
                   } else {
                     LeaveChunk( *nextChunk, nextBlock);
                   }
                }
            *)
            fun indJump (mustReturnToSelf, mayReturnToSelf, mustReturnToOther) =
               let
                  val _ = print "\tnextBlock = "
                  val _ = print (operandToString
                                 (Operand.stackOffset
                                  {offset = Bytes.~ (Runtime.labelSize ()),
                                   ty = Type.label (Label.newNoname ())}))
                  val _ = print ";\n"
               in
                  if mustReturnToSelf
                     then print "\tgoto doSwitchNextBlock;\n"
                     else let
                             val doNextChunk =
                                Promise.delay
                                (fn () =>
                                 print "\tnextChunk = nextChunks[nextBlock];\n")
                             val _ =
                                if mayReturnToSelf
                                   then (Promise.force doNextChunk
                                         ; print "\tif (nextChunk == &"
                                         ; print (ChunkLabel.toString selfChunk)
                                         ; print ") { goto doSwitchNextBlock; }\n")
                                   else ()
                             val _ =
                                case mustReturnToOther of
                                   NONE => (Promise.force doNextChunk; leaveChunk ("(*nextChunk)", "nextBlock"))
                                 | SOME dstChunk => leaveChunk (ChunkLabel.toString dstChunk, "nextBlock")
                          in
                             ()
                          end
               end
            fun outputTransfer t =
               let
                  datatype z = datatype Transfer.t
                  fun jump label =
                     let
                        val dstChunk = labelChunk label
                     in
                        if ChunkLabel.equals (dstChunk, selfChunk)
                           then gotoLabel (label, {tab = true})
                           else leaveChunk (ChunkLabel.toString dstChunk,
                                            labelIndexAsString (label, {pretty = true}))
                     end
                  fun rtrans rsTo =
                     let
                        val mustRToOne =
                           case rsTo of
                              [] => NONE
                            | l::rsTo =>
                                 if List.forall (rsTo, fn l' => Label.equals (l, l'))
                                    then SOME l
                                    else NONE
                        fun isSelf c = ChunkLabel.equals (selfChunk, c)
                        val rsTo =
                           List.fold
                           (rsTo, [], fn (l, cs) =>
                            let
                               val c = labelChunk l
                            in
                               if List.contains (cs, c, ChunkLabel.equals)
                                  then cs
                                  else c::cs
                            end)
                        val mayRToSelf = List.exists (rsTo, isSelf)
                        val (mustRToSelf, mustRToOther) =
                           case List.revKeepAll (rsTo, not o isSelf) of
                              [] => (true, NONE)
                            | c::rsTo =>
                                 (false,
                                  if List.forall (rsTo, fn c' => ChunkLabel.equals (c, c'))
                                     then SOME c
                                     else NONE)
                     in
                        case (!Control.chunkMustRToSingOpt, mustRToOne) of
                           (true, SOME dst) => jump dst
                         | _ =>
                              indJump (!Control.chunkMustRToSelfOpt andalso mustRToSelf,
                                       !Control.chunkMayRToSelfOpt andalso mayRToSelf,
                                       if (!Control.chunkMustRToOtherOpt andalso
                                           (!Control.chunkMayRToSelfOpt orelse not mayRToSelf))
                                          then mustRToOther
                                          else NONE)
                     end
                  val _ =
                     if !Control.codegenComments > 0
                        then (print "\t/* "
                              ; print (Layout.toString (Transfer.layout t))
                              ; print " */\n")
                        else ()
               in
                  case t of
                     CCall {func =
                            CFunction.T
                            {target =
                             CFunction.Target.Direct "Thread_returnToC", ...},
                            return = SOME {return, size = SOME size}, ...} =>
                        (push (return, size);
                         flushFrontier ();
                         flushStackTop ();
                         print "\treturn ";
                         print (C.call ("Thread_returnToC", [])))
                   | CCall {args, func, return} =>
                        let
                           val CFunction.T {return = returnTy, target, ...} = func
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
                           val _ = if CFunction.modifiesFrontier func then flushFrontier () else ()
                           val _ = if CFunction.readsStackTop func then flushStackTop () else ()
                           val _ = print "\t"
                           val _ =
                              if Type.isUnit returnTy
                                 then ()
                              else prints [creturnName (Type.toCType returnTy), " = "]
                           datatype z = datatype CFunction.Target.t
                           val _ =
                              case target of
                                 Direct name => print (C.call (name, args))
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
                                       print (C.call (name, args))
                                    end
                           val _ = afterCall ()
                           val _ =
                              if CFunction.modifiesFrontier func
                                 then cacheFrontier ()
                              else ()
                           val _ =
                              if CFunction.writesStackTop func
                                 then cacheStackTop ()
                              else ()
                           val _ =
                              if CFunction.maySwitchThreadsFrom func
                                 then indJump (false, true, NONE)
                                 else (case return of
                                          NONE => (print "\treturn "
                                                   ; print (C.call ("MLton_unreachable", [])))
                                        | SOME {return, ...} => gotoLabel (return, {tab = true}))
                        in
                           ()
                        end
                   | Call {label, return, ...} =>
                        (Option.app (return, fn {return, size, ...} => push (return, size))
                         ; jump label)
                   | Goto dst => gotoLabel (dst, {tab = true})
                   | Raise {raisesTo} =>
                        (outputStatement (Statement.PrimApp
                                          {args = Vector.new2
                                                  (Operand.gcField GCField.StackBottom,
                                                   Operand.gcField GCField.ExnStack),
                                           dst = SOME Operand.StackTop,
                                           prim = Prim.cpointerAdd})
                         ; rtrans raisesTo)
                   | Return {returnsTo} => rtrans returnsTo
                   | Switch (Switch.T {cases, default, expect, test, ...}) =>
                        let
                           val test = operandToString test
                           val test =
                              case expect of
                                 NONE => test
                               | SOME w => concat ["Expect (", test, ", ", WordX.toC w, ")"]
                           fun bnz (lnz, lz) =
                              (print "\tif ("
                               ; print test
                               ; print ") goto "
                               ; print (Label.toString lnz)
                               ; print "; else goto "
                               ; print (Label.toString lz)
                               ; print ";\n")
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
                                     NONE => print (C.call ("Unreachable", []))
                                   | SOME default => gotoLabel (default, {tab = false}))
                               ; print "\t}\n")
                        in
                           case (Vector.length cases, default) of
                              (0, NONE) => Error.bug "CCodegen.outputTransfer: Switch"
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
            val outputStatement = fn s =>
               let
                  val _ =
                     if !Control.codegenComments > 1
                        then (print "\t/* "
                              ; print (Layout.toString (Statement.layout s))
                              ; print " */\n")
                        else ()
               in
                  outputStatement s
               end
            (* Fusing of adjacent `Word<N>_<op>` and `Word{S,U}<N>_<op>CheckP`
             * primitives *does not* depend on the relative order of `!a` and `?a`
             * in /basis-library/primitive/prim1.sml:mkOverflow
             *)
            fun outputStatementsFuseOpAndChk statements =
               (ignore o Vector.foldi)
               (statements, false, fn (i, s1, skip) =>
                let
                   fun default () = (outputStatement s1; false)
                in
                   if skip then false else
                   case s1 of
                      Statement.PrimApp {args = args1, dst = SOME dst1, prim = prim1} =>
                         let
                            fun fuse chk =
                               (case Vector.sub (statements, i + 1) of
                                   s2 as Statement.PrimApp {args = args2, dst = SOME dst2, prim = prim2} =>
                                      if Vector.equals (args1, args2, Operand.equals)
                                         then (case chk prim2 of
                                                  NONE => default ()
                                                | SOME (prim, (ws, {signed})) =>
                                                     let
                                                        val name =
                                                           String.substituteFirst
                                                           (Prim.toString prim,
                                                            {substring = "CheckP",
                                                             replacement = "AndCheck"})
                                                        val _ =
                                                           if !Control.codegenComments > 1
                                                              then (print "\t/* "
                                                                    ; print (Layout.toString (Statement.layout s1))
                                                                    ; print " */\n"
                                                                    ; print "\t/* "
                                                                    ; print (Layout.toString (Statement.layout s2))
                                                                    ; print " */\n")
                                                              else ()
                                                        val _ = print "\t{\n"
                                                        val _ = print "\tWord"
                                                        val _ = print (if signed then "S" else "U")
                                                        val _ = print (WordSize.toString ws)
                                                        val _ = print " w;\n"
                                                        val _ = print "\tBool b;\n"
                                                        val _ = print "\t"
                                                        val _ =
                                                           print (C.call (name,
                                                                          Vector.toListMap (args1, fetchOperand) @
                                                                          ["&w", "&b"]))
                                                        val _ = print "\t"
                                                        val _ =
                                                           print (move {dst = operandToString dst1,
                                                                        dstIsMem = Operand.isMem dst1,
                                                                        src = "w",
                                                                        srcIsMem = false,
                                                                        ty = Operand.ty dst1})
                                                        val _ = print "\t"
                                                        val _ =
                                                           print (move {dst = operandToString dst2,
                                                                        dstIsMem = Operand.isMem dst2,
                                                                        src = "b",
                                                                        srcIsMem = false,
                                                                        ty = Operand.ty dst2})
                                                        val _ = print "\t}\n"
                                                     in
                                                        true
                                                     end)
                                         else default ()
                                 | _ => default ())
                               handle Subscript => default ()
                         in
                            case Prim.name prim1 of
                               Prim.Name.Word_add ws1 =>
                                  fuse (fn prim2 =>
                                        case Prim.name prim2 of
                                           Prim.Name.Word_addCheckP (z as (ws2, _)) =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME (prim2, z)
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Name.Word_addCheckP (z as (ws1, _)) =>
                                  fuse (fn prim2 =>
                                        case Prim.name prim2 of
                                           Prim.Name.Word_add ws2 =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME (prim1, z)
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Name.Word_mul (ws1, {signed = signed1}) =>
                                  fuse (fn prim2 =>
                                        case Prim.name prim2 of
                                           Prim.Name.Word_mulCheckP (z as (ws2, {signed = signed2})) =>
                                              if WordSize.equals (ws1, ws2)
                                                 andalso Bool.equals (signed1, signed2)
                                                 then SOME (prim2, z)
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Name.Word_mulCheckP (z as (ws1, {signed = signed1})) =>
                                  fuse (fn prim2 =>
                                        case Prim.name prim2 of
                                           Prim.Name.Word_mul (ws2, {signed = signed2}) =>
                                              if WordSize.equals (ws1, ws2)
                                                 andalso Bool.equals (signed1, signed2)
                                                 then SOME (prim1, z)
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Name.Word_neg ws1 =>
                                  fuse (fn prim2 =>
                                        case Prim.name prim2 of
                                           Prim.Name.Word_negCheckP (z as (ws2, _)) =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME (prim2, z)
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Name.Word_negCheckP (z as (ws1, _)) =>
                                  fuse (fn prim2 =>
                                        case Prim.name prim2 of
                                           Prim.Name.Word_neg ws2 =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME (prim1, z)
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Name.Word_sub ws1 =>
                                  fuse (fn prim2 =>
                                        case Prim.name prim2 of
                                           Prim.Name.Word_subCheckP (z as (ws2, _)) =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME (prim2, z)
                                                 else NONE
                                         | _ => NONE)
                             | Prim.Name.Word_subCheckP (z as (ws1, _)) =>
                                  fuse (fn prim2 =>
                                        case Prim.name prim2 of
                                           Prim.Name.Word_sub ws2 =>
                                              if WordSize.equals (ws1, ws2)
                                                 then SOME (prim1, z)
                                                 else NONE
                                         | _ => NONE)
                             | _ => default ()
                         end
                    | _ => default ()
                end)
            fun outputBlock (Block.T {kind, label, statements, transfer, ...}) =
               let
                  val _ = prints [Label.toString label, ":\n"]
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
                                   print "\t"
                                   ; (print o move)
                                     {dst = operandToString x,
                                      dstIsMem = Operand.isMem x,
                                      src = creturnName (Type.toCType ty),
                                      srcIsMem = false,
                                      ty = ty}
                                end)))
                      | Kind.Func _ => ()
                      | Kind.Handler {frameInfo, ...} => pop frameInfo
                      | Kind.Jump => ()
                  val _ =
                     if !Control.codegenFuseOpAndChk
                        then outputStatementsFuseOpAndChk statements
                        else Vector.foreach (statements, outputStatement)
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
                            | Raise _ => ()
                            | Return _ => ()
                            | Switch (Switch.T {cases, default, ...}) =>
                                 (Vector.foreach (cases, visit o #2);
                                  Option.app (default, visit)))
               end
            val entries =
               let
                  val entries = ref []
                  val _ =
                     Vector.foreach
                     (blocks, fn Block.T {kind, label, ...} =>
                      if Kind.isEntry kind
                         then (List.push (entries, (label, labelIndex label))
                               ; visit label)
                         else ())
               in
                  List.insertionSort (!entries, fn ((_, i1), (_, i2)) => i1 <= i2)
               end

            val _ = print "PRIVATE uintptr_t "
            val _ = print (C.callNoSemi (ChunkLabel.toString chunkLabel,
                                         List.map
                                         (chunkArgs, fn oper =>
                                          concat ["UNUSED ",
                                                  CType.toString (Type.toCType (Operand.ty oper)),
                                                  " ",
                                                  operandToString oper])
                                         @ ["uintptr_t nextBlock"]))
            val _ = print " {\n\n"

            val _ = declareVar' ("nextChunk", "ChunkFnPtr_t", true, NONE)
            val _ = List.foreach (CType.all, fn t => declareVar (creturnName t, t, true, NONE))
            val _ = List.foreach (CType.all, fn t =>
                                  Int.for (0, 1 + tempsMax t, fn i =>
                                           declareVar (temporaryName (t, i), t, false, NONE)))
            val _ = print "\n"
            val _ = print "doSwitchNextBlock: UNUSED;\n"
            val _ =
               if !Control.chunkJumpTable
                  then (print "\tstatic void* const nextLabels["
                        ; print (C.int (List.length entries))
                        ; print "] = {\n"
                        ; List.foreach
                          (entries, fn (label, index) =>
                           (print "\t/* "
                            ; print (C.int index)
                            ; print " */ &&"
                            ; print (Label.toString label)
                            ; print ",\n"))
                        ; print "\t};\n"
                        ; print "\tgoto *nextLabels[nextBlock - "
                        ; print (C.int (#2 (List.first entries)))
                        ; print "];\n\n")
                  else (print "\tswitch (nextBlock) {\n"
                        ; List.foreach
                          (entries, fn (label, index) =>
                           (print "\tcase "
                            ; print (C.int index)
                            ; print ": goto "
                            ; print (Label.toString label)
                            ; print ";\n"))
                        ; print "\tdefault: Unreachable();\n"
                        ; print "\t}\n\n")
            val _ = List.foreach (List.rev (!dfsBlocks), outputBlock)
            val _ = print "} /* "
            val _ = print (ChunkLabel.toString chunkLabel)
            val _ = print " */\n\n"
         in
            ()
         end

      fun declareStatics (prefix: string, print) =
         Vector.foreachi
         (statics, fn (i, (Static.T {location, ...}, _)) =>
          case location of
             Static.Location.Heap => ()
           | _ => print (concat [prefix, "PointerAux static_", C.int i, ";\n"]))

      fun outputChunks chunks =
         let
            val {done, print, ...} = outputC ()
         in
            outputIncludes (["c-chunk.h"], print); print "\n"
            ; declareGlobals ("PRIVATE extern ", print); print "\n"
            ; declareStatics ("PRIVATE extern ", print); print "\n"
            ; declareNextChunks (chunks, print); print "\n"
            ; declareFFI (chunks, print)
            ; List.foreach (chunks, fn chunk => outputChunkFn (chunk, print))
            ; done ()
         end
      val chunksWithSizes =
         List.revMap
         (chunks, fn chunk as Chunk.T {blocks, ...} =>
          (chunk,
           Vector.fold
           (blocks, 0, fn (Block.T {statements, ...}, n) =>
            n + Vector.length statements + 1)))
      fun batch (chunksWithSizes, acc, n) =
         case chunksWithSizes of
            [] => outputChunks acc
          | (chunk, s)::chunksWithSizes' =>
               let
                  val m = n + s
               in
                  if List.isEmpty acc orelse m <= !Control.chunkBatch
                     then batch (chunksWithSizes', chunk::acc, m)
                     else (outputChunks acc;
                           batch (chunksWithSizes, [], 0))
               end
      val () = batch (chunksWithSizes, [], 0)

      val {print, done, ...} = outputC ()
      fun defineNextChunks () =
         (List.foreach (chunks, fn Chunk.T {chunkLabel, ...} =>
                        declareChunk (chunkLabel, print))
          ; print "PRIVATE const ChunkFnPtr_t nextChunks["
          ; print (C.int (Vector.length nextChunks))
          ; print "] = {\n"
          ; Vector.foreachi
            (nextChunks, fn (i, label) =>
             (print "\t"
              ; print "/* "
              ; print (C.int i)
              ; print ": */ "
              ; print "/* "
              ; print (Label.toString label)
              ; print " */ &("
              ; print (ChunkLabel.toString (labelChunk label))
              ; print "),\n"))
          ; print "};\n")
      val _ =
         outputDeclarations
         {additionalMainArgs = [labelIndexAsString (#label main, {pretty = true})],
          includes = ["c-main.h"],
          program = program,
          print = print,
          rest = defineNextChunks}
      val _ = done ()
   in
      ()
   end

end
