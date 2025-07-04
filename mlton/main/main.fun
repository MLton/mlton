(* Copyright (C) 2010-2011,2013-2022,2024-2025 Matthew Fluet.
 * Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Main (S: MAIN_STRUCTS): MAIN =
struct

open S

structure Compile = Compile ()

structure Place =
   struct
      datatype t = Files | Generated | MLB | O | OUT | SML | SSA | SSA2 | SXML | TypeCheck | XML
      val toInt: t -> int =
         fn MLB => 1
          | SML => 1
          | Files => 2
          | TypeCheck => 4
          | XML => 6
          | SXML => 7
          | SSA => 10
          | SSA2 => 11
          | Generated => 13
          | O => 14
          | OUT => 15

      val toString =
         fn Files => "files"
          | Generated => "g"
          | MLB => "mlb"
          | O => "o"
          | OUT => "out"
          | SML => "sml"
          | SSA => "ssa"
          | SSA2 => "ssa2"
          | SXML => "sxml"
          | TypeCheck => "tc"
          | XML => "xml"

      fun compare (p, p') = Int.compare (toInt p, toInt p')
   end

structure OptPred =
   struct
      datatype t =
         Target of string
       | Yes
   end

structure Show =
   struct
      datatype t = Anns | PathMap
   end

val cc: string list ref = ref ["cc"]
val arScript: string ref = ref "<unset>"
val asOpts: {opt: string, pred: OptPred.t} list ref = ref []
val ccOpts: {opt: string, pred: OptPred.t} list ref = ref []
val mathLinkOpt: string ref = ref "-lm"
val gmpLinkOpt: string ref = ref "-lgmp"
val linkOpts: {opt: string, pred: OptPred.t} list ref = ref []
val llvm_as: string ref = ref "llvm-as"
val llvm_asOpts: {opt: string, pred: OptPred.t} list ref = ref []
val llvm_llc: string ref = ref "llc"
val llvm_llcOpts: {opt: string, pred: OptPred.t} list ref = ref []
val llvm_opt: string ref = ref "opt"
val llvm_optOpts: {opt: string, pred: OptPred.t} list ref = ref []

val debugRuntime: bool ref = ref false
val expert: bool ref = ref false
val explicitAlign: Control.align option ref = ref NONE
val explicitChunkify: Control.Chunkify.t option ref = ref NONE
datatype explicitCodegen = Native | Explicit of Control.Codegen.t
val explicitCodegen: explicitCodegen option ref = ref NONE
val explicitNativePIC: bool option ref = ref NONE
val keepGenerated = ref false
val keepO = ref false
val output: string option ref = ref NONE
val profileSet: bool ref = ref false
val runtimeArgs: string list ref = ref ["@MLton"]
val show: Show.t option ref = ref NONE
val stop = ref Place.OUT

fun parseMlbPathVar (line: String.t) =
   case String.tokens (line, Char.isSpace) of
      [var, path] => SOME {var = var, path = path}
    | _ => NONE

fun readMlbPathMap (file: File.t) =
   if not (File.canRead file) then
      Error.bug (concat ["can't read MLB path map file: ", file])
   else
      List.keepAllMap
      (File.lines file, fn line =>
       if String.forall (line, Char.isSpace)
          then NONE
       else
          case parseMlbPathVar line of
             NONE => Error.bug (concat ["strange mlb path mapping: ",
                                        file, ":: ", line])
           | SOME v => SOME v)

val targetMap: unit -> {arch: MLton.Platform.Arch.t,
                        os: MLton.Platform.OS.t,
                        target: string} list =
   Promise.lazy
   (fn () =>
    let
       val targetsDir =
          OS.Path.mkAbsolute {path = "targets", relativeTo = !Control.libDir}
       val potentialTargets = Dir.lsDirs targetsDir
       fun targetMap target =
          let
             val targetDir =
                OS.Path.mkAbsolute {path = target, relativeTo = targetsDir}
             val osFile =
                OS.Path.joinDirFile {dir = targetDir, file = "os"}
             val archFile =
                OS.Path.joinDirFile {dir = targetDir, file = "arch"}
             val os   = File.contents osFile
             val arch = File.contents archFile
             val os   = List.first (String.tokens (os,   Char.isSpace))
             val arch = List.first (String.tokens (arch, Char.isSpace))
             val os =
                case MLton.Platform.OS.fromString os of
                   NONE => Error.bug (concat ["strange os: ", os])
                 | SOME os => os
             val arch =
                case MLton.Platform.Arch.fromString arch of
                   NONE => Error.bug (concat ["strange arch: ", arch])
                 | SOME a => a
          in
             SOME {arch = arch, os = os, target = target}
          end
          handle _ => NONE
    in
       List.keepAllMap (potentialTargets, targetMap)
    end)

fun setTargetType (target: string, usage): unit =
   case List.peek (targetMap (), fn {target = t, ...} => target = t) of
      NONE => usage (concat ["invalid target: ", target])
    | SOME {arch, os, ...} =>
         let
            open Control
         in
            Target.arch := arch
            ; Target.os := os
         end

fun hasCodegen (cg, {default}) =
   let
      datatype z = datatype Control.Target.arch
      datatype z = datatype Control.Target.os
      datatype z = datatype Control.Format.t
      datatype z = datatype Control.codegen
   in
      case !Control.Target.arch of
         AMD64 => (case cg of
                      X86Codegen => false
                    | _ => true)
       | X86 => (case cg of
                    AMD64Codegen => false
                  | X86Codegen =>
                      not default
                      orelse
                      (* Darwin PIC doesn't work *)
                      !Control.Target.os <> Darwin
                  | _ => true)
       | _ => (case cg of
                  AMD64Codegen => false
                | X86Codegen => false
                | _ => true)
   end
fun hasNativeCodegen () =
   let
      datatype z = datatype Control.codegen
   in
      hasCodegen (AMD64Codegen, {default = true})
      orelse hasCodegen (X86Codegen, {default = true})
   end


fun defaultAlignIs8 () =
   let
      datatype z = datatype Control.Target.arch
   in
      case !Control.Target.arch of
         Alpha => true
       | AMD64 => true
       | ARM => true
       | ARM64 => true
       | HPPA => true
       | IA64 => true
       | MIPS => true
       | PowerPC64 => true
       | Sparc => true
       | S390 => true
       | _ => false
   end

fun makeOptions {usage} =
   let
      val usage = fn s => (ignore (usage s); raise Fail "unreachable")
      fun reportAnnotation (s, flag, e) =
         case e of
            Control.Elaborate.Bad =>
               usage (concat ["invalid -", flag, " flag: ", s])
          | Control.Elaborate.Good _ => ()
          | Control.Elaborate.Other =>
               usage (concat ["invalid -", flag, " flag: ", s])
          | Control.Elaborate.Proxy (ids, {deprecated}) =>
               if deprecated andalso !Control.warnDeprecated
                  then
                     Out.output
                     (Out.error,
                      concat ["Warning: ", "deprecated annotation: ", s, ", use ",
                              List.toString Control.Elaborate.Id.name ids, ".\n"])
               else ()
      open Control Popt
      datatype z = datatype MLton.Platform.Arch.t
      datatype z = datatype MLton.Platform.OS.t
      fun tokenizeOpt f opts =
         List.foreach (String.tokens (opts, Char.isSpace),
                       fn opt => f opt)
      fun tokenizeTargetOpt f (target, opts) =
         List.foreach (String.tokens (opts, Char.isSpace),
                       fn opt => f (target, opt))
   in
      List.map
      (
       [
       (Normal, "align", if defaultAlignIs8 () then " {8|4}" else " {4|8}",
        "object alignment",
        (SpaceString (fn s =>
                      explicitAlign
                      := SOME (case s of
                                  "4" => Align4
                                | "8" => Align8
                                | _ => usage (concat ["invalid -align flag: ",
                                                      s]))))),
       (Expert, "ar-script", " <ar>", "path to a script producing archives",
        SpaceString (fn s => arScript := s)),
       (Normal, "as-opt", " <opt>", "pass option to assembler",
        (SpaceString o tokenizeOpt)
        (fn s => List.push (asOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "as-opt-quote", " <opt>", "pass (quoted) option to assembler",
        SpaceString
        (fn s => List.push (asOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "bounce-rssa-limit", "<n>",
        "Maximum number of rssa variables to bounce around gc",
        Int (fn i => bounceRssaLimit := (if i < 0 then NONE else SOME i))),
       (Expert, "bounce-rssa-live-cutoff", "<n>",
        "Limit of bounceable variables at bounce points",
        Int (fn i => bounceRssaLiveCutoff := (if i < 0 then NONE else SOME i))),
       (Expert, "bounce-rssa-loop-size-cutoff", "<n>",
        "Largest loop size to consider",
        Int (fn i => bounceRssaLoopCutoff := (if i < 0 then NONE else SOME i))),
       (Expert, "bounce-rssa-usage-cutoff", "<n>",
        "Maximum variable use count to consider",
        Int (fn i => bounceRssaUsageCutoff := (if i < 0 then NONE else SOME i))),
       (Normal, "cc", " <cc>", "executable for C compiler",
        SpaceString
        (fn s => cc := String.tokens (s, Char.isSpace))),
       (Normal, "cc-opt", " <opt>", "pass option to C compiler",
        (SpaceString o tokenizeOpt)
        (fn s => List.push (ccOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "cc-opt-quote", " <opt>", "pass (quoted) option to C compiler",
        SpaceString
        (fn s => List.push (ccOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "chunkify", " {coalesce<n>|func|one|simple}", "set chunkify stategy",
        SpaceString
        (fn s =>
         explicitChunkify := (case Chunkify.fromString s of
                                 SOME chunkify => SOME chunkify
                               | NONE => usage (concat ["invalid -chunkify flag: ", s])))),
       (Expert, "chunk-batch", " <n>", "batch c files at size ~n",
        Int (fn n => chunkBatch := n)),
       (Expert, "chunk-jump-table", " {false|true}",
        "whether to use explicit jump table for chunk entry switch",
        Bool (fn b => chunkJumpTable := b)),
       (Expert, "chunk-may-rto-self-opt", " {true|false}",
        "whether to optimize return/raise that may transfer to self chunk",
        Bool (fn b => chunkMayRToSelfOpt := b)),
       (Expert, "chunk-must-rto-other-opt", " {true|false}",
        "whether to optimize return/raise that must transfer to one other chunk",
        Bool (fn b => chunkMustRToOtherOpt := b)),
       (Expert, "chunk-must-rto-self-opt", " {true|false}",
        "whether to optimize return/raise that must transfer to self chunk",
        Bool (fn b => chunkMustRToSelfOpt := b)),
       (Expert, "chunk-must-rto-sing-opt", " {true|false}",
        "whether to optimize return/raise that must transfer to a single label",
        Bool (fn b => chunkMustRToSingOpt := b)),
       (Expert, "chunk-tail-call", " {false|true}",
        "whether to use tail calls for interchunk transfers",
        Bool (fn b => chunkTailCall := b)),
       (Expert, "closure-convert-globalize", " {true|false}",
        "whether to globalize during closure conversion",
        Bool (fn b => (closureConvertGlobalize := b))),
       (Expert, "closure-convert-shrink", " {true|false}",
        "whether to shrink during closure conversion",
        Bool (fn b => (closureConvertShrink := b))),
       (Normal, "codegen",
        concat [" {",
                String.concatWith
                (List.keepAllMap
                 (Native :: (List.map (Control.Codegen.all, Explicit)),
                  fn cg =>
                  case cg of
                     Native => if hasNativeCodegen () then SOME "native" else NONE
                   | Explicit cg => if hasCodegen (cg, {default = true})
                                       then SOME (Control.Codegen.toString cg)
                                    else NONE),
                 "|"),
                "}"],
        "which code generator to use",
        SpaceString (fn s =>
                     explicitCodegen
                     := SOME (if s = "native"
                                 then Native
                              else (case List.peek
                                         (Control.Codegen.all, fn cg =>
                                          s = Control.Codegen.toString cg) of
                                       SOME cg => Explicit cg
                                     | NONE => usage (concat ["invalid -codegen flag: ", s]))))),
       (Expert, "codegen-comments", " <n>", "level of comments  (0)",
        intRef codegenComments),
       (Expert, "codegen-fuse-op-and-chk", " {false|true}", "fuse `op` and `opCheckP` primitives in codegen",
        boolRef codegenFuseOpAndChk),
       (Normal, "const", " '<name> <value>'", "set compile-time constant",
        SpaceString (fn s =>
                     case String.tokens (s, Char.isSpace) of
                        [name, value] =>
                           Control.setCommandLineConst {name = name,
                                                        value = value}
                      | _ => usage (concat ["invalid -const flag: ", s]))),
       (Expert, "const-prop-absval-layout-depth", " <n>",
        "cut-off depth for printing of abstract values in`ConstantPropagation`",
        intRef constPropAbsValLayoutDepth),
       (Expert, "contify-into-main", " {false|true}",
        "contify functions into main",
        boolRef contifyIntoMain),
       (Expert, "debug", " {false|true}", "produce executable with debug info",
        Bool (fn b => (debug := b
                       ; debugRuntime := b))),
       (Expert, "debug-runtime", " {false|true}", "link with debug runtime",
        boolRef debugRuntime),
       let
          val flag = "default-ann"
       in
          (Normal, flag, " <ann>", "set annotation default for mlb files",
           SpaceString
           (fn s => reportAnnotation (s, flag,
                                      Control.Elaborate.processDefault s)))
       end,
       (Normal, "default-type", " '<ty><N>'", "set default type",
        SpaceString
        (fn s => (case s of
                     "char8" => Control.defaultChar := s
                   | "int8" => Control.defaultInt := s
                   | "int16" => Control.defaultInt := s
                   | "int32" => Control.defaultInt := s
                   | "int64" => Control.defaultInt := s
                   | "intinf" => Control.defaultInt := s
                   | "real32" => Control.defaultReal := s
                   | "real64" => Control.defaultReal := s
                   | "widechar16" => Control.defaultWideChar := s
                   | "widechar32" => Control.defaultWideChar := s
                   | "word8" => Control.defaultWord := s
                   | "word16" => Control.defaultWord := s
                   | "word32" => Control.defaultWord := s
                   | "word64" => Control.defaultWord := s
                   | _ => usage (concat ["invalid -default-type flag: ", s])))),
       (Expert, "diag-pass", " <pass>", "keep diagnostic info for pass",
        SpaceString
        (fn s =>
         (case Regexp.fromString s of
             SOME (re,_) => let val re = Regexp.compileDFA re
                            in List.push (diagPasses, re)
                            end
           | NONE => usage (concat ["invalid -diag-pass flag: ", s])))),
       let
          val flag = "disable-ann"
       in
          (Normal, flag, " <ann>", "disable annotation in mlb files",
           SpaceString
           (fn s =>
            reportAnnotation (s, flag,
                              Control.Elaborate.processEnabled (s, false))))
       end,
       (Expert, "disable-pass", " <pass>", "disable optimization pass",
        SpaceString
        (fn s => (case Regexp.fromString s of
                     SOME (re,_) => let val re = Regexp.compileDFA re
                                    in List.push (executePasses, (re, false))
                                    end
                   | NONE => usage (concat ["invalid -disable-pass flag: ", s])))),
       (Expert, "drop-pass", " <pass>", "disable optimization pass",
        SpaceString
        (fn s => (if !Control.warnDeprecated
                     then Out.output
                          (Out.error,
                           "Warning: -drop-pass is deprecated.  Use -disable-pass.\n")
                     else ();
                  case Regexp.fromString s of
                     SOME (re,_) => let val re = Regexp.compileDFA re
                                    in List.push (executePasses, (re, false))
                                    end
                   | NONE => usage (concat ["invalid -disable-pass flag: ", s])))),
       let
          val flag = "enable-ann"
       in
          (Expert, flag, " <ann>", "globally enable annotation",
           SpaceString
           (fn s =>
            reportAnnotation (s, flag,
                              Control.Elaborate.processEnabled (s, true))))
       end,
       (Expert, "enable-pass", " <pass>", "enable optimization pass",
        SpaceString
        (fn s => (case Regexp.fromString s of
                     SOME (re,_) => let val re = Regexp.compileDFA re
                                    in List.push (executePasses, (re, true))
                                    end
                   | NONE => usage (concat ["invalid -enable-pass flag: ", s])))),
       (Expert, "error-threshhold", " <n>", "error threshhold (20)",
        intRef errorThreshhold),
       (Expert, "emit-main", " {true|false}", "emit main() startup function",
        boolRef emitMain),
       (Expert, "expert", " {false|true}", "enable expert status",
        boolRef expert),
       (Normal, "export-header", " <file>", "write C header file for _export's",
        SpaceString (fn s => exportHeader := SOME s)),
       (Expert, "force-handles-signals", " {false|true}", "force checks for signals",
        boolRef forceHandlesSignals),
       (Expert, "format",
        concat [" {",
                String.concatWith
                (List.keepAllMap
                  (Control.Format.all, fn cg => SOME (Control.Format.toString cg)),
                 "|"),
                "}"],
        "generated output format",
        SpaceString (fn s =>
                     Control.format
                     := (case List.peek
                              (Control.Format.all, fn cg =>
                               s = Control.Format.toString cg) of
                            SOME cg => cg
                          | NONE => usage (concat ["invalid -format flag: ", s])))),
       (Expert, "gc-check", " {limit|first|every}", "force GCs",
        SpaceString (fn s =>
                     gcCheck :=
                     (case s of
                         "limit" => Limit
                       | "first" => First
                       | "every" => Every
                       | _ => usage (concat ["invalid -gc-check flag: ", s])))),
       (Expert, "globalize-arrays", " {false|true}", "globalize arrays",
        boolRef globalizeArrays),
       (Expert, "globalize-refs", " {true|false}", "globalize refs",
        boolRef globalizeRefs),
       (Expert, "globalize-small-int-inf", " {true|false}", "globalize int-inf as small type",
        boolRef globalizeSmallIntInf),
       (Expert, "globalize-small-type", " {0|1|2|3|4|9}", "globalize small type",
        intRef globalizeSmallType),
       (Expert, "gmp-link-opt", " <opt>", "link option for GMP library",
        (SpaceString o tokenizeOpt)
        (fn s => gmpLinkOpt := s)),
       (Normal, "ieee-fp", " {false|true}", "use strict IEEE floating-point",
        boolRef Native.IEEEFP),
       (Expert, "indentation", " <n>", "indentation level in ILs",
        intRef indentation),
       (Normal, "inline", " <n>", "set inlining threshold",
        Int (fn i => inlineNonRec := {small = i,
                                      product = #product (!inlineNonRec)})),
       (Expert, "inline-into-main", " {true|false}",
        "inline functions into main",
        boolRef inlineIntoMain),
       (Expert, "inline-leafa-loops", " {true|false}", "leaf inline loops",
        Bool (fn loops =>
              case !inlineLeafA of
                 {repeat, size, ...} =>
                    inlineLeafA :=
                    {loops = loops, repeat = repeat, size = size})),
       (Expert, "inline-leafa-repeat", " {true|false}", "leaf inline repeat",
        Bool (fn repeat =>
              case !inlineLeafA of
                 {loops, size, ...} =>
                    inlineLeafA :=
                    {loops = loops, repeat = repeat, size = size})),
       (Expert, "inline-leafa-size", " <n>", "set leaf inlining threshold (20)",
        SpaceString (fn s =>
                     case !inlineLeafA of
                        {loops, repeat, ...} =>
                           inlineLeafA :=
                           {loops = loops, repeat = repeat,
                            size = (if s = "inf"
                                       then NONE
                                    else if String.forall (s, Char.isDigit)
                                       then Int.fromString s
                                    else (usage o concat)
                                         ["invalid -inline-leaf-size flag: ", s])})),
       (Expert, "inline-leafb-loops", " {true|false}", "leaf inline loops",
        Bool (fn loops =>
              case !inlineLeafB of
                 {repeat, size, ...} =>
                    inlineLeafB :=
                    {loops = loops, repeat = repeat, size = size})),
       (Expert, "inline-leafb-repeat", " {true|false}", "leaf inline repeat",
        Bool (fn repeat =>
              case !inlineLeafB of
                 {loops, size, ...} =>
                    inlineLeafB :=
                    {loops = loops, repeat = repeat, size = size})),
       (Expert, "inline-leafb-size", " <n>", "set leaf inlining threshold (40)",
        SpaceString (fn s =>
                     case !inlineLeafB of
                        {loops, repeat, ...} =>
                           inlineLeafB :=
                           {loops = loops, repeat = repeat,
                            size = (if s = "inf"
                                       then NONE
                                    else if String.forall (s, Char.isDigit)
                                       then Int.fromString s
                                    else (usage o concat)
                                         ["invalid -inline-leaf-size flag: ", s])})),
       (Expert, "inline-nonrec-product", " <n>", "set inlining threshold (320)",
        Int (fn product =>
             case !inlineNonRec of
                {small, ...} =>
                   inlineNonRec := {small = small, product = product})),
       (Expert, "inline-nonrec-small", " <n>", "set inlining threshold (60)",
        Int (fn small =>
             case !inlineNonRec of
                {product, ...} =>
                   inlineNonRec := {small = small, product = product})),
       (Normal, "keep", " {g|o}", "save intermediate files",
        SpaceString (fn s =>
                     case s of
                        "ast" => keepAST := true
                      | "core-ml" => keepCoreML := true
                      | "dot" => keepDot := true
                      | "g" => keepGenerated := true
                      | "machine" => keepMachine := true
                      | "o" => keepO := true
                      | "rssa" => keepRSSA := true
                      | "ssa" => keepSSA := true
                      | "ssa2" => keepSSA2 := true
                      | "sxml" => keepSXML := true
                      | "xml" => keepXML := true
                      | _ => usage (concat ["invalid -keep flag: ", s]))),
       (Expert, "keep-pass", " <pass>", "keep the results of pass",
        SpaceString
        (fn s => (case Regexp.fromString s of
                     SOME (re,_) => let val re = Regexp.compileDFA re
                                    in List.push (keepPasses, re)
                                    end
                   | NONE => usage (concat ["invalid -keep-pass flag: ", s])))),
       (Expert, "layout-width", " <n>", "target width for pretty printer",
        Int (fn n =>
             if n > 0
                then Layout.setDefaultWidth n
                else usage (concat ["invalid -layout-width arg: ", Int.toString n]))),
       (Expert, "libname", " <basename>", "the name of the generated library",
        SpaceString (fn s => libname := s)),
       (Expert, "limit-check-expect", " {none|false|true}", "whether to expect limit checks to trigger a collection",
        SpaceString (fn s =>
                     limitCheckExpect :=
                     (case s of
                         "false" => SOME false
                       | "none" => NONE
                       | "true" => SOME true
                       | _ => usage (concat ["invalid -limit-check-expect flag: ", s])))),
       (Normal, "link-opt", " <opt>", "pass option to linker",
        (SpaceString o tokenizeOpt)
        (fn s => List.push (linkOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "link-opt-quote", " <opt>", "pass (quoted) option to linker",
        SpaceString
        (fn s => List.push (linkOpts, {opt = s, pred = OptPred.Yes}))),
       (Normal, "llvm-as", " <llvm-as>", "executable for llvm .ll -> .bc assembler",
        SpaceString (fn s => llvm_as := s)),
       (Normal, "llvm-as-opt", " <opt>", "pass option to llvm assembler",
        (SpaceString o tokenizeOpt)
        (fn s => List.push (llvm_asOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "llvm-as-opt-quote", " <opt>", "pass (quoted) option to llvm assembler",
        SpaceString
        (fn s => List.push (llvm_asOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "llvm-aamd", " {none|tbaa}",
        "Include alias-analysis metadata when compiling with LLVM",
        SpaceString
        (fn s =>
         llvmAAMD := (case LLVMAliasAnalysisMetaData.fromString s of
                         SOME aamd => aamd
                       | NONE => usage (concat ["invalid -llvm-aamd flag: ", s])))),
       (Expert, "llvm-cc10", " {false|true}", "use llvm 'cc10' for interchunk transfers",
        boolRef llvmCC10),
       (Normal, "llvm-llc", " <llc>", "executable for llvm .bc -> .o system compiler",
        SpaceString (fn s => llvm_llc := s)),
       (Normal, "llvm-llc-opt", " <opt>", "pass option to llvm compiler",
        (SpaceString o tokenizeOpt)
        (fn s => List.push (llvm_llcOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "llvm-llc-opt-quote", " <opt>", "pass (quoted) option to llvm compiler",
        SpaceString
        (fn s => List.push (llvm_llcOpts, {opt = s, pred = OptPred.Yes}))),
       (Normal, "llvm-opt", " <opt>", "executable for llvm .bc -> .bc optimizer",
        SpaceString (fn s => llvm_opt := s)),
       (Normal, "llvm-opt-opt", " <opt>", "pass option to llvm optimizer",
        (SpaceString o tokenizeOpt)
        (fn s => List.push (llvm_optOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "llvm-opt-opt-quote", " <opt>", "pass (quoted) option to llvm optimizer",
        SpaceString
        (fn s => List.push (llvm_optOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "loop-unroll-limit", " <n>", "limit code growth by loop unrolling",
        Int
        (fn i =>
         if i >= 0
            then loopUnrollLimit := i
            else usage (concat ["invalid -loop-unroll-limit: ", Int.toString i]))),
       (Expert, "loop-unswitch-limit", " <n>", "limit code growth by loop unswitching",
        Int
        (fn i =>
          if i >= 0
            then loopUnswitchLimit := i
            else usage (concat ["invalid -loop-unswitch-limit: ", Int.toString i]))),
       (Expert, "mark-cards", " {true|false}", "mutator marks cards",
        boolRef markCards),
       (Expert, "math-link-opt", " <opt>", "link option for math library",
        (SpaceString o tokenizeOpt)
        (fn s => mathLinkOpt := s)),
       (Expert, "max-function-size", " <n>", "max function size (blocks)",
        intRef maxFunctionSize),
       (Normal, "mlb-path-map", " <file>", "additional MLB path map",
        SpaceString (fn s => mlbPathVars := !mlbPathVars @ readMlbPathMap s)),
       (Normal, "mlb-path-var", " '<name> <value>'", "additional MLB path var",
        SpaceString
        (fn s => mlbPathVars := !mlbPathVars @
                                [case parseMlbPathVar s of
                                    NONE => Error.bug ("strange mlb path var: " ^ s)
                                  | SOME v => v])),
       (Expert, "native-al-redundant", "{true|false}",
        "eliminate redundant AL ops",
        boolRef Native.elimALRedundant),
       (Expert, "native-copy-prop", " {true|false}",
        "use copy propagation",
        boolRef Native.copyProp),
       (Expert, "native-cutoff", " <n>",
        "live transfer cutoff distance",
        intRef Native.cutoff),
       (Expert, "native-live-transfer", " {0,...,8}",
        "use live transfer",
        intRef Native.liveTransfer),
       (Expert, "native-live-stack", " {false|true}",
        "track liveness of stack slots",
        boolRef Native.liveStack),
       (Expert, "native-move-hoist", " {true|false}",
        "use move hoisting",
        boolRef Native.moveHoist),
       (Expert, "native-optimize", " <n>", "level of optimizations",
        intRef Native.optimize),
       (Expert, "native-pic", " {false|true}", "generate position-independent code",
        Bool (fn b => explicitNativePIC := SOME b)),
       (Expert, "native-split", " <n>", "split assembly files at ~n lines",
        Int (fn i => Native.split := SOME i)),
       (Expert, "native-shuffle", " {true|false}",
        "shuffle registers at C-calls",
        Bool (fn b => Native.shuffle := b)),
       (Expert, "opt-fuel", " <n>", "optimization 'fuel'",
        Int (fn n => optFuel := SOME n)),
       (Expert, "opt-passes", " {default|minimal}", "level of optimizations",
        SpaceString (fn s =>
                     case Control.OptimizationPasses.setAll s of
                        Result.No s => usage (concat ["invalid -opt-passes flag: ", s])
                      | Result.Yes () => ())),
       (Normal, "output", " <file>", "name of output file",
        SpaceString (fn s => output := SOME s)),
       (Expert, "polyvariance", " {true|false}", "use polyvariance",
        Bool (fn b => if b then () else polyvariance := NONE)),
       (Expert, "polyvariance-hofo", " {true|false}", "duplicate higher-order fns only",
        Bool (fn hofo =>
              case !polyvariance of
                 SOME {product, rounds, small, ...} =>
                    polyvariance := SOME {hofo = hofo,
                                          product = product,
                                          rounds = rounds,
                                          small = small}
               | _ => ())),
       (Expert, "polyvariance-product", " <n>", "set polyvariance threshold (300)",
        Int (fn product =>
             case !polyvariance of
                SOME {hofo, rounds, small, ...} =>
                   polyvariance := SOME {hofo = hofo,
                                         product = product,
                                         rounds = rounds,
                                         small = small}
              | _ => ())),
       (Expert, "polyvariance-rounds", " <n>", "set polyvariance rounds (2)",
        Int (fn rounds =>
             case !polyvariance of
                SOME {hofo, product, small, ...} =>
                   polyvariance := SOME {hofo = hofo,
                                         product = product,
                                         rounds = rounds,
                                         small = small}
              | _ => ())),
       (Expert, "polyvariance-small", " <n>", "set polyvariance threshold (30)",
        Int (fn small =>
             case !polyvariance of
                SOME {hofo, product, rounds, ...} =>
                   polyvariance := SOME {hofo = hofo,
                                         product = product,
                                         rounds = rounds,
                                         small = small}
              | _ => ())),
       (Expert, "pi-style", " {default|npi|pic|pie}", "position-independent style",
        SpaceString (fn s =>
                     (case (s, PositionIndependentStyle.fromString s) of
                         ("default", NONE) => positionIndependentStyle := NONE
                       | (_, SOME pis) => positionIndependentStyle := SOME pis
                       | _ => usage (concat ["invalid -pi-style flag: ", s])))),
       (Expert, "prefer-abs-paths", " {false|true}",
        "prefer absolute paths when referring to files",
        boolRef preferAbsPaths),
       (Expert, "prof-pass", " <pass>", "keep profile info for pass",
        SpaceString (fn s =>
                     (case Regexp.fromString s of
                         SOME (re,_) => let val re = Regexp.compileDFA re
                                        in
                                           List.push (profPasses, re)
                                        end
                       | NONE => usage (concat ["invalid -diag-pass flag: ", s])))),
       (Normal, "profile", " {no|alloc|count|time}",
        "produce executable suitable for profiling",
        SpaceString
        (fn s =>
         if !profileSet
            then usage "can't have multiple -profile switches"
         else
            (profileSet := true
             ; profile := (case s of
                              "no" => ProfileNone
                            | "alloc" => ProfileAlloc
                            | "call" => ProfileCallStack
                            | "count" => ProfileCount
                            | "drop" => ProfileDrop
                            | "time" => ProfileTime
                            | _ => usage (concat
                                          ["invalid -profile arg: ", s]))))),
       (Expert, "profile-block", " {false|true}",
        "profile IL blocks in addition to IL functions",
        boolRef profileBlock),
       (Normal, "profile-branch", " {false|true}",
        "profile branches in addition to functions",
        boolRef profileBranch),
       (Expert, "profile-c", " <regexp>",
        "include C-calls in files matching <regexp> in profile",
        SpaceString
        (fn s =>
         (case Regexp.fromString s of
             SOME (re,_) => let
                               open Regexp
                               val re = seq [anys, re, anys]
                               val re = compileDFA re
                            in List.push (profileC, re)
                            end
           | NONE => usage (concat ["invalid -profile-c flag: ", s])))),
       (Expert, "profile-exclude", " <regexp>",
        "exclude files matching <regexp> from profile",
        SpaceString
        (fn s =>
         (case Regexp.fromString s of
             SOME (re,_) => let
                               open Regexp
                               val re = seq [anys, re, anys]
                               val re = compileDFA re
                            in List.push (profileInclExcl, (re, false))
                            end
           | NONE => usage (concat ["invalid -profile-exclude flag: ", s])))),
       (Expert, "profile-il", " {source}", "where to insert profile exps",
        SpaceString
        (fn s =>
         case s of
            "source" => profileIL := ProfileSource
          | "ssa" => profileIL := ProfileSSA
          | "ssa2" => profileIL := ProfileSSA2
          | _ => usage (concat ["invalid -profile-il arg: ", s]))),
       (Expert, "profile-include", " <regexp>",
        "include files matching <regexp> from profile",
        SpaceString
        (fn s =>
         (case Regexp.fromString s of
             SOME (re,_) => let
                               open Regexp
                               val re = seq [anys, re, anys]
                               val re = compileDFA re
                            in List.push (profileInclExcl, (re, true))
                            end
           | NONE => usage (concat ["invalid -profile-include flag: ", s])))),
       (Expert, "profile-raise", " {false|true}",
        "profile raises in addition to functions",
        boolRef profileRaise),
       (Normal, "profile-stack", " {false|true}", "profile the stack",
        boolRef profileStack),
       (Expert, "profile-tail-call-opt", " {always|self-only|never}", "perform tail call optimization when profiling",
        SpaceString
        (fn s =>
         (case ProfileTailCallOpt.fromString s of
             SOME ptco => profileTailCallOpt := ptco
           | NONE => usage (concat ["invalid -profile-tail-call-opt flag: ", s])))),
       (Normal, "profile-val", " {false|true}",
        "profile val bindings in addition to functions",
        boolRef profileVal),
       (Normal, "runtime", " <arg>", "pass arg to runtime via @MLton",
        SpaceString (fn s => List.push (runtimeArgs, s))),
       (Expert, "seed-rand", " <w>", "seed the pseudo-random number generator",
        Word Random.srand),
       (Expert, "show", " {anns|path-map}", "print specified data and stop",
        SpaceString
        (fn s =>
         show := SOME (case s of
                          "anns" => Show.Anns
                        | "path-map" => Show.PathMap
                        | _ => usage (concat ["invalid -show arg: ", s])))),
       (Normal, "show-basis", " <file>", "write final basis environment",
        SpaceString (fn s => showBasis := SOME s)),
       (Expert, "show-basis-compact", " {false|true}", "show basis environment in compact form",
        boolRef showBasisCompact),
       (Expert, "show-basis-def", " {true|false}", "show basis environment with definition source position",
        boolRef showBasisDef),
       (Expert, "show-basis-flat", " {true|false}", "show basis environment with long identifier names",
        boolRef showBasisFlat),
       (Normal, "show-def-use", " <file>", "write def-use information",
        SpaceString (fn s => showDefUse := SOME s)),
       (Expert, "signal-check", " {if-handles-signals|always}", "when to insert signal checks",
        SpaceString (fn s =>
                     signalCheck :=
                     (case s of
                         "always" => SignalCheck.Always
                       | "if-handles-signals" => SignalCheck.IfHandlesSignals
                       | _ => usage (concat ["invalid -signal-check flag: ", s])))),
       (Expert, "signal-check-at-limit-check", " {true|false}", "whether to force a signal check at a limit check",
        boolRef signalCheckAtLimitCheck),
       (Expert, "signal-check-expect", " {none|false|true}", "whether to expect signal checks to trigger a collection",
        SpaceString (fn s =>
                     signalCheckExpect :=
                     (case s of
                         "false" => SOME false
                       | "none" => NONE
                       | "true" => SOME true
                       | _ => usage (concat ["invalid -signal-check-expect flag: ", s])))),
       (Expert, "show-types", " {true|false}", "show types in ILs",
        boolRef showTypes),
       (Expert, "split-types-bool", " {smart|always|never}",
        "bool type splitting method",
        SpaceString (fn s =>
                     splitTypesBool :=
                     (case s of
                         "always" => Always
                       | "never" => Never
                       | "smart" => Smart
                       | _ => usage (concat ["invalid -split-types-bool flag: ", s])))),
       (Expert, "ssa-passes", " <passes>", "ssa optimization passes",
        SpaceString
        (fn s =>
         case Control.OptimizationPasses.set {il = "ssa", passes = s} of
            Result.Yes () => ()
          | Result.No s => usage (concat ["invalid -ssa-passes arg: ", s]))),
       (Expert, "ssa2-passes", " <passes>", "ssa2 optimization passes",
        SpaceString
        (fn s =>
         case Control.OptimizationPasses.set {il = "ssa2", passes = s} of
            Result.Yes () => ()
          | Result.No s => usage (concat ["invalid -ssa2-passes arg: ", s]))),
       (Expert, "stack-check-expect", " {none|false|true}", "whether to expect stack checks to trigger a collection",
        SpaceString (fn s =>
                     stackCheckExpect :=
                     (case s of
                         "false" => SOME false
                       | "none" => NONE
                       | "true" => SOME true
                       | _ => usage (concat ["invalid -stack-check-expect flag: ", s])))),
       (Normal, "stop", " {f|g|o|tc}", "when to stop",
        SpaceString
        (fn s =>
         stop := (case s of
                     "f" => Place.Files
                   | "g" => Place.Generated
                   | "o" => Place.O
                   | "tc" => Place.TypeCheck
                   | _ => usage (concat ["invalid -stop arg: ", s])))),
       (Expert, "stop-pass", " <pass>", "stop compilation after pass",
        SpaceString
        (fn s => (case Regexp.fromString s of
                     SOME (re,_) => let val re = Regexp.compileDFA re
                                    in List.push (stopPasses, re)
                                    end
                   | NONE => usage (concat ["invalid -stop-pass flag: ", s])))),
       (Expert, "sxml-passes", " <passes>", "sxml optimization passes",
        SpaceString
        (fn s =>
         case Control.OptimizationPasses.set {il = "sxml", passes = s} of
            Result.Yes () => ()
          | Result.No s => usage (concat ["invalid -sxml-passes arg: ", s]))),
       (Normal, "target",
        concat [" {",
                (case targetMap () of
                    [] => ""
                  | [x] => #target x
                  | x :: _ => concat [#target x, "|..."]),
                "}"],
        "platform that executable will run on",
        SpaceString
        (fn t =>
         (target := (if t = "self" then Self else Cross t);
          setTargetType (t, usage)))),
       (Normal, "target-as-opt", " <target> <opt>", "target-dependent assembler option",
        (SpaceString2 o tokenizeTargetOpt)
        (fn (target, opt) =>
         List.push (asOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-as-opt-quote", " <target> <opt>", "target-dependent assembler option (quoted)",
        (SpaceString2
         (fn (target, opt) =>
          List.push (asOpts, {opt = opt, pred = OptPred.Target target})))),
       (Normal, "target-cc-opt", " <target> <opt>", "target-dependent C compiler option",
        (SpaceString2 o tokenizeTargetOpt)
        (fn (target, opt) =>
         List.push (ccOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-cc-opt-quote", " <target> <opt>", "target-dependent C compiler option (quoted)",
        (SpaceString2
         (fn (target, opt) =>
          List.push (ccOpts, {opt = opt, pred = OptPred.Target target})))),
       (Normal, "target-link-opt", " <target> <opt>", "target-dependent linker option",
        (SpaceString2 o tokenizeTargetOpt)
        (fn (target, opt) =>
         List.push (linkOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-link-opt-quote", " <target> <opt>", "target-dependent linker option (quoted)",
        (SpaceString2
         (fn (target, opt) =>
          List.push (linkOpts, {opt = opt, pred = OptPred.Target target})))),
       (Expert, "target-llvm-as-opt", " <target> <opt>", "target-dependent llvm assembler option",
        (SpaceString2 o tokenizeTargetOpt)
        (fn (target, opt) => List.push (llvm_asOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-llvm-as-opt-quote", " <target> <opt>", "target-dependent llvm assembler option (quoted)",
        SpaceString2
        (fn (target, opt) => List.push (llvm_asOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-llvm-llc-opt", " <target> <opt>", "target-dependent llvm compiler option",
        (SpaceString2 o tokenizeTargetOpt)
        (fn (target, opt) => List.push (llvm_llcOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-llvm-llc-opt-quote", " <target> <opt>", "target-dependent llvm compiler option (quoted)",
        SpaceString2
        (fn (target, opt) => List.push (llvm_llcOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-llvm-opt-opt", " <target> <opt>", "target-dependent llvm optimizer option",
        (SpaceString2 o tokenizeTargetOpt)
        (fn (target, opt) => List.push (llvm_optOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, "target-llvm-opt-opt-quote", " <target> <opt>", "target-dependent llvm optimizer option (quoted)",
        SpaceString2
        (fn (target, opt) => List.push (llvm_optOpts, {opt = opt, pred = OptPred.Target target}))),
       (Expert, #1 trace, " name1,...", "trace compiler internals", #2 trace),
       (Expert, "type-check", " {false|true}", "type check ILs",
        Bool
        (fn b =>
         let
            val re = Regexp.seq [Regexp.anys, Regexp.string ":typeCheck"]
            val re = Regexp.compileDFA re
         in
            List.push (executePasses, (re, b))
         end)),
       (Normal, "verbose", " {0|1|2|3}", "how verbose to be",
        SpaceString
        (fn s =>
         verbosity := (case s of
                          "0" => Silent
                        | "1" => Top
                        | "2" => Pass
                        | "3" => Detail
                        | _ => usage (concat ["invalid -verbose arg: ", s])))),
       (Expert, "warn-ann", " {true|false}",
        "unrecognized annotation warnings",
        boolRef warnAnn),
       (Expert, "warn-deprecated", " {true|false}",
        "deprecated feature warnings",
        boolRef warnDeprecated),
       (Expert, "xml-passes", " <passes>", "xml optimization passes",
        SpaceString
        (fn s =>
         case Control.OptimizationPasses.set {il = "xml", passes = s} of
            Result.Yes () => ()
          | Result.No s => usage (concat ["invalid -xml-passes arg: ", s]))),
       (Expert, "zone-cut-depth", " <n>", "zone cut depth",
        intRef zoneCutDepth)
       ],
       fn (style, name, arg, desc, opt) =>
       {arg = arg, desc = desc, name = name, opt = opt, style = style})
   end

val mainUsage =
   "mlton [option ...] file.{c|mlb|o|sml} [file.{c|o|s|S} ...]"

val {parse, usage} =
   Popt.makeUsage {mainUsage = mainUsage,
                   makeOptions = makeOptions,
                   showExpert = fn () => !expert}

val usage = fn s => (usage s; raise Fail "unreachable")

fun commandLine (_: string, args: string list): unit =
   let
      open Control
      datatype z = datatype MLton.Platform.Arch.t
      datatype z = datatype MLton.Platform.OS.t
      val args =
         case args of
            lib :: args =>
               (libDir := OS.Path.mkCanonical lib
                ; args)
          | _ => Error.bug "incorrect args from shell script"
      val () = setTargetType ("self", usage)
      val result = parse args

      val target = !target
      val targetStr =
         case target of
            Cross s => s
          | Self => "self"
      val targetsDir =
         OS.Path.mkAbsolute {path = "targets", relativeTo = !libDir}
      val targetDir =
         OS.Path.mkAbsolute {path = targetStr, relativeTo = targetsDir}
      val () = Control.libTargetDir := targetDir
      val targetIncDir =
         OS.Path.mkAbsolute {path = "include", relativeTo = targetDir}
      val targetLibDir = targetDir
      val targetArch = !Target.arch
      val targetArchStr = String.toLower (MLton.Platform.Arch.toString targetArch)
      val targetOS = !Target.os
      val targetOSStr = String.toLower (MLton.Platform.OS.toString targetOS)
      val targetArchOSStr = concat [targetArchStr, "-", targetOSStr]

      val pisTargetDefault =
         let
            fun get s =
               Control.StrMap.lookupIntInf (Promise.force Control.Target.consts, "default::" ^ s)
         in
            if get "pie" > 0
               then Control.PositionIndependentStyle.PIE
            else if get "pic" > 0
               then Control.PositionIndependentStyle.PIC
            else Control.PositionIndependentStyle.NPI
         end
      val pisFormat =
         case (targetOS, targetArch, !format) of
            (MinGW, _, _) => NONE
          | (Cygwin, _, _) => NONE
          | (_, _, Executable) => NONE
          | (_, _, Archive) => NONE
          | (_, _, Library) => SOME Control.PositionIndependentStyle.PIC
          | (_, _, LibArchive) => SOME Control.PositionIndependentStyle.PIC
      val () =
         positionIndependentStyle
         := (case (!positionIndependentStyle, pisFormat) of
                (SOME pis, _) => SOME pis
              | (NONE, NONE) => NONE
              | (NONE, SOME pisFormat) => SOME pisFormat)
      val positionIndependentStyle = !positionIndependentStyle

      val () =
         Native.pic := (case !explicitNativePIC of
                           NONE =>
                              let
                                 val pis =
                                    case positionIndependentStyle of
                                       NONE => pisTargetDefault
                                     | SOME pis => pis
                              in
                                 case pis of
                                    Control.PositionIndependentStyle.NPI => false
                                  | Control.PositionIndependentStyle.PIC => true
                                  | Control.PositionIndependentStyle.PIE => true
                              end
                         | SOME b => b)

      val stop = !stop

      val () =
         align := (case !explicitAlign of
                      NONE => if defaultAlignIs8 () then Align8 else Align4
                    | SOME a => a)
      val () =
         codegen := (case !explicitCodegen of
                        NONE =>
                           if hasCodegen (AMD64Codegen, {default = true})
                              then AMD64Codegen
                           else if hasCodegen (X86Codegen, {default = true})
                              then X86Codegen
                           else CCodegen
                      | SOME Native =>
                           if hasCodegen (AMD64Codegen, {default = false})
                              then AMD64Codegen
                           else if hasCodegen (X86Codegen, {default = false})
                              then X86Codegen
                           else usage (concat ["can't use native codegen on ",
                                               MLton.Platform.Arch.toString targetArch,
                                               " target"])
                      | SOME (Explicit cg) => cg)
      val () = MLton.Rusage.measureGC (!verbosity <> Silent)
      val () = if !exnHistory
                  then (case !profile of
                           ProfileNone => profile := ProfileCallStack
                         | ProfileCallStack => ()
                         | _ => usage "can't use -profile with Exn.keepHistory"
                        ; profileRaise := true)
               else ()
      val () = if !profileStack
                  then (case !profile of
                           ProfileAlloc => ()
                         | ProfileCount => ()
                         | ProfileTime => ()
                         | _ => usage "can't use '-profile-stack true' without '-profile {alloc,count,time}'")
                  else ()

      val () =
         Control.setCommandLineConst
         {name = "CallStack.keep",
          value = Bool.toString (!Control.profile = Control.ProfileCallStack)}

      fun tokenize l =
         String.tokens (concat (List.separate (l, " ")), Char.isSpace)

      (* When cross-compiling, use the named cross compiler.
       * Older gcc versions used -b for multiple targets.
       * If this is still needed, a shell script wrapper can hide this.
       *)
      val cc =
         case target of
            Cross s =>
               let
                  val {dir = ccDir, file = ccFile} =
                     OS.Path.splitDirFile (hd (!cc))
               in
                  OS.Path.joinDirFile
                  {dir = ccDir,
                   file = s ^ "-" ^ ccFile}
                  ::
                  tl (!cc)
               end
          | Self => !cc
      val arScript = !arScript

      local
         fun addMD s =
            if !debugRuntime then s ^ "-dbg" else s
         fun addPI s =
            s ^ (Control.PositionIndependentStyle.toSuffix positionIndependentStyle)
      in
         val mkLibName = addPI o addMD
      end
      fun addTargetOpts opts =
         List.fold
         (!opts, [], fn ({opt, pred}, ac) =>
          if (case pred of
                 OptPred.Target s =>
                    let
                       val s = String.toLower s
                    in
                       s = targetArchOSStr
                       orelse s = targetArchStr
                       orelse s = targetOSStr
                    end
               | OptPred.Yes => true)
             then opt :: ac
          else ac)
      val asOpts = addTargetOpts asOpts
      val ccOpts = addTargetOpts ccOpts
      val linkOpts = addTargetOpts linkOpts
      val linkOpts =
         List.map (["mlton", "gdtoa"], fn lib => "-l" ^ mkLibName lib)
         @ [!mathLinkOpt, !gmpLinkOpt]
         @ linkOpts
      val linkOpts = ("-L" ^ targetLibDir) :: linkOpts

      val linkArchives =
         List.map (["mlton", "gdtoa"], fn lib =>
                   OS.Path.joinDirFile {dir = targetLibDir,
                                        file = "lib" ^ mkLibName lib ^ ".a"})

      val llvm_as = !llvm_as
      val llvm_llc = !llvm_llc
      val llvm_opt = !llvm_opt
      val llvm_asOpts = addTargetOpts llvm_asOpts
      val llvm_llcOpts = addTargetOpts llvm_llcOpts
      val llvm_optOpts = addTargetOpts llvm_optOpts

      val _ =
         if not (hasCodegen (!codegen, {default = false}))
            then usage (concat ["can't use ",
                                Control.Codegen.toString (!codegen),
                                " codegen on ",
                                MLton.Platform.Arch.toString targetArch,
                                " target"])
         else ()
      val () =
         Control.labelsHaveExtra_ := (case (targetOS, targetArch) of
                                         (Cygwin, X86) => true
                                       | (Darwin, _) => true
                                       | (MinGW, X86) => true
                                       | _ => false)
      val _ =
         chunkify :=
         (case !explicitChunkify of
             NONE => (case !codegen of
                         AMD64Codegen => Chunkify.Func
                       | CCodegen => Chunkify.Coalesce {limit = 4096}
                       | LLVMCodegen => Chunkify.Coalesce {limit = 4096}
                       | X86Codegen => Chunkify.Func
                       )
           | SOME c => c)
      val _ = if not (!Control.codegen = X86Codegen) andalso !Native.IEEEFP
                 then usage "must use x86 codegen with -ieee-fp true"
              else ()
      val _ =
         if !keepDot andalso List.isEmpty (!keepPasses)
            then keepSSA := true
         else ()
      val () =
         keepDefUse
         := (isSome (!showDefUse)
             orelse (Control.Elaborate.enabled Control.Elaborate.warnUnused)
             orelse (Control.Elaborate.default Control.Elaborate.warnUnused))
      val _ =
         if !profile = ProfileTime
            then if (case targetOS of
                        AIX => false
                      | Cygwin => false
                      | Darwin => true
                      | FreeBSD => true
                      | HPUX => true
                      | Hurd => false
                      | Linux => true
                      | MinGW => true
                      | NetBSD => true
                      | OpenBSD => true
                      | Solaris => true
                      | WASI => false)
                    then ()
                    else usage (concat ["can't use -profile time on ",
                                        MLton.Platform.OS.toString targetOS])
            else ()
      val () =
         case !show of
            NONE => ()
          | SOME info =>
            (case info of
                Show.Anns =>
                Layout.outputl (Control.Elaborate.document {expert = !expert},
                                Out.standard)
              | Show.PathMap =>
                let
                   open Layout
                in
                   outputl (align
                            (List.revMap (Control.mlbPathMap (),
                                          fn {var, path, ...} =>
                                          str (concat [var, " ", path]))),
                            Out.standard)
                end
             ; let open OS.Process in exit success end)
   in
      case result of
      Result.No msg => usage msg
    | Result.Yes [] =>
         (inputFile := "<none>"
          ; Out.outputl (Out.standard, Version.banner)
          ; if Verbosity.< (!verbosity, Detail)
               then ()
               else Layout.outputl (Control.layout (), Out.standard))
    | Result.Yes (input :: rest) =>
         let
            val _ = inputFile := File.base (File.fileOf input)
            val (start, base) =
               let
                  val rec loop =
                     fn [] => usage (concat ["invalid file suffix on ", input])
                      | (suf, start, hasNum) :: sufs =>
                           if String.hasSuffix (input, {suffix = suf})
                              then (start,
                                    let
                                       val f = File.base input
                                    in
                                       if hasNum
                                          then File.base f
                                       else f
                                    end)
                           else loop sufs
                  datatype z = datatype Place.t
               in
                  loop [(".mlb", MLB, false),
                        (".sml", SML, false),
                        (".xml", XML, false),
                        (".sxml", SXML, false),
                        (".ssa", SSA, false),
                        (".ssa2", SSA2, false),
                        (".c", Generated, true),
                        (".o", O, true)]
               end
            val _ =
               List.foreach
               (rest, fn f =>
                if List.exists ([".c", ".o", ".s", ".S"], fn suffix =>
                                String.hasSuffix (f, {suffix = suffix}))
                   then File.withIn (f, fn _ => ())
                else usage (concat ["invalid file suffix: ", f]))
            val csoFiles = rest
         in
            case Place.compare (start, stop) of
               GREATER => usage (concat ["cannot go from ", Place.toString start,
                                         " to ", Place.toString stop])
             | EQUAL => usage "nothing to do"
             | LESS =>
                  let
                     val tempFiles: File.t list ref = ref []
                     val tmpDir =
                        let
                           val (tmpVar, default) =
                              case MLton.Platform.OS.host of
                                 MinGW => ("TEMP", "C:/WINDOWS/TEMP")
                               | _ => ("TMPDIR", "/tmp")
                        in
                           case OS.Process.getEnv tmpVar of
                              NONE => default
                            | SOME d => d
                        end
                     fun temp (suf: string): File.t =
                        let
                           val (f, out) =
                              File.temp {prefix = OS.Path.concat (tmpDir, "file"),
                                         suffix = suf}
                           val _ = Out.close out
                           val _ = List.push (tempFiles, f)
                        in
                           f
                        end
                     fun suffix s = concat [base, s]
                     fun maybeOut suf =
                        case !output of
                           NONE => suffix suf
                         | SOME f => f
                     fun maybeOutBase suf =
                        case !output of
                           NONE => suffix suf
                         | SOME f => if File.extension f = SOME "exe"
                                        then concat [File.base f, suf]
                                     else concat [f, suf]
                     val {base = outputBase, ...} =
                        OS.Path.splitBaseExt (maybeOut ".ext")
                     val {file = defLibname, ...} =
                        OS.Path.splitDirFile outputBase
                     val defLibname =
                        if String.hasPrefix (defLibname, {prefix = "lib"})
                        then String.extract (defLibname, 3, NONE)
                        else defLibname
                     fun toAlNum c = if Char.isAlphaNum c then c else #"_"
                     val () =
                        if !libname <> "" then () else
                        libname := CharVector.map toAlNum defLibname
                     (* Library output includes a header by default *)
                     val () =
                        case (!format, !exportHeader) of
                           (Executable, _) => ()
                         | (_, NONE) => exportHeader := SOME (!libname ^ ".h")
                         | _ => ()
                     val _ =
                        atMLtons :=
                        Vector.fromList
                        (tokenize (rev ("--" :: (!runtimeArgs))))
                     fun compileO (inputs: File.t list): unit =
                        let
                           val output =
                              case (!format, targetOS) of
                                 (Archive, _) => maybeOut ".a"
                               | (Executable, _) => maybeOut ""
                               | (LibArchive, _) => maybeOut ".a"
                               | (Library, Darwin) => maybeOut ".dylib"
                               | (Library, Cygwin) => !libname ^ ".dll"
                               | (Library, MinGW)  => !libname ^ ".dll"
                               | (Library, _) => maybeOut ".so"
                           val libOpts =
                              case targetOS of
                                 Darwin => [ "-dynamiclib" ]
                               | Cygwin =>  [ "-shared",
                                              "-Wl,--out-implib," ^
                                                 maybeOut ".a",
                                              "-Wl,--output-def," ^
                                                 !libname ^ ".def"]
                               | MinGW =>  [ "-shared",
                                             "-Wl,--out-implib," ^
                                                maybeOut ".a",
                                             "-Wl,--output-def," ^
                                                !libname ^ ".def"]
                               | _ =>      [ "-fPIC", "-shared" ]
                           val _ =
                              trace (Top, "Link")
                              (fn () =>
                               if !format = Archive orelse !format = LibArchive
                               then System.system
                                    (arScript,
                                     List.concat
                                      [[targetStr, targetOSStr, output],
                                       inputs,
                                       linkArchives])
                               else System.system
                                    (hd cc,
                                     List.concat
                                      [tl cc,
                                       case !format of
                                          Executable =>
                                             Control.PositionIndependentStyle.linkOpts
                                             positionIndependentStyle
                                        | Library => libOpts
                                        | _ => [],
                                       ["-o", output],
                                       inputs,
                                       linkOpts]))
                              ()
                           (* gcc on Cygwin appends .exe, which I don't want, so
                            * move the output file to it's rightful place.
                            * Notice that we do not use targetOS here, since we
                            * care about the platform we're running on, not the
                            * platform we're generating for.
                            *
                            * We want to keep the .exe as is for MinGW/Win32.
                            *)
                           val _ =
                              if MLton.Platform.OS.host = Cygwin
                                 then
                                    if String.contains (output, #".")
                                       then ()
                                    else
                                       File.move {from = concat [output, ".exe"],
                                                  to = output}
                              else ()
                        in
                           ()
                        end
                  fun mkOutputO (c: Counter.t, input: File.t): File.t =
                     if stop = Place.O orelse !keepO
                        then
                           if File.dirOf input = File.dirOf (maybeOutBase ".o")
                              then
                                 concat [File.base input, ".o"]
                              else
                                 maybeOutBase
                                    (concat [".",
                                             Int.toString (Counter.next c),
                                             ".o"])
                        else temp ".o"
                  fun mkOutputBC (c: Counter.t, input: File.t, xsuf): File.t =
                     if stop = Place.O orelse !keepO
                        then
                           if File.dirOf input = File.dirOf (maybeOutBase (xsuf ^ ".bc"))
                              then
                                 concat [File.base input, xsuf, ".bc"]
                              else
                                 maybeOutBase
                                    (concat [".",
                                             Int.toString (Counter.next c),
                                             xsuf,
                                             ".bc"])
                        else temp (xsuf ^ ".bc")
                  fun compileC (c: Counter.t, input: File.t): File.t =
                     let
                        val output = mkOutputO (c, input)
                        val _ =
                           System.system
                            (hd cc,
                             List.concat
                             [tl cc,
                              [ "-c" ],
                              if !debug
                              then [ "-g", "-DASSERT=1" ] else [],
                              if !format = Executable
                              then [] else [ "-DLIBNAME=" ^ !libname ],
                              Control.PositionIndependentStyle.ccOpts
                              positionIndependentStyle,
                              [ "-I" ^ targetIncDir ],
                              ccOpts,
                              ["-o", output],
                              [input]])
                     in
                        output
                     end
                  fun compileS (c: Counter.t, input: File.t): File.t =
                     let
                        val output = mkOutputO (c, input)
                        val _ =
                           System.system
                           (hd cc,
                            List.concat
                            [tl cc,
                             ["-c"],
                             if !debug then [ "-Wa,-g" ] else [],
                             asOpts,
                             ["-o", output],
                             [input]])
                     in
                        output
                     end
                  fun compileLL (c: Counter.t, input: File.t): File.t =
                     let
                        val asBC = mkOutputBC (c, input, ".as")
                        val _ =
                           System.system
                           (llvm_as,
                            List.concat
                            [llvm_asOpts,
                             ["-o", asBC],
                             [input]])
                        val optBC = mkOutputBC (c, input, ".opt")
                        val _ =
                           System.system
                           (llvm_opt,
                            List.concat
                            [llvm_optOpts,
                             ["-o", optBC],
                             [asBC]])
                        val output = mkOutputO (c, input)
                        val _ =
                           System.system
                           (llvm_llc,
                            List.concat
                            [["-filetype=obj"],
                             Control.PositionIndependentStyle.llvm_llcOpts
                             (positionIndependentStyle,
                              {targetDefault = pisTargetDefault}),
                             llvm_llcOpts,
                             ["-o", output],
                             [optBC]])
                     in
                        output
                     end
                  fun compileCSO (inputs: File.t list): unit =
                     if List.forall (inputs, fn f =>
                                     SOME "o" = File.extension f)
                        then compileO inputs
                     else
                     let
                        val c = Counter.new 0
                        val oFiles =
                           trace (Top, "Compile and Assemble")
                           (fn () =>
                            List.fold
                            (inputs, [], fn (input, ac) =>
                             let
                                val extension = File.extension input
                             in
                                if SOME "o" = extension
                                   then input :: ac
                                else if SOME "c" = extension
                                   then (compileC (c, input)) :: ac
                                else if SOME "ll" = extension
                                   then (compileLL(c, input)) :: ac
                                else if SOME "s" = extension
                                        orelse SOME "S" = extension
                                   then (compileS (c, input)) :: ac
                                else Error.bug
                                     (concat
                                      ["invalid extension: ",
                                       Option.toString (fn s => s) extension])
                             end))
                           ()
                     in
                        case stop of
                           Place.O => ()
                         | _ => compileO (rev oFiles)
                     end
                  fun compileSrc sel =
                     let
                        val outputs: File.t list ref = ref []
                        val r = Counter.generator 0
                        fun make (style: style, suf: string) () =
                           let
                              val suf = concat [".", Int.toString (r ()), suf]
                              val file = (if !keepGenerated
                                             orelse stop = Place.Generated
                                             then maybeOutBase
                                          else temp) suf
                              val _ = List.push (outputs, file)
                              val out = Out.openOut file
                              fun print s = Out.output (out, s)
                              val _ = outputHeader' (style, out)
                              fun done () = Out.close out
                           in
                              {file = file,
                               print = print,
                               done = done}
                           end
                        val _ = Control.message (Verbosity.Detail, Control.layout)
                        val {sourceFiles, frontend, compile} =
                           (sel o Compile.mkCompile)
                           {outputC = make (Control.C, ".c"),
                            outputLL = make (Control.LLVM, ".ll"),
                            outputS = make (Control.Assembly, ".s")}
                        val _ =
                           case stop of
                              Place.Files =>
                                 Vector.foreach
                                 (sourceFiles input, fn f =>
                                  (print (String.translate
                                          (f, fn #"\\" => "/" | c => str c))
                                   ; print "\n"))
                            | Place.TypeCheck => frontend input
                            | _ => compile input
                     in
                        case stop of
                           Place.Files => ()
                         | Place.TypeCheck => ()
                         | Place.Generated => ()
                         | _ =>
                              (* Shrink the heap before calling C compiler. *)
                              (MLton.GC.pack ()
                               ; compileCSO (List.concat [!outputs, csoFiles]))
                     end
                  fun compile () =
                     case start of
                        Place.SML => compileSrc #sml
                      | Place.MLB => compileSrc #mlb
                      | Place.Generated => compileCSO (input :: csoFiles)
                      | Place.O => compileCSO (input :: csoFiles)
                      | Place.XML => compileSrc #xml
                      | Place.SXML => compileSrc #xml
                      | Place.SSA => compileSrc #ssa
                      | Place.SSA2 => compileSrc #ssa2
                      | _ => Error.bug "invalid start"
                  val doit =
                     traceTop Version.banner
                     (fn () =>
                      Exn.finally
                      (compile, fn () =>
                       List.foreach (!tempFiles, File.remove)))
               in
                  doit ()
               end
         end
   end

val main = CommandLine.makeMain commandLine

fun mainWrapped () = CommandLine.wrapMain main ()

end
