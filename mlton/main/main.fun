(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Main (S: MAIN_STRUCTS): MAIN =
struct

open S

structure Compile = Compile ()

structure Place =
   struct
      datatype t = CM | Files | Generated | MLB | O | OUT | SML | TypeCheck

      val toInt: t -> int =
         fn MLB => 1
          | CM => 1
          | Files => 2
          | SML => 3
          | TypeCheck => 4
          | Generated => 5
          | O => 6
          | OUT => 7

      val toString =
         fn CM => "cm"
          | Files => "files"
          | SML => "sml"
          | MLB => "mlb"
          | Generated => "g"
          | O => "o"
          | OUT => "out"
          | TypeCheck => "tc"

      fun compare (p, p') = Int.compare (toInt p, toInt p')
   end

structure OptPred =
   struct
      datatype t =
         Target of string
       | Yes 
   end

val asOpts: {opt: string, pred: OptPred.t} list ref = ref []
val buildConstants: bool ref = ref false
val ccOpts: {opt: string, pred: OptPred.t} list ref = ref []
val cmmcOpts: {opt:string, pred: OptPred.t} list ref = ref []
val coalesce: int option ref = ref NONE
val expert: bool ref = ref false
val explicitAlign: Control.align option ref = ref NONE
val explicitCodegen: Control.codegen option ref = ref NONE
val gcc: string ref = ref "<unset>"
val keepGenerated = ref false
val keepO = ref false
val keepSML = ref false
val linkOpts: {opt: string, pred: OptPred.t} list ref = ref []
val output: string option ref = ref NONE
val profileSet: bool ref = ref false
val profileTimeSet: bool ref = ref false
val qcmm: string ref = ref "<unset>"
val runtimeArgs: string list ref = ref ["@MLton"]
val showAnns: bool ref = ref false
val stop = ref Place.OUT

val targetMap: unit -> {arch: MLton.Platform.Arch.t,
                        os: MLton.Platform.OS.t,
                        target: string} list =
   Promise.lazy
   (fn () =>
    List.map
    (File.lines (OS.Path.joinDirFile {dir = !Control.libDir,
                                      file = "target-map"}),
     fn line =>
     case String.tokens (line, Char.isSpace) of
        [target, arch, os] =>
           let
              val arch =
                 case MLton.Platform.Arch.fromString arch of
                    NONE => Error.bug (concat ["strange arch: ", arch])
                  | SOME a => a
              val os =
                 case MLton.Platform.OS.fromString os of
                    NONE => Error.bug (concat ["strange os: ", os])
                  | SOME os => os
           in
              {arch = arch, os = os, target = target}
           end
      | _ => Error.bug (concat ["strange target mapping: ", line])))

fun setTargetType (target: string, usage): unit =
   case List.peek (targetMap (), fn {target = t, ...} => target = t) of
      NONE => usage (concat ["invalid target: ", target])
    | SOME {arch, os, ...} =>
         let 
            open Control
         in
            targetArch := arch
            ; targetOS := os
         end

fun hasNative () =
   let
      datatype z = datatype Control.arch
   in
      case !Control.targetArch of
         AMD64 => true
       | X86 => true
       | _ => false
   end

fun makeOptions {usage} = 
   let
      val usage = fn s => (ignore (usage s); raise Fail "unreachable")
      fun reportAnnotation (s, flag, e) =
         case e of
            Control.Elaborate.Bad => 
               usage (concat ["invalid -", flag, " flag: ", s])
          | Control.Elaborate.Deprecated ids =>
               Out.output 
               (Out.error,
                concat ["Warning: ", "deprecated annotation: ", s, ".  Use ",
                        List.toString Control.Elaborate.Id.name ids, ".\n"])
          | Control.Elaborate.Good () => ()
          | Control.Elaborate.Other =>
               usage (concat ["invalid -", flag, " flag: ", s])
      open Control Popt
      fun push r = SpaceString (fn s => List.push (r, s))
      datatype z = datatype MLton.Platform.Arch.t
   in
      List.map
      (
       [
       (Normal, "align",
        case !targetArch of
           Sparc => " {8|4}"
         | _ => " {4|8}",
        "object alignment",
        (SpaceString (fn s =>
                      explicitAlign
                      := SOME (case s of
                                  "4" => Align4
                                | "8" => Align8
                                | _ => usage (concat ["invalid -align flag: ",
                                                      s]))))),
       (Normal, "as-opt", " <opt>", "pass option to assembler",
        SpaceString (fn s =>
                     List.push (asOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "build-constants", " {false|true}",
        "output C file that prints basis constants",
        boolRef buildConstants),
       (Expert, "cc", " <gcc>", "path to gcc executable",
        SpaceString (fn s => gcc := s)),
       (Normal, "cc-opt", " <opt>", "pass option to C compiler",
        SpaceString (fn s =>
                     List.push (ccOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "cmm-debug", " {false|true}", "debug C-- codegen",
        boolRef Cmm.debug),
       (Expert, "cmm-non-tail", " {cutTo|cutToNR|return}", 
        "how to implement non-tail transfers",
        SpaceString (fn s =>
                     case s of
                        "cutTo" => Cmm.nonTail := Cmm.CutTo {neverReturns = false}
                      | "cutToNR" => Cmm.nonTail := Cmm.CutTo {neverReturns = true}
                      | "return" => Cmm.nonTail := Cmm.Return
                      | _ => usage (concat ["invalid -cmm-non-tail flag: ", s]))),
       (Expert, "cmmc", " <qc-->", "path to qc-- executable",
        SpaceString (fn s => qcmm := s)),
       (Normal, "cmmc-opt", " <opt>", "pass option to C-- compiler",
        SpaceString (fn s =>
                     List.push (cmmcOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "coalesce", " <n>", "coalesce chunk size for C codegen",
        Int (fn n => coalesce := SOME n)),
       (Normal, "codegen",
        concat [" {", if hasNative () then "native|" else "", "bytecode|c}"],
        "which code generator to use",
        SpaceString (fn s =>
                     explicitCodegen
                     := SOME (case s of
                                 "bytecode" => Bytecode
                               | "c" => CCodegen
                               | "cmm" => CmmCodegen
                               | "native" => Native
                               | _ => usage (concat
                                             ["invalid -codegen flag: ", s])))),
       (Normal, "const", " '<name> <value>'", "set compile-time constant",
        SpaceString (fn s =>
                     case String.tokens (s, Char.isSpace) of
                        [name, value] =>
                           Compile.setCommandLineConstant {name = name,
                                                           value = value}
                      | _ => usage (concat ["invalid -const flag: ", s]))),
       (Expert, "contify-into-main", " {false|true}",
        "contify functions into main",
        boolRef contifyIntoMain),
       (Expert, "debug", " {false|true}", "produce executable with debug info",
        boolRef debug),
       let
          val flag = "default-ann"
       in
          (Normal, flag, " <ann>", "set annotation default for mlb files",
           SpaceString
           (fn s => reportAnnotation (s, flag,
                                      Control.Elaborate.processDefault s)))
       end,
       (Expert, "diag-pass", " <pass>", "keep diagnostic info for pass",
        SpaceString 
        (fn s =>
         (case Regexp.fromString s of
             SOME (re,_) => let val re = Regexp.compileDFA re
                            in 
                               List.push (diagPasses, re)
                               ; List.push (keepPasses, re)
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
       (Expert, "drop-pass", " <pass>", "omit optimization pass",
        SpaceString
        (fn s => (case Regexp.fromString s of
                     SOME (re,_) => let val re = Regexp.compileDFA re
                                    in List.push (dropPasses, re)
                                    end
                   | NONE => usage (concat ["invalid -drop-pass flag: ", s])))),
       let
          val flag = "enable-ann"
       in
          (Expert, flag, " <ann>", "globally enable annotation",
           SpaceString 
           (fn s =>
            reportAnnotation (s, flag,
                              Control.Elaborate.processEnabled (s, true))))
       end,
       (Expert, "error-threshhold", " 20", "error threshhold",
        intRef errorThreshhold),
       (Expert, "expert", " {false|true}", "enable expert status",
        boolRef expert),
       (Normal, "export-header", " <file>", "write C header file for _export's",
        SpaceString (fn s => exportHeader := SOME s)),
       (Expert, "gc-check", " {limit|first|every}", "force GCs",
        SpaceString (fn s =>
                     gcCheck :=
                     (case s of
                         "limit" => Limit
                       | "first" => First
                       | "every" => Every
                       | _ => usage (concat ["invalid -gc-check flag: ", s])))),
       (Normal, "ieee-fp", " {false|true}", "use strict IEEE floating-point",
        boolRef Native.IEEEFP),
       (Expert, "indentation", " <n>", "indentation level in ILs",
        intRef indentation),
       (Normal, "inline", " <n>", "set inlining threshold", Int setInlineSize),
       (Expert, "inline-into-main", " {true|false}",
        "inline functions into main",
        boolRef inlineIntoMain),
       (Normal, "keep", " {g|o|sml}", "save intermediate files",
        SpaceString (fn s =>
                     case s of
                        "dot" => keepDot := true
                      | "g" => keepGenerated := true
                      | "machine" => keepMachine := true
                      | "o" => keepO := true
                      | "sml" => keepSML := true
                      | "rssa" => keepRSSA := true
                      | "ssa" => keepSSA := true
                      | "ssa2" => keepSSA2 := true
                      | _ => usage (concat ["invalid -keep flag: ", s]))),
       (Expert, "keep-pass", " <pass>", "keep the results of pass",
        SpaceString
        (fn s => (case Regexp.fromString s of
                     SOME (re,_) => let val re = Regexp.compileDFA re
                                    in List.push (keepPasses, re)
                                    end
                   | NONE => usage (concat ["invalid -keep-pass flag: ", s])))),
       (Normal, "link-opt", " <opt>", "pass option to linker",
        SpaceString (fn s =>
                     List.push (linkOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "loop-passes", " <n>", "loop optimization passes (1)",
        Int 
        (fn i => 
         if i >= 1
            then loopPasses := i
            else usage (concat ["invalid -loop-passes arg: ", Int.toString i]))),
       (Expert, "mark-cards", " {true|false}", "mutator marks cards",
        boolRef markCards),
       (Expert, "max-function-size", " <n>", "max function size (blocks)",
        intRef maxFunctionSize),
       (Normal, "mlb-path-map", " <file>", "additional MLB path map",
        SpaceString (fn s => mlbPathMaps := !mlbPathMaps @ [s])),
       (Expert, "native-commented", " <n>", "level of comments  (0)",
        intRef Native.commented),
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
       (Expert, "native-split", " <n>", "split assembly files at ~n lines",
        Int (fn i => Native.split := SOME i)),
       (Expert, "native-shuffle", " {true|false}",
        "shuffle registers at C-calls",
        Bool (fn b => Native.shuffle := b)),
       (Expert, "opt-passes", " {default|minimal}", "level of optimizations",
        SpaceString (fn s =>
                     let
                        fun err s =
                           usage (concat ["invalid -opt-passes flag: ", s])
                        fun doit optPasses =
                           List.foreach
                           (!optimizationPassesSet, fn (_,optPassesSet) =>
                            case optPassesSet optPasses of
                               Result.Yes () => ()
                             | Result.No s' => err ("il :: " ^ s')) 
                     in
                        case s of
                           "default" => doit OptPassesDefault
                         | "minimal" => doit OptPassesMinimal
                         | _ => err s
                     end)),
       (Normal, "output", " <file>", "name of output file",
        SpaceString (fn s => output := SOME s)),
       (Expert, "polyvariance", " {true|false}", "use polyvariance",
        Bool (fn b => if b then () else polyvariance := NONE)),
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
                            | "label" => ProfileLabel
                            | "time" => (profileTimeSet := true
                                         ; ProfileTimeLabel)
                            | "time-field" => ProfileTimeField
                            | "time-label" => ProfileTimeLabel
                            | _ => usage (concat
                                          ["invalid -profile arg: ", s]))))),
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
       (Normal, "runtime", " <arg>", "pass arg to runtime via @MLton",
        push runtimeArgs),
       (Expert, "show-anns", " {false|true}", "show annotations",
        boolRef showAnns),
       (Normal, "show-basis", " <file>", "write out the final basis environment",
        SpaceString (fn s => showBasis := SOME s)),
       (Normal, "show-def-use", " <file>", "write def-use information",
        SpaceString (fn s => showDefUse := SOME s)),
       (Expert, "show-types", " {false|true}", "show types in ILs",
        boolRef showTypes),
       (Expert, "ssa-passes", " <passes>", "ssa optimization passes",
        SpaceString
        (fn s =>
         case !Control.ssaPassesSet (OptPassesCustom s) of
            Result.Yes () => ()
          | Result.No s' => usage (concat ["invalid -ssa-pass arg: ", s']))),
       (Expert, "ssa2-passes", " <passes>", "ssa2 optimization passes",
        SpaceString
        (fn s =>
         case !Control.ssa2PassesSet (OptPassesCustom s) of
            Result.Yes () => ()
          | Result.No s' => usage (concat ["invalid -ssa2-pass arg: ", s']))),
       (Normal, "stop", " {f|g|o|sml|tc}", "when to stop",
        SpaceString
        (fn s =>
         stop := (case s of
                     "f" => Place.Files
                   | "g" => Place.Generated     
                   | "o" => Place.O
                   | "sml" => Place.SML
                   | "tc" => Place.TypeCheck
                   | _ => usage (concat ["invalid -stop arg: ", s])))),
       (Expert, "sxml-passes", " <passes>", "sxml optimization passes",
        SpaceString
        (fn s =>
         case !Control.sxmlPassesSet (OptPassesCustom s) of
            Result.Yes () => ()
          | Result.No s' => usage (concat ["invalid -sxml-pass arg: ", s']))),
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
        (SpaceString2
         (fn (target, opt) =>
          List.push (asOpts, {opt = opt, pred = OptPred.Target target})))),
       (Normal, "target-cc-opt", " <target> <opt>", "target-dependent C compiler option",
        (SpaceString2
         (fn (target, opt) =>
          List.push (ccOpts, {opt = opt, pred = OptPred.Target target})))),
       (Normal, "target-cmmc-opt", " <target> <opt>", "target-dependent C-- compiler option",
        (SpaceString2
         (fn (target, opt) =>
          List.push (cmmcOpts, {opt = opt, pred = OptPred.Target target})))),
       (Normal, "target-link-opt", " <target> <opt>", "target-dependent linker option",
        (SpaceString2
         (fn (target, opt) =>
          List.push (linkOpts, {opt = opt, pred = OptPred.Target target})))),
       (Expert, #1 trace, " name1,...", "trace compiler internals", #2 trace),
       (Expert, "type-check", " {false|true}", "type check ILs",
        boolRef typeCheck),
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
       (Expert, "xml-passes", " <passes>", "xml optimization passes",
        SpaceString
        (fn s =>
         case !Control.xmlPassesSet (OptPassesCustom s) of
            Result.Yes () => ()
          | Result.No s' => usage (concat ["invalid -xml-pass arg: ", s']))),
       (Expert, "zone-cut-depth", " <n>", "zone cut depth",
        intRef zoneCutDepth)
       ],
       fn (style, name, arg, desc, opt) =>
       {arg = arg, desc = desc, name = name, opt = opt, style = style})
   end

val mainUsage =
   "mlton [option ...] file.{c|cm|mlb|o|sml} [file.{c|o|s|S} ...]"

val {parse, usage} =
   Popt.makeUsage {mainUsage = mainUsage,
                   makeOptions = makeOptions,
                   showExpert = fn () => !expert}

val usage = fn s => (usage s; raise Fail "unreachable")
   
fun commandLine (args: string list): unit =
   let
      open Control
      val args =
         case args of
            lib :: args =>
               (libDir := OS.Path.mkCanonical lib
                ; args)
          | _ => Error.bug "incorrect args from shell script"
      val () = setTargetType ("self", usage)
      val result = parse args
      val targetArch = !targetArch
      val () =
         align := (case !explicitAlign of
                      NONE => (case targetArch of
                                  Sparc => Align8
                                | HPPA => Align8
                                | _ => Align4)
                    | SOME a => a)
      val () =
         codegen := (case !explicitCodegen of
                        NONE => if hasNative () then Native else CCodegen
                      | SOME c => c)
      val () = MLton.Rusage.measureGC (!verbosity <> Silent)
      val () =
         if !showAnns then
            (Layout.outputl (Control.Elaborate.document {expert = !expert}, 
                             Out.standard)
             ; let open OS.Process in exit success end)
         else ()
      val () = if !profileTimeSet
                  then (case !codegen of
                           Native => profile := ProfileTimeLabel
                         | _ => profile := ProfileTimeField)
                  else ()
      val () = if !exnHistory
                  then (case !profile of
                           ProfileNone => profile := ProfileCallStack
                         | ProfileCallStack => ()
                         | _ => usage "can't use -profile with Exn.keepHistory"
                        ; profileRaise := true)
               else ()
      val () =
         Compile.setCommandLineConstant
         {name = "CallStack.keep",
          value = Bool.toString (!Control.profile = Control.ProfileCallStack)}
      val gcc = !gcc
      val qcmm = !qcmm
      val stop = !stop
      val target = !target
      val targetStr =
         case target of
            Cross s => s
          | Self => "self"
      val _ = libTargetDir := OS.Path.concat (!libDir, targetStr)
      val archStr = String.toLower (MLton.Platform.Arch.toString targetArch)
      val targetOS = !targetOS
      val () =
         Control.labelsHaveExtra_ := (case targetOS of
                                         Cygwin => true
                                       | MinGW => true
                                       | _ => false)
      val OSStr = String.toLower (MLton.Platform.OS.toString targetOS)
      fun tokenize l =
         String.tokens (concat (List.separate (l, " ")), Char.isSpace)
      fun addTargetOpts opts =
         tokenize
         (List.fold
          (!opts, [], fn ({opt, pred}, ac) =>
           if (case pred of
                  OptPred.Target s =>
                     let
                        val s = String.toLower s
                     in
                        s = archStr orelse s = OSStr
                     end
                | OptPred.Yes => true)
              then opt :: ac
           else ac))
      val asOpts = addTargetOpts asOpts
      val ccOpts = addTargetOpts ccOpts
      val cmmcOpts = addTargetOpts cmmcOpts
      val linkOpts =
         List.concat [[concat ["-L", !libTargetDir]],
                      if !debug then ["-lmlton-gdb"] else ["-lmlton"],
                      if !codegen = CmmCodegen then ["-lqc--"] else [],
                      addTargetOpts linkOpts]
      (* With gcc 3.4, the '-b <arch>' must be the first argument. *)
      val targetOpts =
         case target of
            Cross s =>
               if Cygwin = MLton.Platform.OS.host
                  andalso String.hasSubstring (s, {substring = "mingw"})
                  then ["-mno-cygwin"]
               else ["-b", s]
          | Self => []
      val _ =
         if !codegen = Native andalso not (hasNative ())
            then usage (concat ["can't use native codegen on ",
                                MLton.Platform.Arch.toString targetArch])
         else ()
      val _ =
         chunk :=
         (case !codegen of
             Bytecode => OneChunk
           | CCodegen => Coalesce {limit = (case !coalesce of
                                               NONE => 4096
                                             | SOME n => n)}
           | CmmCodegen =>
                if isSome (!coalesce)
                   then usage "can't use -coalesce and -codegen cmm"
                else ChunkPerFunc
           | Native =>
                if isSome (!coalesce)
                   then usage "can't use -coalesce and -codegen native"
                else ChunkPerFunc)
      val _ = if not (!Control.codegen = Native) andalso !Native.IEEEFP
                 then usage "must use native codegen with -ieee-fp true"
              else ()
      val _ =
         if !keepDot andalso List.isEmpty (!keepPasses)
            then keepSSA := true
         else ()
      val keepDefUse = 
         isSome (!showDefUse)
         orelse (Control.Elaborate.enabled Control.Elaborate.warnUnused)
         orelse (Control.Elaborate.default Control.Elaborate.warnUnused)
      val warnMatch =
          (Control.Elaborate.enabled Control.Elaborate.nonexhaustiveMatch)
          orelse (Control.Elaborate.enabled Control.Elaborate.redundantMatch)
          orelse (Control.Elaborate.default Control.Elaborate.nonexhaustiveMatch <>
                  Control.Elaborate.DiagEIW.Ignore)
          orelse (Control.Elaborate.default Control.Elaborate.redundantMatch <>
                  Control.Elaborate.DiagEIW.Ignore)
      val _ = elaborateOnly := (stop = Place.TypeCheck
                                andalso not (warnMatch)
                                andalso not (keepDefUse))
      val _ =
         if !codegen = Bytecode andalso !profile <> ProfileNone
            then usage (concat ["bytecode doesn't support profiling\n"])
         else ()
      val _ =
         case targetOS of
            Darwin => ()
          | FreeBSD => ()
          | HPUX => ()
          | Linux => ()
          | NetBSD => ()
          | OpenBSD => ()
          | Solaris => ()
          | _ =>
               if !profile = ProfileTimeField 
                  orelse !profile = ProfileTimeLabel
                  then usage (concat ["can't use -profile time on ",
                                      MLton.Platform.OS.toString targetOS])
               else ()
      fun printVersion (out: Out.t): unit =
         Out.output (out, concat [version, " ", build, "\n"])
   in
      case result of
      Result.No msg => usage msg
    | Result.Yes [] =>
         (inputFile := "<none>"
          ; if isSome (!showBasis)
               then (trace (Top, "Type Check SML")
                     Compile.elaborateSML {input = []})
            else if !buildConstants
               then Compile.outputBasisConstants Out.standard
            else if !verbosity = Silent orelse !verbosity = Top
               then printVersion Out.standard
            else outputHeader' (No, Out.standard))
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
                        (".cm", CM, false),
                        (".sml", SML, false),
                        (".c", Generated, true),
                        (".o", O, true)]
               end
            val _ =
               List.foreach
               (rest, fn f =>
                if List.exists ([".c", ".cmm", ".o", ".s", ".S"], fn suffix =>
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
                     val _ =
                        if !verbosity = Top
                           then printVersion Out.error
                        else ()
                     val tempFiles: File.t list ref = ref []
                     val tmpDir =
                        let
                           val (tmpVar, default) =
                              case MLton.Platform.OS.host of
                                 MinGW => ("TEMP", "C:/WINDOWS/TEMP")
                               | _ => ("TMPDIR", "/tmp")
                        in
                           case Process.getEnv tmpVar of
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
                     val _ =
                        atMLtons :=
                        Vector.fromList
                        (maybeOut "" :: tokenize (rev ("--" :: (!runtimeArgs))))
                     datatype debugFormat =
                        Dwarf | DwarfPlus | Dwarf2 | Stabs | StabsPlus
                     (* The -Wa,--gstabs says to pass the --gstabs option to the
                      * assembler. This tells the assembler to generate stabs
                      * debugging information for each assembler line.
                      *)
                     val debugFormat = StabsPlus
                     val (gccDebug, asDebug) =
                        case debugFormat of
                           Dwarf => (["-gdwarf", "-g2"], "-Wa,--gdwarf2")
                         | DwarfPlus => (["-gdwarf+", "-g2"], "-Wa,--gdwarf2")
                         | Dwarf2 => (["-gdwarf-2", "-g2"], "-Wa,--gdwarf2")
                         | Stabs => (["-gstabs", "-g2"], "-Wa,--gstabs")
                         | StabsPlus => (["-gstabs+", "-g2"], "-Wa,--gstabs")
                     val qcmmDebug = []
                     fun compileO (inputs: File.t list): unit =
                        let
                           val output = maybeOut ""
                           val _ =
                              trace (Top, "Link")
                              (fn () =>
                               System.system
                               (gcc,
                                List.concat
                                [targetOpts, 
                                 ["-o", output],
                                 if !debug then gccDebug else [],
                                 inputs, 
                                 if !codegen = CmmCodegen
                                    then [concat [!Control.libTargetDir, "/pcmap.ld"]]
                                    else [],
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
                           if !keepGenerated 
                              orelse start = Place.Generated
                              then
                                 concat [File.base input,
                                         ".o"]
                              else 
                                 suffix
                                 (concat [".",
                                          Int.toString
                                          (Counter.next c),
                                          ".o"])
                        else temp ".o"
                  fun compileC (c: Counter.t, input: File.t): File.t =
                     let
                        val (debugSwitches, switches) =
                           (gccDebug @ ["-DASSERT=1"], ccOpts)
                        val switches =
                           if !debug
                              then debugSwitches @ switches
                              else switches
                        val switches =
                           targetOpts @ ("-std=gnu99" :: "-c" :: switches)
                        val output = mkOutputO (c, input)
                        val _ =
                           System.system
                           (gcc,
                            List.concat [switches,
                                         ["-o", output, input]])
                     in
                        output
                     end
                  local val qcmmGlobals = ref true in
                  fun compileCmm (c: Counter.t, input: File.t): File.t =
                     let
                        val switches = cmmcOpts
                        val switches =
                           if !qcmmGlobals
                              then (qcmmGlobals := false
                                    ; "-globals" :: switches)
                              else switches
                        val switches =
                           "-c" :: switches
                        val output = mkOutputO (c, input)
                        val _ =
                           System.system
                           (qcmm,
                            List.concat [switches,
                                         ["-o", output, input]])
                     in
                        output
                     end
                  end
                  fun compileS (c: Counter.t, input: File.t): File.t =
                     let
                        val (debugSwitches, switches) =
                           ([asDebug], asOpts)
                        val switches =
                           if !debug
                              then debugSwitches @ switches
                              else switches
                        val switches =
                           targetOpts @ ("-c" :: switches)
                        val output = mkOutputO (c, input)
                        val _ =
                           System.system
                           (gcc,
                            List.concat [switches,
                                         ["-o", output, input]])
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
                                else if SOME "cmm" = extension
                                   then (compileCmm (c, input)) :: ac
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
                  fun compileSml (files: File.t list) =
                     let
                        val outputs: File.t list ref = ref []
                        val r = ref 0
                        fun make (style: style, suf: string) () =
                           let
                              val suf = concat [".", Int.toString (!r), suf]
                              val _ = Int.inc r
                              val file = (if !keepGenerated
                                             orelse stop = Place.Generated
                                             then suffix
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
                        val _ =
                           case !verbosity of
                              Silent => ()
                            | Top => ()
                            | _ => 
                                 outputHeader
                                 (Control.No, fn l =>
                                  let val out = Out.error
                                  in Layout.output (l, out)
                                     ; Out.newline out
                                  end)
                        val _ =
                           case stop of
                              Place.TypeCheck =>
                                 trace (Top, "Type Check SML")
                                 Compile.elaborateSML {input = files}
                            | _ => 
                                 trace (Top, "Compile SML")
                                 Compile.compileSML
                                 {input = files,
                                  outputC = make (Control.C, ".c"),
                                  outputCmm = make (Control.C, ".cmm"),
                                  outputS = make (Control.Assembly,
                                                  if !debug then ".s" else ".S")}
                     in
                        case stop of
                           Place.Generated => ()
                         | Place.TypeCheck => ()
                         | _ =>
                              (* Shrink the heap before calling gcc. *)
                              (MLton.GC.pack ()
                               ; compileCSO (List.concat [!outputs, csoFiles]))
                     end
                  fun showFiles (fs: File.t vector) =
                     Vector.foreach
                     (fs, fn f =>
                      print (concat [String.translate
                                     (f, fn #"\\" => "/"
                                          | c => str c),
                                     "\n"]))
                  fun compileCM input =
                     let
                        val files = CM.cm {cmfile = input}
                        fun saveSML smlFile =
                           File.withOut
                           (smlFile, fn out =>
                            (outputHeader' (ML, out)
                             ; (List.foreach
                                (files, fn f =>
                                 (Out.output
                                  (out, concat ["(*#line 0.0 \"", f, "\"*)\n"])
                                  ; File.outputContents (f, out))))))
                     in
                        case stop of
                           Place.Files =>
                              showFiles (Vector.fromList files)
                         | Place.SML => saveSML (maybeOut ".sml")
                         | _ =>
                              (if !keepSML
                                  then saveSML (suffix ".sml")
                               else ()
                               ; compileSml files)
                     end
                  fun compileMLB file =
                     let
                        val outputs: File.t list ref = ref []
                        val r = ref 0
                        fun make (style: style, suf: string) () =
                           let
                              val suf = concat [".", Int.toString (!r), suf]
                              val _ = Int.inc r
                              val file = (if !keepGenerated
                                             orelse stop = Place.Generated
                                             then suffix
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
                        val _ =
                           case !verbosity of
                              Silent => ()
                            | Top => ()
                            | _ => 
                                 outputHeader
                                 (Control.No, fn l =>
                                  let val out = Out.error
                                  in Layout.output (l, out)
                                     ; Out.newline out
                                  end)
                        fun saveSML smlFile =
                           File.withOut
                           (smlFile, fn out =>
                            (outputHeader' (ML, out)
                             ; (Vector.foreach
                                (Compile.sourceFilesMLB {input = file}, fn f =>
                                 (Out.output
                                  (out, concat ["(*#line 0.0 \"", f, "\"*)\n"])
                                  ; File.outputContents (f, out))))))
                        val _ =
                           case stop of
                              Place.Files =>
                                 showFiles
                                 (Compile.sourceFilesMLB {input = file})
                            | Place.SML => saveSML (maybeOut ".sml")
                            | Place.TypeCheck =>
                                 trace (Top, "Type Check SML")
                                 Compile.elaborateMLB {input = file}
                            | _ => 
                                 trace (Top, "Compile SML")
                                 Compile.compileMLB
                                 {input = file,
                                  outputC = make (Control.C, ".c"),
                                  outputCmm = make (Control.C, ".cmm"),
                                  outputS = make (Control.Assembly,
                                                  if !debug then ".s" else ".S")}
                     in
                        case stop of
                           Place.Files => ()
                         | Place.SML => ()
                         | Place.TypeCheck => ()
                         | Place.Generated => ()
                         | _ =>
                              (* Shrink the heap before calling gcc. *)
                              (MLton.GC.pack ()
                               ; compileCSO (List.concat [!outputs, csoFiles]))
                     end
                  fun compile () =
                     case start of
                        Place.CM => compileCM input
                      | Place.SML =>
                           Control.checkFile
                           (input, fn s => raise Fail s,
                            fn () => compileSml [input])
                      | Place.MLB => compileMLB input
                      | Place.Generated => compileCSO (input :: csoFiles)
                      | Place.O => compileCSO (input :: csoFiles)
                      | _ => Error.bug "invalid start"
                  val doit 
                    = trace (Top, "MLton")
                      (fn () => 
                       Exn.finally
                       (compile, fn () =>
                        List.foreach (!tempFiles, File.remove)))
               in
                  doit ()
               end
         end
   end

val commandLine = Process.makeCommandLine commandLine
   
fun exportNJ (file: File.t): unit =
   SMLofNJ.exportFn (file, fn (_, args) => commandLine args)
   
fun exportMLton (): unit =
   case CommandLine.arguments () of
      [worldFile] =>
         SMLofNJ.exportFn (worldFile, fn (_, args) => commandLine args)
    | _ => Error.bug "usage: exportMLton worldFile"

end
