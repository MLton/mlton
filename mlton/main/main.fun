(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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

val buildConstants: bool ref = ref false
val ccOpts: {opt: string, pred: OptPred.t} list ref = ref []
val coalesce: int option ref = ref NONE
val expert: bool ref = ref false
val gcc: string ref = ref "<unset>"
val keepGenerated = ref false
val keepO = ref false
val keepSML = ref false
val linkOpts: {opt: string, pred: OptPred.t} list ref = ref []
val output: string option ref = ref NONE
val profileSet: bool ref = ref false
val runtimeArgs: string list ref = ref ["@MLton"]
val stop = ref Place.OUT

val targetMap: unit -> {arch: MLton.Platform.Arch.t,
			os: MLton.Platform.OS.t,
			target: string} list =
   Promise.lazy
   (fn () =>
    List.map
    (File.lines (concat [!Control.libDir, "/target-map"]), fn line =>
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
   case List.peek (targetMap (), fn {target = t, ...} => t = target) of
      NONE => usage (concat ["invalid target ", target])
    | SOME {arch, os, ...} =>
	 let
	    datatype z = datatype MLton.Platform.Arch.t
	    open Control
	 in
	    targetArch := arch
	    ; targetOS := os
	    ; (case arch of
		  Sparc => (align := Align8; codegen := CCodegen)
		| _ => ())
	 end

fun warnDeprecated (flag, use) =
   Out.output (Out.error,
	       concat ["Warning: -", flag, " is deprecated.  ",
		       "Use ", use, ".\n"])
   
fun makeOptions {usage} = 
   let
      val usage = fn s => (usage s; raise Fail "unreachable")
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
	 | X86 => " {4|8}",
	"object alignment",
	(SpaceString (fn s =>
		      align
		      := (case s of
			     "4" => Align4
			   | "8" => Align8
			   | _ => usage (concat ["invalid -align flag: ",
						 s]))))),
       (Expert, "allow-export", " {false|true}",
	"allow _export expression in program",
	Bool (fn b =>
	      (warnDeprecated ("allow-export", "-default-ann")
	       ; Control.Elaborate.default Control.Elaborate.allowExport := b))),
       (Expert, "allow-import", " {false|true}",
	"allow _import expression in program",
	Bool (fn b =>
	      (warnDeprecated ("allow-import", "-default-ann")
	       ; Control.Elaborate.default Control.Elaborate.allowImport := b))),
       (Expert, "basis", " {2002|1997|...}",
	"select Basis Library revision to prefix to the program",
	SpaceString (fn s =>
		     let
			val () = warnDeprecated ("basis", "mlb files")
			val s' = concat ["basis-", s]
		     in
			if List.contains (basisLibs, s', String.equals)
			   then basisLibrary := s'
			else usage (concat ["invalid -basis flag: ", s])
		     end)),
       (Expert, "build-constants", " {false|true}",
	"output C file that prints basis constants",
	boolRef buildConstants),
       (Expert, "card-size-log2", " <n>",
	"log (base 2) of card size used by GC",
	intRef cardSizeLog2),
       (Expert, "cc", " <gcc>", "path to gcc executable",
	SpaceString (fn s => gcc := s)),
       (Normal, "cc-opt", " <opt>", "pass option to C compiler",
	SpaceString (fn s =>
		     List.push (ccOpts, {opt = s, pred = OptPred.Yes}))),
       (Expert, "coalesce", " <n>", "coalesce chunk size for C codegen",
	Int (fn n => coalesce := SOME n)),
       (Normal, "codegen",
	if !targetArch = Sparc then " {c}" else " {native|bytecode|c}",
	"which code generator to use",
	SpaceString (fn s =>
		     case s of
			"bytecode" => codegen := Bytecode
		      | "c" => codegen := CCodegen
		      | "native" => codegen := Native
		      | _ => usage (concat ["invalid -codegen flag: ", s]))),
       (Expert, "contify-into-main", " {false|true}",
	"contify functions into main",
	boolRef contifyIntoMain),
       (Expert, "dead-code", " {true|false}",
	"annotated dead code elimination",
	Bool (fn b =>
	      (warnDeprecated ("dead-code", "-default-ann")
	       ; Control.Elaborate.enabled Control.Elaborate.deadCode := b))),
       (Expert, "debug", " {false|true}", "produce executable with debug info",
	boolRef debug),
       (Normal, "default-ann", " <ann>", "set annotation default for mlb files",
	SpaceString 
	(fn s =>
	 List.foreach
	 (String.tokens (s, fn #"," => true | _ => false), fn s =>
	  if Control.Elaborate.setDef (String.tokens
				       (s, fn #" " => true | _ => false))
	     then ()
	     else usage (concat ["invalid -default-ann flag: ", s])))),
       (Normal, "detect-overflow", " {true|false}",
	"overflow checking on integer arithmetic",
	boolRef detectOverflow),
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
       (Normal, "disable-ann", " <ann>", "disable annotation in mlb files",
	SpaceString 
	(fn s =>
	 List.foreach
	 (String.tokens (s, fn #"," => true | _ => false), fn s =>
	  if Control.Elaborate.setAble
	     (false, String.deleteSurroundingWhitespace s)
	     then ()
	     else usage (concat ["invalid -disable-ann flag: ", s])))),
       (Expert, "drop-pass", " <pass>", "omit optimization pass",
	SpaceString
	(fn s => (case Regexp.fromString s of
		     SOME (re,_) => let val re = Regexp.compileDFA re
				    in List.push (dropPasses, re)
				    end
		   | NONE => usage (concat ["invalid -drop-pass flag: ", s])))),
       (Expert, "eliminate-overflow", " {true|false}",
	"eliminate useless overflow tests",
	boolRef eliminateOverflow),
       (Expert, "enable-ann", " <ann>", "globally enable annotation",
	SpaceString 
	(fn s =>
	 List.foreach
	 (String.tokens (s, fn #"," => true | _ => false), fn s =>
	  if Control.Elaborate.setAble 
	     (true, String.deleteSurroundingWhitespace s)
	     then ()
	     else usage (concat ["invalid -enable-ann flag: ", s])))),
       (Expert, "error-threshhold", " 20", "error threshhold",
	intRef errorThreshhold),
       (Normal, "exn-history", " {false|true}", "enable Exn.history",
	boolRef exnHistory),
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
       (Expert, "handlers", " {flow|simple}",
	"how to implement handlers",
	SpaceString (fn s =>
		     case s of
			"flow" => handlers := Flow
		      | "simple" => handlers := Simple
		      | _ => usage (concat ["invalid -handlers flag: ", s]))),
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
		      | _ => usage (concat ["invalid -keep flag: ", s]))),
       (Expert, "keep-pass", " <pass>", "keep the results of pass",
	SpaceString
	(fn s => (case Regexp.fromString s of
		     SOME (re,_) => let val re = Regexp.compileDFA re
				    in List.push (keepPasses, re)
				    end
		   | NONE => usage (concat ["invalid -keep-pass flag: ", s])))),
       (Expert, "limit-check", " {lhle|pb|ebb|lh|lhf|lhfle}",
	"limit check insertion algorithm",
	SpaceString (fn s =>
		     case s of
		        "pb" => limitCheck := PerBlock
		      | "ebb" => limitCheck := ExtBasicBlocks
		      | "lh" => limitCheck := LoopHeaders {fullCFG = false,
							   loopExits = false}
		      | "lhf" => limitCheck := LoopHeaders {fullCFG = true,
							    loopExits = false}
		      | "lhle" => limitCheck := LoopHeaders {fullCFG = false,
							     loopExits = true}
		      | "lhfle" => limitCheck := LoopHeaders {fullCFG = true,
							      loopExits = true}
		      | _ => usage (concat ["invalid -limit-check flag: ", s]))),
       (Expert, "limit-check-counts", " {false|true}",
	"compute dynamic counts of limit checks",
	boolRef limitCheckCounts),
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
       (Expert, "native",
	if !targetArch = Sparc then " {false}" else " {true|false}",
	"use native code generator",
	Bool (fn b =>
	      (warnDeprecated ("native", "-codegen")
	       ; Control.codegen := (if b then Native else CCodegen)))),
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
       (Expert, "new-return", " {false|true}", "non-tail call return convention",
	boolRef newReturn),
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
			    | "count" => ProfileCount
			    | "time" => ProfileTime
			    | _ => usage (concat
					  ["invalid -profile arg: ", s]))))),
       (Expert, "profile-basis", " {false|true}",
	"profile the basis implementation",
	boolRef profileBasis),
       (Normal, "profile-branch", " {false|true}",
	"profile branches in addition to functions",
	boolRef profileBranch),
       (Expert, "profile-il", " {source}", "where to insert profile exps",
	SpaceString
	(fn s =>
	 case s of
	    "source" => profileIL := ProfileSource
	  | "ssa" => profileIL := ProfileSSA
	  | _ => usage (concat ["invalid -profile-il arg: ", s]))),
       (Normal, "profile-stack", " {false|true}", "profile the stack",
	boolRef profileStack),
       (Expert, "reserve-esp", " {false|true}", "reserve %ESP on x86",
	SpaceString
	(fn s =>
	 case Bool.fromString s of
	    NONE => usage (concat ["invalid -reserve-esp arg: ", s])
	  | SOME b => reserveEsp := SOME b)),
       (Normal, "runtime", " <arg>", "pass arg to runtime via @MLton",
	push runtimeArgs),
       (Normal, "safe", " {true|false}", "bounds checking and other checks",
	boolRef safe),
       (Expert, "sequence-unit", " {false|true}",
	"in (e1; e2), require e1: unit",
	Bool (fn b =>
	      (warnDeprecated ("sequence-unit", "-default-ann")
	       ; Control.Elaborate.default Control.Elaborate.sequenceUnit := b))),
       (Normal, "show-basis", " <file>", "write out the final basis environment",
	SpaceString (fn s => showBasis := SOME s)),
       (Normal, "show-def-use", " <file>", "write def-use information",
	SpaceString (fn s => showDefUse := SOME s)),
       (Expert, "show-types", " {false|true}", "show types in ILs",
	boolRef showTypes),
       (Expert, "ssa-passes", " <passes>", "ssa optimization passes",
	SpaceString
	(fn s =>
	 case !Control.ssaPassesSet s of
	    Result.Yes ss => Control.ssaPasses := ss
	  | Result.No s' => usage (concat ["invalid -ssa-pass arg: ", s']))),
       (Expert, "stack-cont", " {false|true}",
	"force continuation formals to stack",
	boolRef stackCont),
       (Normal, "stop", " {f|g|o|sml|tc}", "where to stop",
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
	 case !Control.sxmlPassesSet s of
	    Result.Yes ss => Control.sxmlPasses := ss
	  | Result.No s' => usage (concat ["invalid -sxml-pass arg: ", s']))),
       (Normal, "target",
	concat [" {",
		concat (List.separate (List.map (targetMap (), #target), "|")),
		"}"],
	"platform that executable will run on",
	SpaceString (fn s =>
		     (setTargetType (s, usage)
		      ; target := (if s = "self" then Self else Cross s)))),
       (Expert, "target-cc-opt", " <target> <opt>", "target-dependent CC option",
	(SpaceString2
	 (fn (target, opt) =>
	  List.push (ccOpts, {opt = opt, pred = OptPred.Target target})))),
       (Expert, "target-link-opt", " <target> <opt>",
	"target-dependent link option",
	(SpaceString2
	 (fn (target, opt) =>
	  List.push (linkOpts, {opt = opt, pred = OptPred.Target target})))),
       (Expert, #1 trace, " name1,...", "trace compiler internals", #2 trace),
       (Expert, "text-io-buf-size", " <n>", "TextIO buffer size",
	intRef textIOBufSize),
       (Expert, "type-check", " {false|true}", "type check ILs",
	boolRef typeCheck),
       (Expert, "type-error", " {concise|full}", "type error verbosity",
	SpaceString
	(fn s =>
	 typeError := (case s of
			  "concise" => Concise
			| "full" => Full
			| _ => usage (concat
				      ["invalid -type-error arg: ", s])))),
       (Normal, "verbose", " {0|1|2|3}", "how verbose to be",
	SpaceString
	(fn s =>
	 verbosity := (case s of
			  "0" => Silent
			| "1" => Top
			| "2" => Pass
			| "3" =>  Detail
			| _ => usage (concat ["invalid -verbose arg: ", s])))),
       (Expert, "warn-ann", " {true|false}",
	"unrecognized annotation warnings",
	boolRef warnAnn),
       (Expert, "warn-match", " {true|false}",
	"nonexhaustive and redundant match warnings",
	Bool (fn b =>
	      (warnDeprecated ("warn-match", "-default-ann")
	       ; Control.Elaborate.default Control.Elaborate.warnMatch := b))),
       (Expert, "warn-unused", " {false|true}",
	"unused identifier warnings",
	Bool (fn b =>
	      (warnDeprecated ("warn-unused", "-default-ann")
	       ; Control.Elaborate.default Control.Elaborate.warnUnused := b))),
       (Expert, "xml-passes", " <passes>", "xml optimization passes",
	SpaceString
	(fn s =>
	 case !Control.xmlPassesSet s of
	    Result.Yes ss => Control.xmlPasses := ss
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
	       (libDir := lib
		; args)
	  | _ => Error.bug "incorrect args from shell script"
      val _ = setTargetType ("self", usage)
      val result = parse args
      val gcc = !gcc
      val stop = !stop
      val target = !target
      val targetStr =
	 case target of
	    Cross s => s
	  | Self => "self"
      val _ = libTargetDir := concat [!libDir, "/", targetStr]
      val targetArch = !targetArch
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
      val ccOpts = addTargetOpts ccOpts
      val linkWithGmp =
	 case targetOS of
	    Linux =>
	       (* This mess is necessary because the linker on linux
		* adds a dependency to a shared library even if there are
		* no references to it.  So, on linux, we explicitly link
		* with libgmp.a instead of using -lgmp.
		*)
	       let
		  val conf = "/etc/ld.so.conf"
		  val dirs = if File.canRead conf then File.lines conf else []
		  val dirs = "/lib\n" :: "/usr/lib\n" :: dirs
	       in
		  case (List.peekMap
			(dirs, fn d =>
			 let
			    val lib =
			       concat [String.dropSuffix (d, 1), "/libgmp.a"]
			 in
			    if File.canRead lib
			       then SOME lib
			    else NONE
			 end)) of
		     NONE => ["-lgmp"]
		   | SOME lib => [lib]
	       end
	  | _ => []
      val linkOpts =
	 List.concat [[concat ["-L", !libTargetDir],
		       if !debug then "-lmlton-gdb" else "-lmlton"],
		      linkWithGmp,
		      addTargetOpts linkOpts]
      val _ =
	 if !Control.codegen = Native andalso targetArch = Sparc
	    then usage "can't use native codegen on Sparc"
	 else ()
      val _ =
	 chunk :=
	 (case !Control.codegen of
	     Bytecode => OneChunk
	   | CCodegen => Coalesce {limit = (case !coalesce of
					       NONE => 4096
					     | SOME n => n)}
	   | Native =>
		if isSome (!coalesce)
		   then usage "can't use -coalesce and -native true"
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
	 orelse !(Control.Elaborate.enabled Control.Elaborate.warnUnused)
	 orelse !(Control.Elaborate.default Control.Elaborate.warnUnused)
      val warnMatch =
	 !(Control.Elaborate.enabled Control.Elaborate.warnMatch)
	 orelse !(Control.Elaborate.default Control.Elaborate.warnMatch)
      val _ = elaborateOnly := (stop = Place.TypeCheck
				andalso not (warnMatch)
				andalso not (keepDefUse))
      val _ =
	 case targetOS of
	    FreeBSD => ()
	  | Linux => ()
	  | NetBSD => ()
	  | OpenBSD => ()
	  | Solaris => ()
	  | _ =>
	       if !profile = ProfileTime
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
			   if String.isSuffix {string = input, suffix = suf}
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
		if List.exists ([".c", ".o", ".s", ".S"], fn suffix =>
				String.isSuffix {string = f, suffix = suffix})
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
			      case targetOS of
				 MinGW => ("TEMP", "C:/WINNT/TEMP")
			       | _ => ("TMPDIR", "/tmp")
			in
			   case Process.getEnv tmpVar of
			      NONE => default
			    | SOME d => d
			end
		     fun temp (suf: string): File.t =
			let
			   val (f, out) =
			      File.temp {prefix = concat [tmpDir, "/file"],
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
		     fun compileO (inputs: File.t list): unit =
			let
			   val output = maybeOut ""
			   val _ =
			      trace (Top, "Link")
			      (fn () =>
			       System.system
			       (gcc,
				List.concat
				[["-o", output],
				 (case target of
				     Cross s => ["-b", s]
				   | Self => []),
				 if !debug then gccDebug else [],
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
		  fun compileCSO (inputs: File.t list): unit =
		     if List.forall (inputs, fn f =>
				     SOME "o" = File.extension f)
			then compileO inputs
		     else
		     let
			val c = Counter.new 0
			val oFiles =
			   trace (Top, "Compile C and Assemble")
			   (fn () =>
			    List.fold
			    (inputs, [], fn (input, ac) =>
			     let
				val extension = File.extension input
			     in
				if SOME "o" = extension
				   then input :: ac
				else
				   let
				      val (debugSwitches, switches) =
					 if SOME "c" = extension
					    then
					       (gccDebug @ ["-DASSERT=1"],
						ccOpts)
					 else ([asDebug], [])
				      val switches =
					 if !debug
					    then debugSwitches @ switches
					 else switches
				      val switches =
					 case target of
					    Cross s => "-b" :: s :: switches
					  | Self => switches
				      val switches = "-c" :: switches
				      val output =
					 if stop = Place.O orelse !keepO
					    then
					       if !keepGenerated 
						  orelse start = Place.Generated
						  then
						     concat [String.dropSuffix
							     (input, 1),
							     "o"]
					       else 
						  suffix
						  (concat [".",
							   Int.toString
							   (Counter.next c),
							   ".o"])
					 else temp ".o"
				      val _ =
					 System.system
					 (gcc,
					  List.concat [switches,
						       ["-o", output, input]])

				   in
				      output :: ac
				   end
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
			      List.foreach
			      (files, fn f => print (concat [f, "\n"]))
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
				 Vector.foreach
				 (Compile.sourceFilesMLB {input = file}, fn f => 
				  print (concat [f, "\n"]))
			    | Place.SML => saveSML (maybeOut ".sml")
			    | Place.TypeCheck =>
				 trace (Top, "Type Check SML")
				 Compile.elaborateMLB {input = file}
			    | _ => 
				 trace (Top, "Compile SML")
				 Compile.compileMLB
				 {input = file,
				  outputC = make (Control.C, ".c"),
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
		      | Place.SML => compileSml [input]
		      | Place.MLB => compileMLB input
		      | Place.Generated => compileCSO (input :: csoFiles)
		      | Place.O => compileCSO (input :: csoFiles)
		      | _ => Error.bug "invalid start"
		  val doit 
		    = trace (Top, "MLton")
		      (fn () => 
		       DynamicWind.wind
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
