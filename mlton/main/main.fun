(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
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
      datatype t = CM | Files | Generated | O | OUT | SML | TypeCheck

      val toInt: t -> int =
	 fn CM => 0
	  | Files => 1
	  | SML => 2
	  | TypeCheck => 3
	  | Generated => 4
	  | O => 5
	  | OUT => 6

      val toString =
	 fn CM => "cm"
	  | Files => "files"
	  | SML => "sml"
	  | Generated => "g"
	  | O => "o"
	  | OUT => "out"
	  | TypeCheck => "tc"

      val layout = Layout.str o toString

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
val showBasis: bool ref = ref false
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
		  Sparc =>
		     (align := Align8
		      ; Native.native := false)
		| _ => ())
	 end
   
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
       (Normal, "basis", " {2002|1997|...}",
	"select basis library to prefix to the program",
	SpaceString (fn s =>
		     let
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
       (Expert, "debug", " {false|true}", "produce executable with debug info",
	boolRef debug),
       (Normal, "detect-overflow", " {true|false}",
	"overflow checking on integer arithmetic",
	boolRef detectOverflow),
       (Expert, "diag-pass", " <pass>", "keep diagnostic info for pass",
	SpaceString (fn s =>
		     (case Regexp.fromString s of
			 SOME (re,_) => let val re = Regexp.compileDFA re
					in 
					   List.push (keepDiagnostics, re)
					   ; List.push (keepPasses, re)
					end
		       | NONE => usage (concat ["invalid -diag-pass flag: ", s])))),
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
       (Normal, "exn-history", " {false|true}",
	"enable Exn.history",
	boolRef exnHistory),
       (Expert, "expert", " {false|true}",
	"enable expert status",
	boolRef expert),
       (Normal, "export-header", " {false|true}",
	"output header file for _export's",
	boolRef exportHeader),
       (Expert, "gc-check", " {limit|first|every}", "force GCs",
	SpaceString (fn s =>
		     gcCheck :=
		     (case s of
			 "limit" => Limit
		       | "first" => First
		       | "every" => Every
		       | _ => usage (concat ["invalid -gc-check flag: ", s])))),
       (Expert, "handlers", " {flow|pushpop|simple}",
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
       (Normal, "inline", " <n>", "inlining threshold", Int setInlineSize),
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
       (Normal, "native",
	if !targetArch = Sparc then " {false}" else " {true|false}",
	"use native code generator",
	boolRef Native.native),
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
       (Expert, "polyvariance", " {true|false}", "use polyvariance",
	Bool (fn b => if b then () else polyvariance := NONE)),
       (Normal, "output", " <file>", "name of output file",
	SpaceString (fn s => output := SOME s)),
       (Normal, "profile", " {no|alloc|time}",
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
			    | "time" => ProfileTime
			    | _ => usage (concat
					  ["invalid -profile arg: ", s]))))),
       (Expert, "profile-basis", " {false|true}",
	"profile the basis implementation",
	boolRef profileBasis),
       (Expert, "profile-il", " {source}", "where to insert profile exps",
	SpaceString
	(fn s =>
	 case s of
	    "source" => profileIL := ProfileSource
	  | _ => usage (concat ["invalid -profile-il arg: ", s]))),
       (Normal, "profile-stack", " {false|true}", "profile the stack",
	boolRef profileStack),
       (Normal, "runtime", " <arg>", "pass arg to runtime via @MLton",
	push runtimeArgs),
       (Normal, "safe", " {true|false}", "bounds checking and other checks",
	boolRef safe),
       (Normal, "show-basis", " {false|true}", "display the basis library",
	boolRef showBasis),
       (Normal, "show-basis-used", " {false|true}",
	"display the basis library used by the program",
	boolRef showBasisUsed),
       (Expert, "show-types", " {false|true}", "print types in ILs",
	boolRef showTypes),
       (Expert, "stack-cont", " {false|true}",
	"force continuation formals to stack",
	boolRef stackCont),
       (Normal, "static", " {false|true}",
	"produce a statically linked executable",
	boolRef static),
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
       (Normal, "verbose", " {0|1|2|3}", "how verbose to be",
	SpaceString
	(fn s =>
	 verbosity := (case s of
			  "0" => Silent
			| "1" => Top
			| "2" => Pass
			| "3" =>  Detail
			| _ => usage (concat ["invalid -verbose arg: ", s])))),
       (Expert, "variant", " {header|first-word}",
	"how to represent variant tags",
	SpaceString
	(fn s =>
	 variant := (case s of
			"first-word" => FirstWord
		      | "header" => Header
		      | _ => usage (concat ["invalid -variant arg: ", s])))),
       (Normal, "warn-match", " {true|false}",
	"nonexhaustive and redundant match warnings",
	Bool (fn b => (warnNonExhaustive := b; warnRedundant := b)))
       ],
       fn (style, name, arg, desc, opt) =>
       {arg = arg, desc = desc, name = name, opt = opt, style = style})
   end

val mainUsage =
   "mlton [option ...] file.{cm|sml|c|o} [file.{c|S|o} ...]"

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
	    lib :: args => (libDir := lib; args)
	  | _ => Error.bug "incorrect args from shell script"
      val _ = setTargetType ("self", usage)
      val result = parse args
      val gcc = !gcc
      val target = !target
      val targetStr =
	 case target of
	    Cross s => s
	  | Self => "self"
      val _ = libTargetDir := concat [!libDir, "/", targetStr]
      val targetArch = !targetArch
      val archStr = MLton.Platform.Arch.toString targetArch
      val targetOS = !targetOS
      val OSStr = MLton.Platform.OS.toString targetOS
      fun tokenize l =
	 String.tokens (concat (List.separate (l, " ")), Char.isSpace)
      fun addTargetOpts opts =
	 tokenize
	 (List.fold
	  (!opts, [], fn ({opt, pred}, ac) =>
	   if (case pred of
		  OptPred.Target s => s = archStr orelse s = OSStr
		| OptPred.Yes => true)
	      then opt :: ac
	   else ac))
      val ccOpts = addTargetOpts ccOpts
      val linkOpts = addTargetOpts linkOpts
      datatype z = datatype MLton.Platform.OS.t
      val linkWithGmp =
	 case targetOS of
	    Cygwin => ["-lgmp"]
	  | FreeBSD => ["-L/usr/local/lib/", "-lgmp"]
	  | Linux =>
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
	  | NetBSD => ["-Wl,-R/usr/pkg/lib", "-L/usr/pkg/lib", "-lgmp"]
	  | Sun => ["-lgmp"]
      val linkOpts =
	 List.concat [[concat ["-L", !libTargetDir],
		       if !debug then "-lmlton-gdb" else "-lmlton"],
		      linkWithGmp,
		      linkOpts]
      val _ =
	 if !Native.native andalso targetArch = Sparc
	    then usage "can't use -native true on Sparc"
	 else ()
      val _ =
	 chunk := (if !Native.native
		      then
			 if isSome (!coalesce)
			    then usage "can't use -coalesce and -native true"
			 else ChunkPerFunc
		   else Coalesce {limit = (case !coalesce of
					      NONE => 4096
					    | SOME n => n)})
      val _ = if not (!Native.native) andalso !Native.IEEEFP
		 then usage "can't use -native false and -ieee-fp true"
	      else ()
      val _ =
	 if !keepDot andalso List.isEmpty (!keepPasses)
	    then keepSSA := true
	 else ()
      val _ =
	 if targetOS = Cygwin andalso !profile = ProfileTime
	    then usage "can't use -profile time on Cygwin"
	 else ()
      fun printVersion (out: Out.t): unit =
	 Out.output (out, concat [version, " ", build, "\n"])
   in
      case result of
      Result.No msg => usage msg
    | Result.Yes [] =>
	 (case !verbosity of
	     Silent =>
		if !showBasis
		   then Layout.outputl (Compile.layoutBasisLibrary (),
					Out.standard)
		else if !buildConstants
		   then Compile.outputBasisConstants Out.standard
	        else printVersion Out.standard
	   | Top => printVersion Out.standard
	   | _ => (inputFile := ""
		   ; outputHeader' (No, Out.standard)))
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
		  loop [(".cm", CM, false),
			(".sml", SML, false),
			(".c", Generated, true),
			(".o", O, true)]
	       end
	    val _ =
	       List.foreach
	       (rest, fn f =>
		if List.exists ([".c", ".o", ".s", ".S"], fn suffix =>
				String.isSuffix {string = f, suffix = suffix})
		   andalso File.canRead f
		   then ()
		else usage (concat ["invalid file: ", f]))
	    val csoFiles = rest
	    val stop = !stop
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
			case Process.getEnv "TMPDIR" of
			   NONE => "/tmp"
			 | SOME d => d
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
				 if !static then ["-static"] else [],
				 inputs,
				 linkOpts]))
			      ()
			   (* gcc on Cygwin appends .exe, which I don't want, so
			    * move the output file to it's rightful place.
			    * Notice that we do not use targetOS here, since we
			    * care about the platform we're running on, not the
			    * platform we're generating for.
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
				 Compile.typeCheck {input = files}
			    | _ => 
				 trace (Top, "Compile SML")
				 Compile.compile
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
		  fun compile () =
		     case start of
			Place.CM => compileCM input
		      | Place.SML => compileSml [input]
		      | Place.Generated => compileCSO (input :: csoFiles)
		      | Place.O => compileCSO (input :: csoFiles)
		      | _ => Error.bug "invalid start"
		  val doit 
		    = trace (Top, "MLton")
		      (fn () => 
		       DynamicWind.wind
		       (compile, fn () =>
			List.foreach (!tempFiles, File.remove)))
	       in doit ()
	       end
	 end
   end

val commandLine = Process.makeCommandLine commandLine
   
fun exportNJ (root: Dir.t, file: File.t): unit =
   (Compile.forceBasisLibrary root
    ; SMLofNJ.exportFn (file, fn (_, args) => commandLine args))
   
fun exportMLton (): unit =
   case CommandLine.arguments () of
      [root, file] => exportNJ (root, file)
    | _ => Error.bug "usage: exportMLton root file"

val _ =
   let
      open Trace.Immediate
   in
      debug := Out Out.error
      ; flagged ()
(*      ; on ["admitsEquality"] *)
(*      ; on ["elaborateDec"] *)
(*      ; on ["extendVar"] *)
(*      ; on ["elaborateExp"] *)
(*      ; on ["unify", "Scheme.instantiate"] *)
   end
end
