(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Main: MAIN =
struct

type int = Int.t

structure Place =
   struct
      datatype t = CM | Files | Generated | O | OUT | SML

      val toInt: t -> int =
	 fn CM => 0
	  | Files => 1
	  | SML => 2
	  | Generated => 3
	  | O => 4
	  | OUT => 5

      val toString =
	 fn CM => "cm"
	  | Files => "files"
	  | SML => "sml"
	  | Generated => "g"
	  | O => "o"
	  | OUT => "out"

      val layout = Layout.str o toString

      fun compare (p, p') = Int.compare (toInt p, toInt p')
   end

val buildConstants: bool ref = ref false
val coalesce: int option ref = ref NONE
val includeDirs: string list ref = ref []
val keepGenerated = ref false
val keepO = ref false
val keepSML = ref false
val libs: string list ref = ref []
val libDirs: string list ref = ref []
val output: string option ref = ref NONE
val optimization: int ref = ref 1
val showBasis: bool ref = ref false
val stop = ref Place.OUT

val usageRef: (string -> unit) option ref = ref NONE

fun usage (s: string): 'a =
   (valOf (!usageRef) s
    ; let open OS.Process
      in if MLton.isMLton
	    then exit failure
	 else raise Fail "failure"
      end)

datatype optionStyle = Normal | Expert

val libRef: Dir.t option ref = ref NONE
fun getLib (): Dir.t =
   case !libRef of
      NONE => Error.bug "lib not set"
    | SOME l => l

val hostMap: unit -> {host: string, hostType: Control.hostType} list =
   Promise.lazy
   (fn () =>
    List.map
    (File.lines (concat [getLib (), "/hostmap"]), fn line =>
     case String.tokens (line, Char.isSpace) of
	[host, hostType] =>
	   {host = host,
	    hostType =
	    (case hostType of
		"cygwin" => Control.Cygwin
	      | "linux" => Control.Linux
	      | _ => Error.bug (concat ["strange hostType: ",
					hostType]))}
      | _ => Error.bug (concat ["strange host mapping: ", line])))
   
fun options () = 
   let
      open Control Popt
      fun push r = String (fn s => List.push (r, s))
   in [
       (Expert, "build-constants", "",
	"output C file that prints basis constants",
	trueRef buildConstants),
       (Expert, "coalesce", " n", "coalesce chunk size for C codegen",
	Int (fn n => coalesce := SOME n)),
       (Expert, "debug", " {false|true}", "produce executable with debug info",
	boolRef debug),
       (Normal, "detect-overflow", " {true|false}",
	"overflow checking on integer arithmetic",
	boolRef detectOverflow),
       (Expert, "diag", " pass", "keep diagnostic info for pass",
	SpaceString (fn s => (case Regexp.fromString s of
				 SOME (re,_) => let val re = Regexp.compileDFA re
						in 
						   List.push (keepDiagnostics, re)
						   ; List.push (keepPasses, re)
						end
			       | NONE => usage (concat ["invalid -diag flag: ", s])))),
       (Expert, "drop-pass", " pass", "omit optimization pass",
	SpaceString (fn s => List.push (dropPasses, s))),
       (Expert, "D", "define", "define a constant for gcc",
	String (fn s => (List.push (defines, s)))),
       (Normal, "exn-history", " {false|true}",
	"enable Exn.history",
	boolRef Control.exnHistory),
       (Normal, "fixed-heap", " n[{k|m}]",
	"heap size used by resulting executable",
	Mem (fn n => fixedHeap := SOME n)),
       (Expert, "gc-check", " {limit|first|every}", "force GCs",
	SpaceString (fn s =>
		     gcCheck :=
		     (case s of
			 "limit" => Limit
		       | "first" => First
		       | "every" => Every
		       | _ => usage (concat ["invalid -gc-check flag: ", s])))),
       (Normal, "host",
	concat [" {",
		concat (List.separate (List.map (hostMap (), #host), "|")),
		"}"],
	"host type that executable will run on",
	SpaceString (fn s => host := (if s = "self" then Self else Cross s))),
       (Normal, "ieee-fp", " {false|true}", "use strict IEEE floating-point",
	boolRef Native.IEEEFP),
       (Expert, "indentation", " n", "indentation level in ILs",
	intRef indentation),
(*        (Normal, "include", " file.h", "include a .h file",
 * 	SpaceString (fn s => List.push (includes, s))),
 *)
       (Normal, "inline", " n", "inlining threshold",
	Int setInlineSize),
       (Expert, "inline-array", " {true|false}",
	"inline array allocation",
	boolRef inlineArrayAllocation),
(*        (Normal, "I", "dir", "search dir for include files",
 * 	push includeDirs),
 *)
       (Normal, "keep", " {dot|g|o|sml|ssa}", "save intermediate files",
	SpaceString (fn s =>
		     case s of
			"dot" => keepDot := true
		      | "g" => keepGenerated := true
		      | "o" => keepO := true
		      | "sml" => keepSML := true
		      | "ssa" => keepSSA := true
		      | _ => usage (concat ["invalid -keep flag: ", s]))),
       (Expert, "keep-pass", " pass", "keep the results of pass",
	SpaceString
	(fn s => (case Regexp.fromString s of
		     SOME (re,_) => let val re = Regexp.compileDFA re
				    in List.push (keepPasses, re)
				    end
		   | NONE => usage (concat ["invalid -keep-pass flag: ", s])))),
       (Normal, "l", "library", "link with library", push libs),
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
       (Expert, "loop-passes", " n", "loop optimization passes (1)",
	Int 
	(fn i => 
	 if i >= 1
	    then loopPasses := i
	    else usage (concat ["invalid -loop-passes arg: ", Int.toString i]))),
       (Normal, "L", "dir", "search dir for libraries",
	push libDirs),
       (Normal, "native", " {true|false}", "use native x86 code generator",
	boolRef Native.native),
       (Expert, "native-commented", " n", "level of comments  (0)",
	intRef Native.commented),
       (Expert, "native-copy-prop", " {true|false}", 
	"use copy propagation",
	boolRef Native.copyProp),
       (Expert, "native-cutoff", " n", 
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
       (Expert, "native-optimize", " n", "level of optimizations",
        intRef Native.optimize),
       (Expert, "native-split", " n", "split assembly files at ~n lines",
	Int (fn i => Native.split := SOME i)),
       (Expert, "new-return", " {false|true}", "non-tail call return convention",
	boolRef newReturn),
       (Normal, "polyvariance", " {true|false}", "use polyvariance",
	Bool (fn b => if b then () else polyvariance := NONE)),
       (Normal, "o", " file", "name of output file",
	SpaceString (fn s => output := SOME s)),
       (Expert, "O", "digit", "gcc optimization level",
	Digit (fn d => optimization := d)),
       (Normal, "profile", " {false|true}",
	"produce executable suitable for profiling",
	Bool (fn b => if b then (profile := true; keepSSA := true) else ())),
       (Expert, "print-at-fun-entry", " {false|true}",
	"print debugging message at every call",
	boolRef printAtFunEntry),
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
       (Normal, "stop", " {f|g|o|sml}", "where to stop",
	SpaceString
	(fn s =>
	 stop := (case s of
		     "f" => Place.Files
		   | "g" => Place.Generated	
		   | "o" => Place.O
		   | "sml" => Place.SML
		   | _ => usage (concat ["invalid -stop arg: ", s])))),
       (Expert, #1 trace, " name1,...", "trace compiler internals", #2 trace),
       (Expert, "text-io-buf-size", " n", "TextIO buffer size",
	intRef textIOBufSize),
       (Expert, "type-check", " {false|true}", "type check ILs",
	boolRef typeCheck),
       (Expert, "use-basis-library", " {true|false}",
	"prefix the basis library to the program",
	boolRef useBasisLibrary),
       (Normal, "v", "[0123]", "how verbose to be about compiler passes",
	String
	(fn s =>
	 verbosity := (case s of
			  "" => Top
			| "0" => Silent
			| "1" => Top
			| "2" => Pass
			| "3" =>  Detail
			| _ => usage (concat ["invalid -v arg: ", s]))))
      ]
   end

val _ =
   usageRef :=
   SOME
   (fn s =>
    let
       fun message s = Out.output (Out.error, s)
       val opts =
	  List.fold
	  (rev (options ()), [], fn ((style, opt, arg, desc, _), rest) =>
	   if style = Normal
	      orelse let open Control
		     in !verbosity <> Silent
		     end
	      then [concat ["    -", opt, arg, " "], desc] :: rest
	   else rest)
       val table =
	  let open Justify
	  in table {justs = [Left, Left],
		    rows = opts}
	  end
    in
       message s
       ; (message
	  "\nusage: mlton [option ...] file.{cm|sml|c|o} [file.{S|o} ...] [library ...]\n")
       ; List.foreach (table, fn ss =>
		       message (concat [String.removeTrailing
					(concat ss, Char.isSpace),
					"\n"]))
    end)

fun commandLine (args: string list): unit =
   let
      open Control
      fun error () = Error.bug "incorrect args from shell script"
      val (lib, gcc, gccSwitches, args) =
	 case args of
	    lib :: gcc :: args =>
	       let
		  fun loop (args, ac) =
		     case args of
			[] => error ()
		      | arg :: args =>
			   if arg = "END"
			      then (rev ac, args)
			   else loop (args, arg :: ac)
		  val (gccSwitches, args) = loop (args, [])
	       in
		  (lib, gcc, gccSwitches, args)
	       end
	  | _ => error ()
      val _ = libRef := SOME lib
      val result =
	 Popt.parse {switches = args,
		     opts = List.map (options (), fn (_, a, _, _, c) => (a, c))}
      val host = !host
      val hostString =
	 case host of
	    Cross s => s
	  | Self => "self"
      val lib = concat [lib, "/", hostString]
      val _ = Control.libDir := lib
      val libDirs = lib :: !libDirs
      val includeDirs = concat [lib, "/include"] :: !includeDirs
      val _ =
	 case List.peek (hostMap (), fn {host = h, ...} => h = hostString) of
	    NONE => usage (concat ["invalid host ", hostString])
	  | SOME {hostType = t, ...} => hostType := t
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
   in case result of
      Result.No switch => usage (concat ["invalid switch: ", switch])
    | Result.Yes [] =>
	 (case !verbosity of
	     Silent =>
		if !showBasis
		   then Layout.outputl (Compile.layoutBasisLibrary (),
					Out.standard)
		else if !buildConstants
			then Compile.outputBasisConstants Out.standard
		     else usage "must supply a file"
	   | _ => 
		(inputFile := ""
		 ; outputHeader' (No, Out.standard)))
    | Result.Yes (input :: rest) =>
	 let
	    val _ = inputFile := (File.base o File.fileOf) input
	    val (start, base) =
	       let
		  val rec loop =
		     fn [] => usage (concat ["invalid file suffix on ", input])
		      | (suf, start) :: sufs =>
			   if String.isSuffix {string = input, suffix = suf}
			      then (start,
				    String.dropSuffix (File.fileOf input, 
						       String.size suf))
			   else loop sufs
		  datatype z = datatype Place.t
	       in loop [(".cm", CM),
			(".sml", SML),
			(".c", Generated),
			(".o", O)]
	       end
	    val (sfiles, rest) =
	       case start of
		  Place.Generated =>
		     List.splitPrefix (rest, fn s => 
				       String.isSuffix {string = s,
							suffix = ".S"})
		| _ => ([], rest)
	    val stop = !stop
	 in case Place.compare (start, stop) of
	    GREATER => usage (concat ["cannot go from ", Place.toString start,
				      " to ", Place.toString stop])
	  | EQUAL => usage "nothing to do"
	  | LESS =>
	       let
		  val tempFiles: File.t list ref = ref []
		  fun temp (suf: string): File.t =
		     let
			val (f, out) = File.temp {prefix = "/tmp/file",
						  suffix = suf}
			val _ = Out.close out
			val _ = List.push (tempFiles, f)
		     in
			f
		     end
		  fun suffix s = concat [base, s]
		  fun file (b, suf) = (if b then suffix else temp) suf
		  fun maybeOut suf =
		     case !output of
			NONE => suffix suf
		      | SOME f => f
		  fun list (prefix: string, l: string list): string list =
		     List.map (l, fn s => prefix ^ s)
		  fun docc (inputs: File.t list,
			    output: File.t,
			    switches: string list,
			    linkLibs: string list) =
		     System.system
		     (gcc, List.concat [switches,
					["-o", output],
					inputs,
					linkLibs])
		  val definesAndIncludes =
		     List.concat [list ("-D", !defines),
				  list ("-I", rev (includeDirs))]
		  (* This mess is necessary because the linker on linux
		   * adds a dependency to a shared library even if there are
		   * no references to it.  So, on linux, we explicitly link
		   * with libgmp.a instead of using -lgmp.
		   *)
		  val linkWithGmp =
		     case !hostType of
			Cygwin => "-lgmp"
		      | Linux =>
			   case (List.peekMap
				 (File.lines "/etc/ld.so.conf", fn d =>
				  let
				     val lib = concat [String.dropSuffix (d, 1),
						       "/libgmp.a"]
				  in
				     if File.canRead lib
					then SOME lib
				     else NONE
				  end)) of
			      NONE => "-lgmp"
			    | SOME lib => lib
		  val linkLibs: string list =
		     List.concat [list ("-L", rev (libDirs)),
				  list ("-l",
					(if !debug
					    then "mlton-gdb"
					 else "mlton")
					    :: !libs),
				  [linkWithGmp]]
		  fun compileO (inputs: File.t list) =
		     let
			val output = maybeOut ""
			val _ =
			   trace (Top, "Link")
			   (fn () =>
			    docc (inputs, output,
				  List.concat
				  [case host of
				      Cross s => ["-b", s]
				    | Self => [],
					 if !debug then ["-g"] else [],
					    if !static then ["-static"] else []],
				  rest @ linkLibs))
			   ()
			(* gcc on Cygwin appends .exe, which I don't want, so
			 * move the output file to it's rightful place.
			 *)
			val _ =
			   case MLton.hostType of
			      MLton.Cygwin =>
				 if String.contains (output, #".")
				    then ()
				 else
				    File.move {from = concat [output, ".exe"],
					       to = output}
			    | MLton.Linux => ()
		     in
			()
		     end
		  fun compileS (main: File.t, inputs: File.t list) =
		     let
			val switches = ["-c"]
			val r = ref 0
			fun doit (input: File.t, isMain: bool): File.t =
			   let
			      val switches =
				 if !debug
				    then
				       (* The -Wa,--gstabs says to pass the
					* --gstabs option to the assembler.
					* This tells the assembler to generate
					* stabs debugging information for each
					* assembler line.
					*)
				       (if isMain then "-g" else "-Wa,--gstabs")
					   :: switches
				 else switches
			      val switches =
				 case host of
				    Cross s => "-b" :: s :: switches
				  | Self => switches
			      val output =
				 if stop = Place.O orelse !keepO
				    then
				       if isMain
					  then suffix ".o"
				       else
					  if !keepGenerated
					     then
						concat
						[String.dropSuffix (input, 1),
						 "o"]
					  else
					     suffix
					     (Int.inc r
					      ; concat [".", Int.toString (!r),
							".o"])
				 else temp ".o"
			   in docc ([input], output, switches, [])
			      ; output
			   end
			val outputs =
			   trace (Top, "Assemble")
			   (fn () =>
			    doit (main, true)
			    :: List.revMap (inputs, fn i => doit (i, false)))
			   ()
		     in case stop of
			Place.O => ()
		      | _ => compileO outputs
		     end
		  fun compileC (cFile: File.t,
				sFiles: File.t list) =
		     let
			val switches =
			   List.concat
			   [["-S"],
			    if !debug then ["-g"] else [],
			    definesAndIncludes,
			    [concat ["-O", Int.toString (!optimization)]],
			    if !Native.native
			       then []
			    else gccSwitches]
			val switches =
			   case host of
			      Cross s => "-b" :: s :: switches
			    | Self => switches
			val output = temp ".s"
			val _ =
			   trace (Top, "Compile C")
			   (fn () => docc ([cFile], output, switches, []))
			   ()
		     in compileS (output, sFiles)
		     end
		  fun compileSml (files: File.t list) =
		     let
			val docc =
			   fn {input, output} =>
			   docc ([input], output, definesAndIncludes, linkLibs)
			val cFile = ref NONE
			val sFiles = ref []
			fun cOut () =
			   let
			      val suf = ".c"
			      val file = 
				 case stop of
				    Place.Generated => maybeOut suf
				  | _ => file (!keepGenerated, suf)
			   in cFile := SOME file
			      ; file
			   end
			val r = ref 0
			fun sOut () =
			   let
			      val suf = concat [".",
                                                Int.toString (!r),
                                                if !Control.debug
                                                  then ".s"
                                                  else ".S"]
			      val file = (if !keepGenerated
					     orelse stop = Place.Generated
					     then suffix
					  else temp) suf
			      val _ = Int.inc r
			   in List.push (sFiles, file)
			      ; file
			   end
			fun make (style: style,
				  f: unit -> File.t) () =
			   let
			      val f = f ()
			      val out = Out.openOut f
			      fun print s = Out.output (out, s)
			      val _ = outputHeader' (style, out)
			      fun done () = Out.close out
			   in {file = f,
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
			   trace (Top, "Compile SML")
			   Compile.compile
			   {input = files,
			    docc = docc,
			    outputC = make (Control.C, cOut),
			    outputS = make (Control.Assembly, sOut)}
			(* These collects are so that the heap is shrunk
			 * as much as possible before calling gcc.
			 *)
			val _ = MLton.GC.collect ()
			val _ = MLton.GC.collect ()
		     in
			case stop of
			   Place.Generated => ()
			 | _ => compileC (valOf (!cFile), !sFiles)
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
				 let
				    fun ss (m: string) =
				       Out.output
				       (out,
					concat ["(* ", m, " of ", f, " *)\n"])
				 in ss "start"
				    ; File.outputContents (f, out)
				    ; ss "stop"
				 end))))
		     in case stop of
			Place.Files =>
			   List.foreach (files, fn f => print (concat [f, "\n"]))
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
		      | Place.Generated => compileC (input, sfiles)
		      | Place.O => compileO [input]
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

val commandLine =
   fn args =>
   ((commandLine args; OS.Process.success)
    handle e =>
       let
	  val out = Out.error
	  open Layout
       in
	  output (seq [str "mlton: ", Exn.layout e], out)
	  ; List.foreach (Exn.history e, fn s =>
			  (Out.output (out, "\n\t")
			   ; Out.output (out, s)))
	  ; Out.newline out
	  ; OS.Process.failure
       end)
   
fun exportNJ (root: Dir.t, file: File.t): unit =
   (Compile.forceBasisLibrary root
    ; SMLofNJ.exportFn (file, fn (_, args) => commandLine args))
   
fun exportMLton (): unit =
   case CommandLine.arguments () of
      [root, file] => exportNJ (root, file)
    | _ => Error.bug "usage: exportMLton root file"

end
