(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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

val detectOverflowSet = ref false
val includes: string list ref = ref []
val includeDirs: string list ref = ref []
val keepGenerated = ref false
val keepO = ref false
val keepSML = ref false
val lib = ref "mlton"
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

val options = 
   let
      open Control Popt
      fun push r = String (fn s => List.push (r, s))
   in [
       (Normal,
	"detect-overflow",
	" {true|false}",
	"overflow checking on integer arithmetic",
	Bool (fn b => (detectOverflowSet := true
		       ; detectOverflow := b))),
       (Normal, "D", "define", "define compile-time constant",
	String (fn s => (List.push (defines, s)
			 ; if s = "INSTRUMENT"
			      then instrument := true
			   else ()))),
       (Expert, "g", "", "produce executable with debug info",
	None (fn () => (debug := true
			; lib := "mlton-gdb"))),
       (Normal, "h", " heapSize [{k|m}]",
	"heap size used by resulting executable",
	Mem (fn n => fixedHeap := SOME n)),
       (Normal, "ieee-fp", " {false|true}", 
	"use strict IEEE floating-point",
	Bool (fn b => Native.IEEEFP := b)),
       (Expert, "indentation", " n", "indentation level in ILs",
	intRef indentation),
       (Normal, "include", " file.h", "include a .h file",
	SpaceString (fn s => List.push (includes, s))),
       (Normal, "inline", " n", "inlining threshold",
	Int setInlineSize),
       (Normal, "I", "dir", "search dir for include files",
	push includeDirs),
       (Normal, "keep", " {cps|dot|g|il|o|sml}", "save intermediate files",
	SpaceString (fn s =>
		     case s of
			"cps" => keepCps := true
		      | "dot" => (keepDot := true; keepCps := true)
		      | "g" => keepGenerated := true
		      | "il" => aux := true
		      | "mach" => keepMach := true
		      | "o" => keepO := true
		      | "sml" => keepSML := true
		      | _ => usage (concat ["invalid -keep flag: ", s]))),
       (Normal, "l", "library", "link with library", push libs),
       (Expert, "local-flatten", " {true|false}",
	"CPS local flattening optimization",
	boolRef Control.localFlatten),
       (Normal, "L", "dir", "search dir for libraries",
	push libDirs),
       (Normal, "native", " {true|false}", "use native x86 code generator",
	boolRef Native.native),
       (Expert, "native-commented", " n", "level of comments  (0)",
	Int (fn i => Native.commented := i)),
       (Expert, "native-copy-prop", " {true|false}", 
	"use copy propagation",
	Bool (fn b => Native.copyProp := b)),
       (Expert, "native-live-transfer", " {true|false}",
	"use live transfer",
	Bool (fn b => Control.Native.liveTransfer := b)),
       (Expert, "native-move-hoist", " {true|false}",
	"use move hoisting",
	Bool (fn b => Native.moveHoist := b)),
       (Expert, "native-optimize", " n", "level of optimizations",
        Int (fn i => Native.optimize := i)),
       (Expert, "native-split", " n", "split assembly files at ~n lines",
	Int (fn i => Native.split := SOME i)),
       (Normal, "polyvariance", " {true|false}", "use polyvariance",
	Bool (fn b => if b then () else polyvariance := NONE)),
       (Normal, "o", " file", "name of output file",
	SpaceString (fn s => output := SOME s)),
       (Normal, "p", "", "produce executable suitable for profiling",
	None (fn () => (profile := true
			; keepCps := true))),
       (Expert, "print-at-fun-entry", "", "print debuggin message at every call",
	trueRef printAtFunEntry),
       (Normal, "safe", " {true|false}", "bounds checking and other checks",
	boolRef safe),
       (Normal, "show-basis", "", "display the basis library",
	trueRef showBasis),
       (Expert, "show-types", " {false|true}", "print types in ILs",
	boolRef showTypes),
       (Normal, "static", "", "produce a statically linked executable",
	trueRef static),
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
       (Expert, "type-check", " {false|true}", "type check ILs",
	boolRef typeCheck),
       (Expert, "unused-args", " {true|false}",
	"CPS unusd args optimization",
	boolRef Control.localFlatten),
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
	  (rev options, [], fn ((style, opt, arg, desc, _), rest) =>
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
      val (root, gcc, gccSwitches, args) =
	 case args of
	    root :: gcc :: args =>
	       let
		  fun loop (args, ac) =
		     case args of
			[] => error ()
		      | arg :: args =>
			   if arg = "END"
			      then (rev ac, args)
			   else loop (args, arg :: ac)
		  val (gccSwitches, args) = loop (args, [])
	       in (root, gcc, gccSwitches, args)
	       end
	  | _ => error ()
      val result =
	 Popt.parse {switches = args,
		     opts = List.map (options, fn (_, a, _, _, c) => (a, c))}
      val _ =
	 if !Native.native
	    then chunk := ChunkPerFunc
	 else chunk := Coalesce {limit = 2000}
      val _ =
	 if !detectOverflowSet
	    then ()
	 else if !Native.native
		 then detectOverflow := true
	      else detectOverflow := false
      val _ =
	 List.push (defines,
		    concat ["MLton_detectOverflow=",
			    if !detectOverflow then "TRUE" else "FALSE"])
      val _ =
	 List.push (defines,
		    concat ["MLton_safe=", if !safe then "TRUE" else "FALSE"])
      val _ = if !debug then () else List.push (defines, "NODEBUG")
      val _ = if !aux andalso !keepDot
		 then usage "cannot use -keep dot and -keep il"
	      else ()
      val _ = Control.includes := !includes
   in case result of
      Result.No switch => usage (concat ["invalid switch: ", switch])
    | Result.Yes [] =>
	 (case !verbosity of
	     Silent =>
		if !showBasis
		   then let
			   val out = Out.standard
			in Layout.output (Compile.layoutBasisLibrary (), out)
			   ; Out.newline out
			end
		else usage "must supply a file"
	   | _ => 
		(inputFile := ""
		 ; outputHeader' (No, Out.standard)))
    | Result.Yes (input :: rest) =>
	 let
	    val _ = inputFile := input
	    val (start, base) =
	       let
		  val rec loop =
		     fn [] => usage (concat ["invalid file suffix on ", input])
		      | (suf, start) :: sufs =>
			   if String.isSuffix {string = input, suffix = suf}
			      then (start,
				    String.dropSuffix (input, String.size suf))
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
		     in List.push (tempFiles, f)
			; f
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
				  list ("-I", rev (!includeDirs))]
		  val linkLibs: string list =
		     List.concat [rest,
				  list ("-L", rev (!libDirs)),
				  list ("-l", !lib :: !libs)]
		  fun compileO (inputs: File.t list) =
		     trace (Top, "Link")
		     (fn () =>
		      docc (if !profile
			       then inputs @ [concat [root, "/lib/prof.o"]]
			    else inputs,
			    maybeOut "",
			    List.concat [if !debug then ["-g"] else [],
					 if !static then ["-static"] else []],
			    linkLibs))
		     ()
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
			    if !Native.native
			       then []
			    else
			    List.concat
			    [[concat ["-O", Int.toString (!optimization)]],
			     gccSwitches]]
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
		     in case stop of
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

fun exportNJ (root: Dir.t, file: File.t): unit =
   (Compile.forceBasisLibrary root
    ; (SMLofNJ.exportFn
       (file,
	fn (_, args) => ((commandLine args; OS.Process.success)
			 handle e =>
			    let open Layout
			    in output (seq [str "mlton: ", Exn.layout e],
				       Out.error)
			       ; Out.newline Out.error
			       ; OS.Process.failure
			    end))))
   
fun exportMLton (): unit =
   case CommandLine.arguments () of
      [root, file] => exportNJ (root, file)
    | _ => Error.bug "usage: exportMLton root file"

end
