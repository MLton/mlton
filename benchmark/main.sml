(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Main =
struct

type int = Int.t

val fail = Process.fail

val doHtml = ref false
val doOnce = ref false
val runArgs : string list ref = ref []

fun withInput (file, f: unit -> 'a): 'a =
   let
      open FileDesc
      val inFd =
	 let
	    open Pervasive.Posix.FileSys
	 in
	    openf (file, O_RDONLY, O.flags [])
	 end
   in
      DynamicWind.wind
      (fn () => FileDesc.fluidLet (FileDesc.stdin, inFd, f),
       fn () => FileDesc.close inFd)
   end

fun ignoreOutput f =
   let
      val nullFd =
	 let
	    open Pervasive.Posix.FileSys
	 in
	    openf ("/dev/null", O_WRONLY, O.flags [])
	 end
      open FileDesc
   in
      DynamicWind.wind
      (fn () => fluidLet (stderr, nullFd, fn () =>
			  fluidLet (stdout, nullFd, f)),
       fn () => close nullFd)
   end

fun timeIt (com, args) =
   Process.time (fn () =>
		 Process.wait
		 (Process.spawnp {file = com, args = com :: args}))

local
   val trialTime = Time.seconds (IntInf.fromInt 60)
in
   fun timeCall (com, args): real =
      let 
	 fun doit ac =
	    let
	       val {user, system} = timeIt (com, args)
	       val op + = Time.+
	    in ac + user + system
	    end
	 fun loop (n, ac: Time.t): real =
	    if Time.> (ac, trialTime)
	       then Time.toReal ac / Real.fromInt n
	    else loop (n + 1, doit ac)
      in 
	 if !doOnce
	    then Time.toReal (doit Time.zero)
	 else loop (0, Time.zero)
      end
end

fun compileSizeRun {args, compiler, exe, doTextPlusData: bool} =
   Escape.new
   (fn e =>
    let
       val exe = "./" ^ exe
       val {system, user} = timeIt (compiler, args)
	  handle _ => Escape.escape (e, {compile = NONE,
					 run = NONE,
					 size = NONE})
       val compile = SOME (Time.toReal (Time.+ (system, user)))
       val size =
	  if doTextPlusData
	     then
		let 
		   val {text, data, ...} = Process.size exe
		in SOME (text + data)
		end
	  else SOME (File.size exe)
       val run =
	  timeCall (exe, !runArgs)
	  handle _ => Escape.escape (e, {compile = compile,
					 run = NONE,
					 size = size})
    in {compile = compile,
	run = SOME run,
	size = size}
    end)

fun batch bench = concat [bench, ".batch.sml"]

local
  val n = Counter.new 0
  fun make (compiler, args) =
      let val exe = "a.out"
      in fn {bench} => compileSizeRun {args = args @ ["-o", exe, batch bench],
				       compiler = compiler,
				       exe = exe,
				       doTextPlusData = true}
      end
in
  val makeMLton
    = fn arg => let
		  open Regexp

		  val compilerSave = Save.new ()
		  val compiler = save (star (isChar (fn #"-" => true
		                                      | #"/" => true
		                                      | #"." => true
						      | c => Char.isAlphaNum c)),
				       compilerSave)
		  val comilerC = compileDFA compiler

		  val flagSave = Save.new ()
		  val flag = seq [oneOrMore (char #" "),
				  save (seq [char #"-",
					     star (isChar (fn #"-" => true
					                    | c => Char.isAlphaNum c))],
					flagSave)]
		  val flagC = compileDFA flag

		  val optionSave = Save.new ()
		  val option = save (star (isChar (fn c => Char.isAlphaNum c)),
				     optionSave)
		  val optionC = compileDFA option

		  val optionsSave = Save.new ()
		  val options = save (or [option,
					  seq [char #"{",
					       star (char #" "),
					       option,
					       star (seq [star (char #" "),
							  char #",",
							  star (char #" "),
							  option]),
					       star (char #" "),
					       char #"}"]],
				      optionsSave)
		  val optionsC = compileDFA options

		  val flagAndOptionsSave = Save.new ()
		  val flagAndOptions
		    = save (seq [flag,
				 or [null,
				     seq [oneOrMore (char #" "),
					  options]]],
			    flagAndOptionsSave)
		  val flagAndOptionsC = compileDFA flagAndOptions

		  val flagsAndOptionsSave = Save.new ()
		  val flagsAndOptions 
		    = save (star flagAndOptions,
			    flagsAndOptionsSave)
		  val flagsAndOptionsC = compileDFA flagsAndOptions

		  val compilerAndFlagsAndOptions
		    = seq [compiler,
			   flagsAndOptions]
		  val compilerAndFlagsAndOptionsC 
		    = compileDFA compilerAndFlagsAndOptions

		  val (compiler, flags)
		    = case Compiled.matchAll(compilerAndFlagsAndOptionsC,
					     arg)
			of NONE => ("mlton", [])
			 | SOME m
			 => let
			      val {exists, lookup, peek}
				= Match.stringFuns m

			      val compiler = lookup compilerSave
			      val flagsAndOptions = lookup flagsAndOptionsSave

			      fun doit_flags (flags, flagsAndOptions)
				= case Compiled.matchLong(flagAndOptionsC, 
							  flagsAndOptions,
							  0)
				    of NONE => flags
				     | SOME m
				     => let
					  val {exists, lookup, peek}
					    = Match.stringFuns m

					  val flag = lookup flagSave

					  val {start, length} = Match.startLength m
					  val flagsAndOptions
					    = String.extract(flagsAndOptions,
							     start + length,
							     NONE)
					in
					  case peek optionsSave
					    of NONE => doit_flags
					               ([[flag,""]]::flags,
							flagsAndOptions)
					     | SOME options
					     => let
						  val options
						    = String.fields
						      (options,
						       fn #"{" => true
						        | #" " => true
						        | #"," => true
						        | #"}" => true
						        | _ => false)
						  val options
						    = List.removeAll
						      (options, String.isEmpty)
						in
						  doit_flags
						  ((List.map
						    (options,
						     fn option => [flag, option]))
						   ::flags,
						   flagsAndOptions)
						end
					end
			    in
			      (compiler,
			       doit_flags ([], flagsAndOptions))
			    end
		  val (compiler, flags)
		    = (compiler, 
		       List.cross (List.rev flags))
		  val flags = List.map(flags, List.concat)
		  fun map(nil, f) = nil
		    | map(h::t, f) = (f h)::(map(t, f))
		in
		  map
		  (flags,
		   fn flags
		    => {name = concat (compiler:: 
				       " "::
				       (List.separate(flags, " "))),
			abbrv = "MLton" ^ (Int.toString (Counter.next n)),
			test = make (compiler, flags)})
		end
end

fun kitCompile {bench} =
   compileSizeRun {args = [batch bench],
		   compiler = "mlkit",
		   exe = "run",
		   doTextPlusData = true}
   
fun mosmlCompile {bench} =
   compileSizeRun {args = ["-orthodox", "-standalone", "-toplevel", batch bench],
		   compiler = "mosmlc",
		   exe = "a.out",
		   doTextPlusData = false}

fun njCompile {bench} =
   Escape.new
   (fn e =>
    let
       (* sml should start SML/NJ *)
       val sml = "sml"
       val {system, user} =
	  File.withTempOut
	  (fn out =>
	   (Out.output
	    (out, "local\nval _ = SMLofNJ.Internals.GC.messages false\n")
	    ; File.outputContents (concat [bench, ".sml"], out)
	    ; (Out.output
	       (out,
		concat
		["in val _ = SMLofNJ.exportFn (\"", bench,
		 "\", fn _ =>\n (Main.doit () ; OS.Process.success))\nend\n"]
		 ))),
           fn input => withInput (input, fn () => timeIt (sml, [])))
         handle _ => Escape.escape (e, {compile = NONE,
					run = NONE,
					size = NONE})
       val heap = concat [bench, ".x86-linux"]
    in
       if not (File.doesExist heap)
	  then {compile = NONE,
		run = NONE,
		size = NONE}
       else
          let
	     val compile = Time.toReal (Time.+ (user, system))
	     val size = SOME (File.size heap)
	     val run =
		  timeCall (sml, [concat ["@SMLload=", heap]])
		  handle _ => Escape.escape (e, {compile = SOME compile,
						 run = NONE,
						 size = size})
	  in {compile = SOME compile,
	      run = SOME run,
	      size = size}
	  end
    end)
		
fun polyCompile {bench} =
   Escape.new
   (fn e =>
    let
       val originalDbase = "/usr/lib/poly/ML_dbase"
       val poly = "poly"
    in File.withTemp
       (fn dbase =>
	let
	   val _ = File.copy (originalDbase, dbase)
	   val original = File.size dbase

	   val {system, user} =
	      File.withTempOut
	      (fn out =>
	       Out.output
	       (out,
		concat ["use \"", bench, ".sml\" handle _ => PolyML.quit ();\n",
			"if PolyML.commit() then () else (Main.doit(); ());\n",
			"PolyML.quit();\n"]),
	       fn input => withInput (input, fn () => timeIt ("poly", [dbase])))
	   val after = File.size dbase
	in
	   if original = after
	      then {compile = NONE,
		    run = NONE,
		    size = NONE}
	   else
	       let
		  val compile = SOME (Time.toReal (Time.+ (user, system)))
		  val size = SOME (after - original)
		  val run =
		     timeCall (poly, [dbase])
		     handle _ => Escape.escape (e, {compile = compile,
						    run = NONE,
						    size = size})
	       in
		  {compile = compile,
		   run = SOME run,
		   size = size}
	       end
	end)
    end)

fun usage msg =
   Process.usage {usage = "[-mlkit] [-mosml] [-smlnj] bench1 bench2 ...",
		  msg = msg}

type 'a data = {bench: string,
		compiler: string,
		value: 'a} list


fun main args =
   let
      val compilers: {name: string,
		      abbrv: string,
		      test: {bench: File.t} -> {compile: real option,
						run: real option,
						size: int option}} list ref 
	= ref []
      fun pushCompiler compiler = List.push(compilers, compiler)
      fun pushCompilers compilers' = compilers := (List.rev compilers') @ (!compilers)

      fun setData (switch, data, str) =
	 let
	    fun die () = usage (concat ["invalid -", switch, " argument: ", str])
	    open Regexp
	    val numSave = Save.new ()
	    val regexpSave = Save.new ()
	    val re = seq [save (star digit, numSave),
			  char #",",
			  save (star any, regexpSave)]
	    val reC = compileDFA re
	 in
	    case Compiled.matchAll (reC, str) of
	       NONE => die ()
	     | SOME match => 
		  let
		     val num = Match.lookupString (match, numSave)
		     val num = case Int.fromString num of
		                  NONE => die ()
				| SOME num => num
		     val regexp = Match.lookupString (match, regexpSave)
		     val (regexp, saves) = 
		        case Regexp.fromString regexp of
			   NONE => die ()
			 | SOME regexp => regexp
		     val save = if 0 <= num andalso num < Vector.length saves
				   then Vector.sub (saves, num)
				else die ()
		     val regexpC = compileDFA regexp
		     fun doit s =
		         Option.map
			 (Compiled.matchAll (regexpC, s),
			  fn match => Match.lookupString (match, save))
		  in
		    data := SOME (str, doit)
		  end
	 end
      val outData : (string * (string -> string option)) option ref = ref NONE
      val setOutData = fn str => setData ("out", outData, str)
      val errData : (string * (string -> string option)) option ref = ref NONE
      val setErrData = fn str => setData ("err", errData, str)
      (* Set the stack limit to its max, since mlkit segfaults on some benchmarks
       * otherwise.
       *)
       val _ =
	  case MLton.hostType of
	     MLton.Cygwin => ()
	   | MLton.Linux => 
		let
		   open MLton.Rlimit
		   val {hard, ...} = get stackSize
		in
		   set (stackSize, {hard = hard, soft = hard})
		end
      local
	 open Popt
      in
	 val res =
	    parse
	    {switches = args,
	     opts = [("args",
		      SpaceString
		      (fn args =>
		       runArgs := String.tokens (args, Char.isSpace))),
		     ("err", SpaceString setErrData),
		     ("html", trueRef doHtml),
		     ("mlkit", 
		      None (fn () => pushCompiler
			    {name = "ML-Kit",
			     abbrv = "ML-Kit",
			     test = kitCompile})),
		     ("mosml",
		      None (fn () => pushCompiler
			    {name = "Moscow-ML",
			     abbrv = "Moscow-ML",
			     test = mosmlCompile})),
		     ("mlton",
		      SpaceString (fn arg => pushCompilers
				   (makeMLton arg))),
		     ("once", trueRef doOnce),
		     ("out", SpaceString setOutData),
		     ("poly",
		      None (fn () => pushCompiler
			    {name = "Poly/ML",
			     abbrv = "Poly/ML",
			     test = polyCompile})),
		     ("smlnj",
		      None (fn () => pushCompiler
			    {name = "SML/NJ",
			     abbrv = "SML/NJ",
			     test = njCompile})),
		     trace]}
      end
   in
      case res of
	 Result.No s => usage (concat ["invalid switch: ", s])
       | Result.Yes benchmarks =>
	    let
	       val compilers = List.rev (!compilers)
	       val base = #name (hd compilers)
	       val _ =
		  let open Signal
		  in ignore pipe
		  end
	       fun r2s r = Real.format (r, Real.Format.fix (SOME 1))
	       val i2s = Int.toCommaString
	       val s2s = fn s => s
	       fun show {compiles, runs, sizes, errs, outs} =
		  let
		     val out = Out.standard
		     val _ = List.foreach
			(compilers,
			 fn {name, abbrv, ...}
			 => Out.output (out, concat [abbrv, " -- ", name, "\n"]))
		     fun show (title, data: 'a data, toString) =
			let
			   val _ = Out.output (out, concat [title, "\n"])
			   val compilers =
			      List.fold
			      (compilers, [],
			       fn ({name = n, abbrv = n', ...}, ac) =>
			       if List.exists (data, fn {compiler = c, ...} =>
					       n = c)
				  then (n, n') :: ac
			       else ac)
			   val benchmarks =
			      List.fold (benchmarks, [], fn (b, ac) =>
					 if List.exists (data, fn {bench, ...} =>
							 bench = b)
					    then b :: ac
					 else ac)
			   val rows =
			      ("benchmark"
			       :: List.revMap (compilers, fn (_, n') => n'))
			      :: (List.revMap
				  (benchmarks, fn b =>
				   b :: (List.revMap
					 (compilers, fn (n, _) =>
					  case (List.peek
						(data, fn {bench = b',
							   compiler = c', ...} =>
						 b = b' andalso n = c')) of
					     NONE => "*"
					   | SOME {value = v, ...} => toString v))))
			   open Justify
			   val _ =
			      outputTable
			      (table {justs = (Left ::
					       List.revMap (compilers,
							    fn _ => Right)),
				      rows = rows},
			       out)
			   fun p ss =
			      (Out.output (out, concat ss)
			       ; Out.newline out)
			   fun prow (b :: ns, c) =
			      (p ["<tr>"]
			       ; p ["<t", c, " align = left>", b, "</t", c, ">"]
			       ; (List.foreach
				  (ns, fn n =>
				   p ["<t", c, " align = right>", n,
				      "</t", c, ">"]))
			       ; p ["</tr>"])
			   val _ =
			      if !doHtml
				 then
				    (prow (hd rows, "h")
				     ; (List.foreach
					(tl rows, fn b :: r =>
					 let
					    val b = 
					       concat
					       ["<a href = \"benchmarks/", b,
						".sml\">", b, "</a>"]
					 in
					    prow (b :: r, "d")
					 end)))
			      else ()
			in
			   ()
			end
		     val _ = show ("compile time", compiles, r2s)
		     val _ = show ("run time", runs, r2s)
		     val bases = List.keepAll (runs, fn {compiler, ...} =>
					       compiler = base)
		     val runs =
			List.fold
			(runs, [], fn ({bench, compiler, value}, ac) =>
			 if compiler = base
			    then ac
			 else
			    {bench = bench,
			     compiler = compiler,
			     value =
			     case List.peek (bases, fn {bench = b, ...} =>
					     bench = b) of
				NONE => ~1.0
			      | SOME {value = v, ...} => value / v} :: ac)
		     val _ = show ("run time ratio", runs, r2s)
		     val _ = show ("size", sizes, i2s)
		     val _ = case !outData of
			SOME (out, _) => show (concat ["out: ", out], outs, s2s)
		      | NONE => ()
		     val _ = case !errData of
			SOME (err, _) => show (concat ["err: ", err], errs, s2s)
		      | NONE => ()
		  in ()
		  end
	       val totalFailures = ref []
	       val data = 
		  List.fold
		  (benchmarks, {compiles = [], runs = [], sizes = [],
				outs = [], errs = []},
		   fn (bench, ac) =>
		   let
		      val _ =
			 File.withOut
			 (batch bench, fn out =>
			  (File.outputContents (concat [bench, ".sml"], out)
			   ; Out.output (out, "val _ = Main.doit ()\n")))
		      val foundOne = ref false
		      val res =
			 List.fold
			 (compilers, ac, fn ({name, abbrv, test},
					     ac as {compiles: real data,
						    runs: real data,
						    sizes: int data,
						    outs: string data,
						    errs: string data}) =>
			  if true
			     then
				let
				   val (outTmpFile, outTmpOut) =
				      File.temp
				      {prefix = "tmp", suffix = "out"}
				   val (errTmpFile, errTmpOut) =
				      File.temp
				      {prefix = "tmp", suffix = "err"}
				   val {compile, run, size} =
				      ignoreOutput
				      (fn () => test {bench = bench})
				   val out = 
				      case !outData of 
					 NONE => NONE
				       | SOME (_, doit) => 
					    File.foldLines
					    (outTmpFile, NONE, fn (s, v) =>
					     let val s = String.removeTrailing
						(s, fn c => 
						 Char.equals (c, Char.newline))
					     in
						case doit s of
						   NONE => v
						 | v => v
					     end)
				   val err = 
				      case !errData of 
					 NONE => NONE
				       | SOME (_, doit) =>
					    File.foldLines
					    (errTmpFile, NONE, fn (s, v) =>
					     let val s = String.removeTrailing
						(s, fn c => 
						 Char.equals (c, Char.newline))
					     in
						case doit s of
						   NONE => v
						 | v => v
					     end)
				   val _ = File.remove outTmpFile
				   val _ = File.remove errTmpFile
				   fun add (v, ac) =
				      case v of
					 NONE => ac
				       | SOME v =>
					    (foundOne := true
					     ; {bench = bench,
						compiler = name,
						value = v} :: ac)
				   val ac =
				      {compiles = add (compile, compiles),
				       runs = add (run, runs),
				       sizes = add (size, sizes),
				       outs = add (out, outs),
				       errs = add (err, errs)}
				in show ac
				   ; Out.flush Out.standard
				   ; ac
				end
			  else ac)
		      val _ =
			 if !foundOne
			    then ()
			 else List.push (totalFailures, bench)
		   in
		      res
		   end)
	       val totalFailures = !totalFailures
	       val _ =
		  if List.isEmpty totalFailures
		     then ()
		  else (print ("The following benchmarks failed completely.\n")
			; List.foreach (totalFailures, fn s =>
					print (concat [s, "\n"])))
	    in ()
	    end
   end

val main = Process.makeMain main

end
