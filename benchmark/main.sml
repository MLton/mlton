structure Main =
struct

type int = Int.t

val fail = Process.fail

val doKit = ref false
val doMLton = ref true
val doMLtonTop = ref false
val doMosml = ref false
val doNJ = ref false
val doPoly = ref false
   
local
   val trialTime = Time.seconds (IntInf.fromInt 60)
in
   fun timeCall (com, args): real =
      let 
	 fun loop (n, ac: Time.t): real =
	    if Time.> (ac, trialTime)
	       then Time.toReal ac / Real.fromInt n
	    else loop (n + 1,
		       let
			  val {user, system} =
			     Process.time (fn () => Process.call' (com, args))
			  val op + = Time.+
		       in ac + user + system
		       end)
      in (loop (0, Time.zero))
      end
end

fun compileSizeRun {args, compiler, exe, doTextPlusData: bool} =
   Escape.new
   (fn e =>
    let
       val {system, user} =
	  Process.time (fn () => Process.call' (compiler, args))
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
	  timeCall (exe, [])
	  handle _ => Escape.escape (e, {compile = compile,
					 run = NONE,
					 size = size})
    in {compile = compile,
	run = SOME run,
	size = size}
    end)

fun batch bench = concat [bench, ".batch.sml"]
   
local
   fun make compiler {bench} =
      let val exe = "a.out"
      in compileSizeRun {args = ["-o", exe, batch bench],
			 compiler = compiler,
			 exe = exe,
			 doTextPlusData = true}
      end
in
   val mltonCompile  = make "mlton"
   val mltonTopCompile = make "/usr/local/bin/mlton"
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
	  Process.time
	  (fn () =>
	   Process.callWithOut
	   (sml, [], fn out =>
	    (Out.output
	     (out, "local\nval _ = SMLofNJ.Internals.GC.messages false\n")
	     ; File.outputContents (concat [bench, ".sml"], out)
	     ; (Out.output
		(out,
		 concat
		 ["in val _ = SMLofNJ.exportFn (\"", bench,
		  "\", fn _ =>\n (Main.doit () ; OS.Process.success))\nend\n"]
		  )))))
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
		
(* fun polyCompile {bench} =
 *    Escape.new
 *    (fn e =>
 *     let
 *        val op / = String./
 *        val root = "/usr/lib/polyml"
 *        val originalDbase = root/"ML_dbase"
 *     in File.withTemp
 *        (fn dbase =>
 * 	File.withTemp
 * 	(fn times =>
 * 	 (File.copy (originalDbase, dbase)
 * 	  ; (Process.callWithOut
 * 	     (root/"poly", [dbase], fn out =>
 * 	      (Out.output
 * 	       (out, concat ["\
 * 		\val out = TextIO.openOut \"", times, "\";\n\
 * 		\fun disp r =\n\
 * 		\    TextIO.output (out, concat [Real.toString r, \"\\n\"]);\n\
 * 		\fun time () =\n\
 * 		\   let val {utime, stime, ...} = Posix.ProcEnv.times ()\n\
 * 		\   in Time.+ (utime, stime)\n\
 * 		\   end;\n\
 * 		\val t = time ();\n"])
 * 			     ; File.outputContents (concat [bench, ".sml"], out)
 * 			     ; Out.output (out, "\
 * 			      \val _ = disp (Time.toReal (Time.- (time (), t)));\n\
 * 			      \val _ = PolyML.commit ();\n\
 * 			      \val max = Time.fromSeconds 5\n\
 * 			      \fun loop (n, ac: Time.time): unit =\n\
 * 			      \   if Time.> (ac, max)\n\
 * 			      \   then disp (Time.toReal ac / Real.fromInt n)\n\
 * 			      \   else loop (n + 1, let val t = time () val _ = Main.doit ()\n\
 * 			      \                     in Time.+ (ac, Time.- (time (), t))\n\
 * 			      \                     end)\n\
 * 			      \val _ = loop (0, Time.zeroTime);\n\
 * 			      \val _ = TextIO.closeOut out;\n\
 * 			      \val _ = PolyML.quit ();\n"))))
 * 					   handle _ => Escape.escape
 * 					      (e, {compile = NONE,
 * 						   run = NONE,
 * 						   size = NONE})
 * 					   ; let
 * 						val lines = File.lines times
 * 						fun size () =
 * 						   let
 * 						      val _ = Process.call' (root/"discgarb", [dbase])
 * 						   in File.size dbase - File.size originalDbase
 * 						   end
 * 						val s2r = valOf o Real.fromString
 * 					     in case lines of
 * 						[compile] => {compile = SOME (s2r compile),
 * 							      run = NONE,
 * 							      size = SOME (size ())}
 * 					      | [compile, run] => {compile = SOME (s2r compile),
 * 								   run = SOME (s2r run),
 * 								   size = SOME (size ())}
 * 					      | _ => {compile = NONE, run = NONE, size = NONE}
 * 					     end)))
 *     end)
 *)

val compilers: {doit: bool ref,
		name: string,
		test: {bench: File.t} -> {compile: real option,
					  run: real option,
					  size: int option}} list =
   [{doit = doKit,
     name = "ML Kit",
     test = kitCompile},
    {doit = doMLton,
     name = "MLton",
     test = mltonCompile},
    {doit = doMLtonTop,
     name = "old MLton",
     test = mltonTopCompile},
    {doit = doMosml,
     name = "Moscow ML",
     test = mosmlCompile},
    {doit = doNJ,
     name = "SML/NJ",
     test = njCompile}]

fun usage msg =
   Process.usage {usage = "[-mlkit] [-mosml] [-smlnj] bench1 bench2 ...",
		  msg = msg}

type 'a data = {bench: string,
		compiler: string,
		value: 'a} list

fun main args =
   let
      (* Set the stack limit to its max, since mlkit segfaults on some benchmarks
       * otherwise.
       *)
      val _ =
	 let open MLton.Rlimit
	    val {hard, ...} = get stackSize
	 in
	    set (stackSize, {hard = hard, soft = hard})
	 end
      local
	 open Popt
      in
	 val res =
	    parse {switches = args,
		   opts = [("mlkit", trueRef doKit),
			   ("mltonTop", trueRef doMLtonTop),
			   ("mosml", trueRef doMosml),
			   ("smlnj", trueRef doNJ),
			   trace]}
      end
   in case res of
      Result.No s => usage (concat ["invalid switch: ", s])
    | Result.Yes benchmarks =>
	 let
	    val _ =
	       let open Signal
	       in Handler.set (pipe, Handler.Ignore)
	       end
	    fun r2s r = Real.format (r, Real.Format.fix (SOME 1))
	    val i2s = Int.toCommaString
	    fun show {compiles, runs, sizes} =
	       let
		  val out = Out.standard
		  fun show (title, data: 'a data, toString) =
		     let
			val _ = Out.output (out, concat [title, "\n"])
			val compilers =
			   List.fold
			   (compilers, [], fn ({name = n, ...}, ac) =>
			    if List.exists (data, fn {compiler = c, ...} =>
					    n = c)
			       then n :: ac
			    else ac)
			val benchmarks =
			   List.fold (benchmarks, [], fn (b, ac) =>
				      if List.exists (data, fn {bench, ...} =>
						      bench = b)
					 then b :: ac
				      else ac)
			val rows =
			   ("benchmark" :: rev compilers)
			   :: (List.revMap
			       (benchmarks, fn b =>
				b ::
				List.revMap
				(compilers, fn c =>
				 case (List.peek
				       (data, fn {bench = b',
						  compiler = c', ...} =>
					b = b' andalso c = c')) of
				    NONE => "*"
				  | SOME {value = v, ...} => toString v)))
			open Justify
			val _ =
			   outputTable
			   (table {justs = (Left :: List.revMap (compilers,
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
			   (prow (hd rows, "h")
			    ; List.foreach (tl rows, fn r => prow (r, "d")))
 		     in
			()
		     end
		  val _ = show ("compile time", compiles, r2s)
		  val _ = show ("run time", runs, r2s)
		  val mltons = List.keepAll (runs, fn {compiler, ...} =>
					     compiler = "MLton")
		  val runs =
		     List.fold
		     (runs, [], fn ({bench, compiler, value}, ac) =>
		      if compiler = "MLton"
			 then ac
		      else
			 {bench = bench,
			  compiler = compiler,
			  value =
			  case List.peek (mltons, fn {bench = b, ...} =>
					  bench = b) of
			     NONE => ~1.0
			   | SOME {value = v, ...} => value / v} :: ac)
		  val _ = show ("run time ratio", runs, r2s)
		  val _ = show ("size", sizes, i2s)
	       in ()
	       end
	    val totalFailures = ref []
	    val data = 
	       List.fold
	       (benchmarks, {compiles = [], runs = [], sizes = []},
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
		      (compilers, ac, fn ({doit, name, test},
					  ac as {compiles: real data,
						 runs: real data,
						 sizes: int data}) =>
		       if !doit
			  then
			     let
				val {compile, run, size} =
				   Out.ignore
				   (Out.standard, fn () =>
				    Out.ignore
				    (Out.error, fn () =>
				     test {bench = bench}))
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
				    sizes = add (size, sizes)}
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
