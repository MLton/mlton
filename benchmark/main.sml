(* Copyright (C) 2013,2014 Matthew Fluet.
 * Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Main =
struct

type int = Int.t

fun usage msg =
   Process.usage {usage = "[-mlkit] [-mlton </path/to/mlton>] [-mosml] [-poly] [-smlnj] bench1 bench2 ...",
                  msg = msg}

val doOnce = ref false
val doWiki = ref false
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
      Exn.finally
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
      Exn.finally
      (fn () => fluidLet (stderr, nullFd, fn () =>
                          fluidLet (stdout, nullFd, f)),
       fn () => close nullFd)
   end

datatype command =
   Explicit of {args: string list,
                com: string}
  | Shell of string list

fun timeIt ca =
   Process.time
   (fn () =>
    case ca of
       Explicit {args, com} =>
          Process.wait (Process.spawnp {file = com, args = com :: args})
     | Shell ss => List.foreach (ss, Process.system))
   
local
   val trialTime = Time.seconds (IntInf.fromInt 60)
in
   fun timeCall (com, args): real =
      let 
         fun doit ac =
            let
               val {user, system} = timeIt (Explicit {args = args, com = com})
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

val benchCounts: (string * int) list =
   ("barnes-hut", 24576):: (* 31.32 sec *)
   ("boyer", 12288):: (* 40.77 sec *)
   ("checksum", 8192):: (* 31.33 sec *)
   ("count-graphs", 16):: (* 35.35 sec *)
   ("DLXSimulator", 256):: (* 30.68 sec *)
   ("even-odd", 24):: (* 37.76 sec *)
   ("fft", 12):: (* 35.31 sec *)
   ("fib", 12):: (* 30.48 sec *)
   ("flat-array", 32768):: (* 35.03 sec *)
   ("hamlet", 384):: (* 44.55 sec *)
   ("imp-for", 3072):: (* 30.25 sec *)
   ("knuth-bendix", 3072):: (* 37.84 sec *)
   ("lexgen", 2048):: (* 34.41 sec *)
   ("life", 32):: (* 37.98 sec *)
   ("logic", 256):: (* 39.00 sec *)
   ("mandelbrot", 6):: (* 38.49 sec *)
   ("matrix-multiply", 128):: (* 30.01 sec *)
   ("md5", 96):: (* 41.23 sec *)
   ("merge", 16384):: (* 39.13 sec *)
   ("mlyacc", 3072):: (* 37.48 sec *)
   ("model-elimination", 4):: (* 39.14 sec *)
   ("mpuz", 96):: (* 30.01 sec *)
   ("nucleic", 4096):: (* 31.90 sec *)
   ("output1", 12):: (* 33.01 sec *)
   ("peek", 2048):: (* 35.93 sec *)
   ("pidigits", 4096):: (* 36.27 sec *)
   ("psdes-random", 24):: (* 40.57 sec *)
   ("ratio-regions", 1536):: (* 45.14 sec *)
   ("ray", 1536):: (* 42.18 sec *)
   ("raytrace", 96):: (* 37.27 sec *)
   ("simple", 768):: (* 36.10 sec *)
   ("smith-normal-form", 192):: (* 41.93 sec *)
   ("tailfib", 512):: (* 35.36 sec *)
   ("tak", 24):: (* 44.71 sec *)
   ("tensor", 6):: (* 34.63 sec *)
   ("tsp", 16):: (* 36.91 sec *)
   ("tyan", 384):: (* 32.56 sec *)
   ("vector-concat", 32):: (* 35.13 sec *)
   ("vector-rev", 64):: (* 32.16 sec *)
   ("vliw", 768):: (* 30.51 sec *)
   ("wc-input1", 24576):: (* 41.69 sec *)
   ("wc-scanStream", 24576):: (* 30.55 sec *)
   ("zebra", 64):: (* 33.44 sec *)
   ("zern", 12288):: (* 33.59 sec *)
   nil

val benchCount =
   String.memoize
   (fn s =>
    case List.peek (benchCounts, fn (b, _) => b = s) of
       NONE => Error.bug (concat ["no benchCount for ", s])
     | SOME (_, c) => Int.toString c)

fun compileSizeRun {command, exe, doTextPlusData: bool} =
   Escape.new
   (fn e =>
    let
       val exe = "./" ^ exe
       val {system, user} = timeIt command
          handle _ => Escape.escape (e, {compile = NONE,
                                         run = NONE,
                                         size = NONE})
       val compile = SOME (Time.toReal (Time.+ (system, user)))
       val size =
          if doTextPlusData
             then
                let 
                   val {text, data, ...} = Process.size exe
                in SOME (Position.fromInt (text + data))
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

fun batch {abbrv, bench} =
   let
      val abbrv =
         String.translate
         (abbrv, fn c =>
          if Char.isAlphaNum c
             then String.fromChar c
          else "_")
   in
      concat [bench, ".", abbrv, ".batch.sml"]
   end

local
   val n = Counter.new 0
in
   fun makeMLton commandPattern =
      case ChoicePattern.expand commandPattern of
         Result.No m => usage m
       | Result.Yes cmds =>
            List.map
            (cmds, fn cmd =>
             let
                val abbrv = "MLton" ^ (Int.toString (Counter.next n))
             in
                {name = cmd,
                 abbrv = abbrv,
                 test = (fn {bench} =>
                         let
                            val src = batch {abbrv = abbrv, bench = bench}
                            val exe = String.dropSuffix (src, 4)
                            val cmds = (concat [cmd, " -output ", exe, " ", src])::
                                       (*(concat ["strip ", exe])::*)
                                       nil
                         in
                            compileSizeRun
                            {command = Shell cmds,
                             exe = exe,
                             doTextPlusData = true}
                         end)}
             end)
end

fun kitCompile {bench} =
   compileSizeRun {command = Explicit {args = [batch {abbrv = "MLKit", bench = bench}],
                                       com = "mlkit"},
                   exe = "run",
                   doTextPlusData = true}
   
fun mosmlCompile {bench} =
   compileSizeRun
   {command = Explicit {args = ["-orthodox", "-standalone", "-toplevel",
                                batch {abbrv = "Moscow ML", bench = bench}],
                        com = "mosmlc"},
    exe = "a.out",
    doTextPlusData = false}

val njSuffix =
   Promise.delay
   (fn () =>
    let
       val sml = "sml"
       val suffix =
           File.withTemp
           (fn tmp =>
            (File.withTempOut
             (fn output =>
              Out.output
              (output, concat ["val tmp = TextIO.openOut(\"", tmp, "\");\n",
                               "val _ = TextIO.output(tmp, SMLofNJ.SysInfo.getHeapSuffix());\n",
                               "val _ = TextIO.closeOut(tmp);\n"]),
              fn input =>
              withInput
              (input, fn () =>
               Process.wait (Process.spawnp {file = sml, args = [sml]})))
             ; In.withClose (In.openIn tmp, In.inputAll)))
    in
       suffix
    end)

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
                 "\", fn _ => (Main.doit ", benchCount bench,
                 "; OS.Process.success))\nend\n"]
                 ))),
           fn input => withInput (input, fn () => timeIt (Explicit {args = [],
                                                                    com = sml})))
         handle _ => Escape.escape (e, {compile = NONE,
                                        run = NONE,
                                        size = NONE})
       val suffix = Promise.force njSuffix
       val heap = concat [bench, ".", suffix]
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
       val poly = "/usr/bin/poly"
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
                        "if PolyML.commit() then () else ",
                        "(Main.doit ", benchCount bench, "; ());\n",
                        "PolyML.quit();\n"]),
               fn input =>
               withInput
               (input, fn () =>
                timeIt (Explicit {args = [dbase],
                                  com = poly})))
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

type 'a data = {bench: string,
                compiler: string,
                value: 'a} list

fun main args =
   let
      val compilers: {name: string,
                      abbrv: string,
                      test: {bench: File.t} -> {compile: real option,
                                                run: real option,
                                                size: Position.int option}} list ref 
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
          let
             open MLton.Platform.OS
          in
             if host = Linux
                then
                   let
                      open MLton.Rlimit
                      val {hard, ...} = get stackSize
                   in
                      set (stackSize, {hard = hard, soft = hard})
                   end
             else ()
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
                     ("mlkit", 
                      None (fn () => pushCompiler
                            {name = "MLKit",
                             abbrv = "MLKit",
                             test = kitCompile})),
                     ("mosml",
                      None (fn () => pushCompiler
                            {name = "Moscow ML",
                             abbrv = "Moscow ML",
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
                     trace,
                     ("wiki", trueRef doWiki)]}
      end
   in
      case res of
         Result.No msg => usage msg
       | Result.Yes benchmarks =>
            let
               val compilers = List.rev (!compilers)
               val base = #abbrv (hd compilers)
               val _ =
                  let
                     open MLton.Signal
                  in
                     setHandler (Pervasive.Posix.Signal.pipe, Handler.ignore)
                  end
               fun r2s n r = Real.format (r, Real.Format.fix (SOME n))
               val i2s = Int.toCommaString
               val p2s = i2s o Position.toInt
               val s2s = fn s => s
               val failures = ref []
               fun show ({compiles, runs, sizes, errs, outs}, {showAll}) =
                  let
                     val out = Out.standard
                     val _ =
                        List.foreach
                        (compilers, fn {name, abbrv, ...} =>
                         Out.output (out, concat [abbrv, " -- ", name, "\n"]))
                     val _ =
                        case !failures of
                           [] => ()
                         | fs =>
                            Out.output
                            (out,
                             concat ["WARNING: ", base, " failed on: ",
                                     concat (List.separate (fs, ", ")),
                                     "\n"])
                     fun show (title, data: 'a data, toString, toStringHtml) =
                        let
                           val _ = Out.output (out, concat [title, "\n"])
                           val compilers =
                              List.fold
                              (compilers, [], fn ({name = n, abbrv = a, ...}, ac) =>
                               if showAll
                                  orelse List.exists (data, fn {compiler = c', ...} =>
                                                      a = c')
                                  then (n, a) :: ac
                               else ac)
                           val benchmarks =
                              List.fold
                              (benchmarks, [], fn (b, ac) =>
                               if showAll
                                  orelse List.exists (data, fn {bench = b', ...} =>
                                                      b = b')
                                  then b :: ac
                               else ac)
                           fun rows toString =
                              ("benchmark"
                               :: List.revMap (compilers, fn (_, a) => a))
                              :: (List.revMap
                                  (benchmarks, fn b =>
                                   b :: (List.revMap
                                         (compilers, fn (_, a) =>
                                          case (List.peek
                                                (data, fn {bench = b',
                                                           compiler = c', ...} =>
                                                 b = b' andalso a = c')) of
                                             NONE => "*"
                                           | SOME {value = v, ...} =>
                                                toString v))))
                           open Justify
                           val () =
                              outputTable
                              (table {columnHeads = NONE,
                                      justs = (Left ::
                                               List.revMap (compilers,
                                                            fn _ => Right)),
                                      rows = rows toString},
                               out)
                           fun prow ns =
                              let
                                 fun p s = Out.output (out, s)
                              in
                                 case ns of
                                    [] => raise Fail "bug"
                                  | b :: ns =>
                                       (p "||"
                                        ; p b
                                        ; List.foreach (ns, fn n =>
                                                        (p "||"; p n))
                                        ; p "||\n")
                              end
                           val _ =
                              if not (!doWiki)
                                 then ()
                              else
                                 let
                                    val rows = rows toStringHtml
                                 in                                       
                                    prow (hd rows)
                                    ; (List.foreach
                                       (tl rows,
                                        fn [] => raise Fail "bug"
                                         | b :: r =>
                                              let
                                                 val b = 
                                                    concat
                                                    ["[attachment:",
                                                     b, ".sml ", b, "]"]
                                              in
                                                 prow (b :: r)
                                              end))
                                 end
                        in
                           ()
                        end
                     val bases = List.keepAll (runs, fn {compiler, ...} =>
                                               compiler = base)
                     val ratios =
                        List.fold
                        (runs, [], fn ({bench, compiler, value}, ac) =>
                         if compiler = base andalso not showAll
                            then ac
                         else
                            {bench = bench,
                             compiler = compiler,
                             value =
                             case List.peek (bases, fn {bench = b, ...} =>
                                             bench = b) of
                                NONE => ~1.0
                              | SOME {value = v, ...} => value / v} :: ac)
                     val _ = show ("run time ratio", ratios, r2s 2, r2s 1)
                     val _ = show ("size", sizes, p2s, p2s)
                     val _ = show ("compile time", compiles, r2s 2, r2s 2)
                     val _ = show ("run time", runs, r2s 2, r2s 2)
                     val _ = case !outData of
                        NONE => ()
                      | SOME (out, _) =>
                           show (concat ["out: ", out], outs, s2s, s2s)
                     val _ = case !errData of
                        NONE => ()
                      | SOME (err, _) =>
                           show (concat ["err: ", err], errs, s2s, s2s)
                  in ()
                  end
               val totalFailures = ref []
               val data = 
                  List.fold
                  (benchmarks, {compiles = [], runs = [], sizes = [],
                                outs = [], errs = []},
                   fn (bench, ac) =>
                   let
                      val foundOne = ref false
                      val res =
                         List.fold
                         (compilers, ac, fn ({name, abbrv, test},
                                             ac as {compiles: real data,
                                                    runs: real data,
                                                    sizes: Position.int data,
                                                    outs: string data,
                                                    errs: string data}) =>
                          if true
                             then
                                let
                                   val _ =
                                      File.withOut
                                      (batch {abbrv = abbrv, bench = bench}, fn out =>
                                       (File.outputContents (concat [bench, ".sml"], out)
                                        ; Out.output (out, concat ["val _ = Main.doit ",
                                                                   benchCount bench,
                                                                   "\n"])))
(*
                                   val outTmpFile =
                                      File.tempName {prefix = "tmp", suffix = "out"}
                                   val errTmpFile =
                                      File.tempName {prefix = "tmp", suffix = "err"}
*)
                                   val {compile, run, size} =
                                     ignoreOutput
                                     (fn () => test {bench = bench})
                                   val _ =
                                      if name = base
                                         andalso Option.isNone run
                                         then List.push (failures, bench)
                                      else ()
(*
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
*)
                                   val out = NONE
                                   val err = NONE
                                   fun add (v, ac) =
                                      case v of
                                         NONE => ac
                                       | SOME v =>
                                            (foundOne := true
                                             ; {bench = bench,
                                                compiler = abbrv,
                                                value = v} :: ac)
                                   val ac =
                                      {compiles = add (compile, compiles),
                                       runs = add (run, runs),
                                       sizes = add (size, sizes),
                                       outs = add (out, outs),
                                       errs = add (err, errs)}
                                   val _ = show (ac, {showAll = false})
                                   val _ = Out.flush Out.standard
                                in
                                   ac
                                end
                          else ac)
                      val _ =
                         if !foundOne
                            then ()
                         else List.push (totalFailures, bench)
                   in
                      res
                   end)
               val _ = show (data, {showAll = true})
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
