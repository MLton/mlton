(* Copyright (C) 2009,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Control: CONTROL =
struct

open ControlFlags

structure CommentStyle =
struct
   datatype t = No | Assembly | C | Dot | LLVM | ML

   val preSuf =
      fn No => ("", "")
       | Assembly => ("/* ", " */")
       | C => ("/* ", " */")
       | Dot => ("// ", "")
       | LLVM => ("; ", "")
       | ML => ("(* ", " *)")

end
datatype style = datatype CommentStyle.t

fun outputHeader (style: style, output: Layout.t -> unit) =
   let
      val (pre, suf) = CommentStyle.preSuf style
   in
      output (Layout.str (concat [pre, Version.banner, suf]));
      if Verbosity.< (!verbosity, Verbosity.Detail)
         then ()
         else output (ControlFlags.layout' {pre = pre, suf = suf})
   end

fun outputHeader' (style, out: Out.t) =
   outputHeader (style, fn l => Layout.outputl (l, out))

val depth: int ref = ref 0
fun getDepth () = !depth
fun indent () = depth := !depth + 3
fun unindent () = depth := !depth - 3

fun message (verb: Verbosity.t, th: unit -> Layout.t): unit =
   if Verbosity.<= (verb, !verbosity)
      then let
              val out = Out.error
              val lay = th ()
           in
              if Layout.isEmpty lay
                 then ()
              else (Layout.output (Layout.indent (lay, !depth), out)
                    ; Out.newline out)
           end
   else ()

fun messageStr (verb, s: string): unit =
   message (verb, fn () => Layout.str s)

fun time () =
   let
      open Time
      val {children, self, gc, ...} = times ()
      fun add {utime, stime} = utime + stime
   in
      (add self + add children, add gc)
   end

fun timeToString {total, gc} =
   let
      fun fmt (x, n) = Real.format (x, Real.Format.fix (SOME n))
      val toReal = Real.fromIntInf o Time.toMilliseconds
      val per =
         if Time.equals (total, Time.zero)
            then "0"
         else fmt (100.0 * (toReal gc / toReal total), 0)
      fun t2s t =
         fmt (Real./ (toReal t, 1000.0), 2)
   in concat [t2s (Time.- (total, gc)), " + ", t2s gc, " (", per, "% GC)"]
   end

exception CompileError
exception Stopped
exception Raised

fun trace (verb, name: string) (f: 'a -> 'b) (a: 'a): 'b =
   if Verbosity.<= (verb, !verbosity)
      then let
              val _ = messageStr (verb, concat [name, " starting"])
              val (t, gc) = time ()
              val _ = indent ()
              fun done () =
                 let
                    val _ = unindent ()
                    val (t', gc') = time ()
                 in
                    timeToString {total = Time.- (t', t),
                                  gc = Time.- (gc', gc)}
                 end
           in (f a
               before messageStr (verb, concat [name, " finished in ", done ()]))
              handle exn =>
                 (case exn of
                     CompileError =>
                        (messageStr (verb, concat [name, " reported errors in ", done ()])
                         ; raise exn)
                   | Stopped =>
                        (messageStr (verb, concat [name, " finished in ", done ()])
                         ; raise exn)
                   | Raised =>
                        (messageStr (verb, concat [name, " raised in ", done ()])
                         ; raise exn)
                   | _ =>
                        (messageStr (verb, concat [name, " raised: ", Exn.toString exn])
                         ; (case Exn.history exn of
                               [] => ()
                             | history =>
                                  (indent ()
                                   ; List.foreach (history, fn s => messageStr (verb, s))
                                   ; unindent ()))
                         ; messageStr (verb, concat [name, " raised in ", done ()])
                         ; raise Raised))
           end
      else f a

fun traceTop (name: string) (f: 'a -> unit) (a: 'a) =
   trace (Top, name) f a
   handle CompileError => OS.Process.exit OS.Process.failure
        | Stopped => ()
        | Raised => OS.Process.exit OS.Process.failure
        | exn =>
            (verbosity := Top
             ; messageStr (Top, concat [name, " raised: ", Exn.toString exn])
             ; (case Exn.history exn of
                   [] => ()
                 | history =>
                      (indent ()
                       ; List.foreach (history, fn s => messageStr (Top, s))
                       ; unindent ()))
             ; OS.Process.exit OS.Process.failure)

type traceAccum = {verb: verbosity, 
                   total: Time.t ref, 
                   totalGC: Time.t ref}

val traceAccum: (verbosity * string) -> (traceAccum * (unit -> unit)) =
   fn (verb, name) =>
   let
     val total = ref Time.zero
     val totalGC = ref Time.zero
   in
     ({verb = verb, total = total, totalGC = totalGC},
      fn () => messageStr (verb,
                           concat [name, 
                                   " totals ",
                                   timeToString
                                   {total = !total,
                                    gc = !totalGC}]))
   end

val ('a, 'b) traceAdd: (traceAccum * string) -> ('a -> 'b) -> 'a -> 'b =
   fn ({verb, total, totalGC}, name) =>
   fn f => fn a =>
   if Verbosity.<= (verb, !verbosity)
      then let
              val (t, gc) = time ()
              fun done () =
                 let
                    val (t', gc') = time ()
                 in
                    total := Time.+ (!total, Time.- (t', t))
                    ; totalGC := Time.+ (!totalGC, Time.- (gc', gc))
                 end
           in (f a
               before done ())
              handle exn =>
                 (case exn of
                     CompileError => raise exn
                   | Stopped => raise exn
                   | Raised => raise exn
                   | _ =>
                        (messageStr (verb, concat [name, " raised: ", Exn.toString exn])
                         ; (case Exn.history exn of
                               [] => ()
                             | history =>
                                  (indent ()
                                   ; List.foreach (history, fn s => messageStr (verb, s))
                                   ; unindent ()))
                         ; raise Raised))
           end
      else f a

val ('a, 'b) traceBatch: (verbosity * string) -> ('a -> 'b) ->
                         (('a -> 'b) * (unit -> unit)) =
   fn (verb, name) =>
   let
     val (ta,taMsg) = traceAccum (verb, name)
   in
     fn f =>
     (traceAdd (ta,name) f, taMsg)
   end

(*------------------------------------*)
(*               Errors               *)
(*------------------------------------*)

val numErrors: int ref = ref 0

val errorThreshhold: int ref = ref 20

local
   fun msg (kind: string, r: Region.t, msg: Layout.t, extra: Layout.t): unit =
      let
         open Layout
         val r = Region.toString r
         val msg = Layout.toString msg
         val msg =
            Layout.str
            (concat [String.fromChar (Char.toUpper (String.sub (msg, 0))),
                     String.dropPrefix (msg, 1),
                     "."])
         in
            outputl (align [seq [str (concat [kind, ": "]), str r, str "."],
                            indent (align [msg,
                                           indent (extra, 2)],
                                    2)],
                     Out.error)
      end
in
   fun warning (r, m, e) = msg ("Warning", r, m, e)
   fun error (r, m, e) =
      let
         val _ = Int.inc numErrors
         val _ = msg ("Error", r, m, e)
      in
         if !numErrors = !errorThreshhold
            then raise CompileError
         else ()
      end
end

fun errorStr (r, msg) = error (r, Layout.str msg, Layout.empty)

fun checkForErrors () =
   if !numErrors > 0
      then raise CompileError
   else ()

fun checkFile (f: File.t, {fail: string -> 'a, name, ok: unit -> 'a}): 'a = let
   fun check (test, msg, k) =
      if test f then
         k ()
      else
         fail (concat ["File ", name, " ", msg])
   in
      check (File.doesExist, "does not exist", fn () =>
             check (File.canRead, "cannot be read", ok))
   end

(*---------------------------------------------------*)
(*                  Compiler Passes                  *)
(*---------------------------------------------------*)

datatype 'a display =
    Layout of 'a -> Layout.t
  | Layouts of 'a * (Layout.t -> unit) -> unit

fun 'a sizeMessage (name: string, a: 'a): Layout.t =
   let open Layout
   in str (concat [name, " size = ",
                   Int.toCommaString (MLton.size a), " bytes"])
   end

val diagnosticWriter: (Layout.t -> unit) option ref = ref NONE

fun diagnostics f =
   case !diagnosticWriter of
      NONE => ()
    | SOME w => f w

fun diagnostic f = diagnostics (fn disp => disp (f ()))

fun saveToFile {arg: 'a,
                name: string option,
                toFile = {display: 'a display, style: style, suffix: string},
                verb: Verbosity.t}: unit =
   let
      val name =
         case name of
            NONE => concat [!inputFile, ".", suffix]
          | SOME name => concat [!inputFile, ".", name, ".", suffix]
      fun doit f =
         trace (verb, concat ["save ", name])
         Ref.fluidLet
         (inputFile, name, fn () =>
          File.withOut (!inputFile, fn out =>
                        f (fn l => (Layout.outputl (l, out)))))
   in
      case display of
         Layout layout =>
            doit (fn output =>
                  (outputHeader (style, output)
                   ; output (layout arg)))
       | Layouts layout =>
            doit (fn output =>
                  (outputHeader (style, output)
                   ; layout (arg, output)))
   end

fun maybeSaveToFile {arg: 'a, name: string, suffix: string, toFile}: unit =
   let
      val fullName = concat [name, ".", suffix]
      val keep =
         List.exists
         ([name, fullName], fn name =>
          (List.exists
           (!keepPasses, fn re =>
            Regexp.Compiled.matchesAll (re, name))))
   in
      if keep
         then saveToFile {arg = arg, name = SOME fullName,
                          toFile = toFile, verb = Pass}
         else ()
   end

(* Code for diagnosing a pass. *)
val wrapDiagnosing =
   fn {name: string,
       thunk: unit -> 'a} =>
   if not (List.exists (!diagPasses, fn re =>
                        Regexp.Compiled.matchesAll (re, name)))
      then thunk
   else fn () =>
        let
           val result = ref NONE
           val display =
              Layouts (fn ((), output) =>
                       (diagnosticWriter := SOME output
                        ; result := SOME (thunk ())
                        ; diagnosticWriter := NONE))
           val _ =
              saveToFile
              {arg = (),
               name = SOME name,
               toFile = {display = display, style = No, suffix = "diagnostic"},
               verb = Pass}
        in
           valOf (!result)
        end

(* Code for profiling a pass. *)
val wrapProfiling =
   fn {name: string,
       thunk: unit -> 'a} =>
   if MLton.Profile.isOn
      then if not (List.exists (!profPasses, fn re =>
                                Regexp.Compiled.matchesAll (re, name)))
              then thunk
           else fn () =>
                let
                   open MLton.Profile
                   val d = Data.malloc ()
                in
                   Exn.finally
                   (fn () => withData (d, thunk),
                    fn () => (Data.write (d, concat [!inputFile, ".", name, ".mlmon"])
                              ; Data.free d))
                end
   else thunk

fun translatePass {arg: 'a,
                   doit: 'a -> 'b,
                   keepIL: bool,
                   name: string,
                   srcToFile: {display: 'a display, style: style, suffix: string} option,
                   tgtStats: ('b -> Layout.t) option,
                   tgtToFile: {display: 'b display, style: style, suffix: string} option,
                   tgtTypeCheck: ('b -> unit) option}: 'b =
   let
      val thunk = fn () => doit arg
      val thunk = wrapDiagnosing {name = name, thunk = thunk}
      val thunk = wrapProfiling {name = name, thunk = thunk}
      val thunk = fn () =>
      let
         val () =
            Option.app
            (srcToFile, fn srcToFile =>
             maybeSaveToFile
             {arg = arg,
              name = name,
              suffix = "pre",
              toFile = srcToFile})
         val res = thunk ()
         val () =
            Option.app
            (tgtToFile, fn tgtToFile =>
             maybeSaveToFile
             {arg = res,
              name = name,
              suffix = "post",
              toFile = tgtToFile})
         val () =
            if !ControlFlags.typeCheck
               then Option.app (tgtTypeCheck, fn tgtTypeCheck =>
                                trace (Pass, concat ["typeCheck ", name, ".post"]) tgtTypeCheck res)
               else ()
         local
            val verb = Detail
         in
            val _ = message (verb, fn () => Layout.str (concat [name, ".post stats"]))
            val _ = indent ()
            val _ = Option.app (tgtStats, fn tgtStats => message (verb, fn () => tgtStats res))
            val _ = message (verb, PropertyList.stats)
            val _ = message (verb, HashSet.stats)
            val _ = unindent ()
         end
      in
         res
      end
      val res = trace (Pass, name) thunk ()
      val () =
         if keepIL
            then Option.app (tgtToFile, fn tgtToFile =>
                             saveToFile {arg = res, name = NONE, toFile = tgtToFile, verb = Pass})
            else ()
      val () =
         if List.exists (!stopPasses, fn re =>
                         Regexp.Compiled.matchesAll (re, name))
            then raise Stopped
            else ()
   in
      res
   end

fun simplifyPass {arg: 'a,
                  doit: 'a -> 'a,
                  execute: bool,
                  keepIL: bool,
                  name: string,
                  stats: 'a -> Layout.t,
                  toFile: {display: 'a display, style: style, suffix: string},
                  typeCheck: 'a -> unit}: 'a =
   if List.foldr (!executePasses, execute, fn ((re, new), old) =>
                  if Regexp.Compiled.matchesAll (re, name) then new else old)
      then translatePass {arg = arg,
                          doit = doit,
                          keepIL = keepIL,
                          name = name,
                          srcToFile = SOME toFile,
                          tgtStats = SOME stats,
                          tgtToFile = SOME toFile,
                          tgtTypeCheck = SOME typeCheck}
      else (messageStr (Pass, name ^ " skipped"); arg)

fun simplifyPasses {arg, passes, stats, toFile, typeCheck} =
   List.fold
   (passes, arg, fn ({doit, execute, name}, arg) =>
    simplifyPass {arg = arg,
                  doit = doit,
                  execute = execute,
                  keepIL = false,
                  name = name,
                  stats = stats,
                  toFile = toFile,
                  typeCheck = typeCheck})
end
