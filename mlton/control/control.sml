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

val traceRaising = ref false

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
                 let
                    val done = done ()
                 in
                    if !traceRaising
                       then ()
                       else (messageStr (verb, concat [name, " raised: ", Exn.toString exn])
                             ; (case Exn.history exn of
                                   [] => ()
                                 | history =>
                                      (messageStr (verb, concat [name, " raised with history: "])
                                       ; indent ()
                                       ; (List.foreach
                                          (history, fn s =>
                                           messageStr (verb, s)))
                                       ; unindent ()))
                             ; traceRaising := true)
                          ; messageStr (verb, concat [name, " raised in ", done])
                          ; raise exn
                 end
           end
      else f a

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
                 (if !traceRaising
                     then ()
                     else (messageStr (verb, concat [name, " raised: ", Exn.toString exn])
                           ; (case Exn.history exn of
                                 [] => ()
                               | history =>
                                    (messageStr (verb, concat [name, " raised with history: "])
                                     ; indent ()
                                     ; (List.foreach
                                        (history, fn s =>
                                         messageStr (verb, s)))
                                     ; unindent ()))
                           ; traceRaising := true)
                  ; raise exn)
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

val die = Process.fail

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
            then die "compilation aborted: too many errors"
         else ()
      end
end

fun errorStr (r, msg) = error (r, Layout.str msg, Layout.empty)

fun checkForErrors (name: string) =
   if !numErrors > 0
      then die (concat ["compilation aborted: ", name, " reported errors"])
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
   NoDisplay
  | Layout of 'a -> Layout.t
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
                toFile = {display: 'a display, style: style, suffix: string}}: unit =
   let
      val name =
         case name of
            NONE => concat [!inputFile, ".", suffix]
          | SOME name => concat [!inputFile, ".", name, ".", suffix]
      fun doit f =
         trace (Pass, "display")
         Ref.fluidLet
         (inputFile, name, fn () =>
          File.withOut (!inputFile, fn out =>
                        f (fn l => (Layout.outputl (l, out)))))
   in
      case display of
         NoDisplay => ()
       | Layout layout =>
            doit (fn output =>
                  (outputHeader (style, output)
                   ; output (layout arg)))
       | Layouts layout =>
            doit (fn output =>
                  (outputHeader (style, output)
                   ; layout (arg, output)))
   end

fun maybeSaveToFile {arg: 'a, name = (name: string, namex: string option), toFile}: unit =
   if not (List.exists (!keepPasses, fn re =>
                        Regexp.Compiled.matchesAll (re, name)))
      then ()
      else let
              val name =
                 case namex of
                    NONE => name
                  | SOME namex => concat [name, ".", namex]
           in
              saveToFile {arg = arg, name = SOME name, toFile = toFile}
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
               toFile = {display = display, style = No, suffix = "diagnostic"}}
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

fun pass {name = (name: string, namex: string option),
          stats: 'a -> Layout.t,
          thunk: unit -> 'a,
          toFile: {display: 'a display, style: style, suffix: string}}: 'a =
   let
      val thunk = wrapDiagnosing {name = name, thunk = thunk}
      val thunk = wrapProfiling {name = name, thunk = thunk}
      val result = trace (Pass, name) thunk ()
      val verb = Detail
      val _ = message (verb, fn () => Layout.str (concat [name, " stats"]))
      val _ = indent ()
      val _ = message (verb, fn () => sizeMessage (#suffix toFile, result))
      val _ = message (verb, fn () => stats result)
      val _ = message (verb, PropertyList.stats)
      val _ = message (verb, HashSet.stats)
      val _ = unindent ()
      val _ = checkForErrors name
      val _ = maybeSaveToFile {arg = result, name = (name, namex), toFile = toFile}
   in
      result
   end

fun passTypeCheck {name: string * string option,
                   stats: 'a -> Layout.t,
                   thunk: unit -> 'a,
                   toFile: {display: 'a display, style: style, suffix: string},
                   typeCheck: 'a -> unit}: 'a =
   let
      val result = pass {name = name,
                         stats = stats,
                         thunk = thunk,
                         toFile = toFile}
      val _ =
         if !ControlFlags.typeCheck
            then trace (Pass, "typeCheck") typeCheck result
         else ()
   in
      result
   end

fun simplifyPass {arg: 'a,
                  doit: 'a -> 'a,
                  execute: bool,
                  name: string,
                  stats: 'a -> Layout.t,
                  toFile: {display: 'a display, style: style, suffix: string},
                  typeCheck: 'a -> unit}: 'a =
   if List.foldr (!executePasses, execute, fn ((re, new), old) =>
                  if Regexp.Compiled.matchesAll (re, name) then new else old)
      then let
              val _ =
                 maybeSaveToFile
                 {arg = arg,
                  name = (name, SOME "pre"),
                  toFile = toFile}
              val r =
                 passTypeCheck
                 {name = (name, SOME "post"),
                  stats = stats,
                  thunk = fn () => doit arg,
                  toFile = toFile,
                  typeCheck = typeCheck}
           in
              r
           end
      else (messageStr (Pass, name ^ " skipped"); arg)

fun simplifyPasses {arg, passes, stats, toFile, typeCheck} =
   List.fold
   (passes, arg, fn ({doit, execute, name}, arg) =>
    simplifyPass {arg = arg,
                  doit = doit,
                  execute = execute,
                  name = name,
                  stats = stats,
                  toFile = toFile,
                  typeCheck = typeCheck})
end
