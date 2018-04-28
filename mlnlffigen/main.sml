(* Copyright (C) 2005-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Main : sig val main : unit -> unit end =
struct

structure RE =
   RegExpFn (structure P = AwkSyntax
             structure E = DfaEngine)

fun makeOptions {usage} =
   let
      open Popt Control
   in
      List.map
      ([(Expert, "debug", " {false|true}", "",
         boolRef debug),
        (Normal, "allSU", " {false|true}",
         "generate ML definitions for all #include-d struct and union definitions",
         boolRef allSU),
        (Normal, "collect", " {true|false}",
         "collect enum constants from unnamed enumerateions",
         boolRef collect_enums),
        (Normal, "cppopt", " <opt>",
         "pass option to preprocessor",
         SpaceString (fn s => List.push (cppopts, s))),
        (Normal, "dir", " <dir>",
         "output directory for generated files",
         SpaceString (fn s => dir := s)),
        (Normal, "enum-constructors", " {false|true}",
         "when possible, make the ML representation type of enumerations a datatype",
         boolRef enum_cons),
        (Normal, "gensym", " <string>",
         "suffix for \"gensym-ed\" generated ML structure names",
         SpaceString (fn s => gensym := s)),
        (Normal, "heavy", "",
         "suppress 'light' versions of function wrappers and field accessors",
         None (fn () => weight := {heavy = true, light = false})),
        (Normal, "include", " <file>",
         "include file in the generated .mlb file",
         SpaceString (fn s => List.push (extramembers, s))),
        (Normal, "libhandle", " <longid>",
         "Use the <longid> to refer to the handle to the shared library",
         SpaceString (fn s => libhandle := s)),
        (Normal, "light", "",
         "suppress 'heavy' versions of function wrappers and field accessors",
         None (fn () => weight := {heavy = false, light = true})),
        (Normal, "linkage", " {archive|dynamic|shared}",
         "how to link C objects",
         SpaceString (fn s =>
                      if s = "archive" orelse s = "static"
                         then linkage := Linkage.Archive
                      else if s = "dynamic"
                         then linkage := Linkage.Dynamic
                      else if s = "shared"
                         then linkage := Linkage.Shared
                      else usage (concat ["invalid -linkage arg: ", s]))),
        (Normal, "match", " <re>",
         "generate ML definitions for #include-d definitions matching <re>",
         SpaceString (fn re =>
                      let
                         val regexp =
                            SOME (RE.compileString re)
                            handle RegExpSyntax.CannotParse => NONE
                                 | RegExpSyntax.CannotCompile => NONE
                      in
                         case regexp of
                            SOME regexp =>
                               let
                                  val scanFn = RE.prefix regexp
                                  fun matchFn s =
                                     let
                                        val n = String.length s
                                        fun getc i =
                                           if (i < n)
                                              then SOME (String.sub (s, i), i + 1)
                                              else NONE
                                     in
                                        case scanFn getc 0 of
                                           NONE => false
                                         | SOME (x, k) => k = n
                                     end
                               in
                                  match := matchFn
                               end
                          | NONE => usage (concat ["invalid -match arg: ", re])
                      end)),
        (Normal, "mlbfile", " <file>",
         "name of the generated .mlb file",
         SpaceString (fn s => mlbfile := s)),
        (Normal, "namedargs", " {false|true}",
         "generate function wrappers with named arguments",
         boolRef namedargs),
        (Normal, "prefix", " <string>",
         "prefix for generated ML structure names",
         SpaceString (fn s => prefix := s)),
        (Normal, "target", " <arch>-<os>",
         "platform that executable will run on",
         SpaceString (fn s =>
                      (case Target.fromString s of
                          NONE =>
                             usage (concat ["invalid -target arg: ", s])
                        | SOME t =>
                             (case Target.make t of
                                 NONE => usage (concat ["unsupported -target arg: ", s])
                               | SOME z => target := SOME z)))),
        (Normal, "width", " 75",
         "output line width for pretty-printing",
         intRef width)],
       fn (style, name, arg, desc, opt) =>
       {arg = arg, desc = desc, name = name, opt = opt, style = style})
   end

val mainUsage = "mlnlffigen [option ...] C-file ..."
val {parse, usage} =
   Popt.makeUsage {mainUsage = mainUsage,
                   makeOptions = makeOptions,
                   showExpert = fn () => !Control.debug}

val die = Process.fail

fun commandLine args =
   let
      val rest = parse args
      val () = if Option.isNone (!Control.target)
                  then usage "no -target specified"
                  else ()
    in
       case rest of
          Result.No msg => usage msg
        | Result.Yes [] => usage "no C-file(s)"
        | Result.Yes cfiles => Gen.gen {cfiles = cfiles}
   end

val main = Process.makeMain commandLine

end
