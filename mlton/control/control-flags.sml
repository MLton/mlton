(* Copyright (C) 2009-2012,2014 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure ControlFlags: CONTROL_FLAGS =
struct

structure C = Control ()
open C

structure Align =
   struct
      datatype t = Align4 | Align8

      val toString =
         fn Align4 => "4"
          | Align8 => "8"
   end

datatype align = datatype Align.t

val align = control {name = "align",
                     default = Align4,
                     toString = Align.toString}

val atMLtons = control {name = "atMLtons",
                        default = Vector.new0 (),
                        toString = fn v => Layout.toString (Vector.layout
                                                            String.layout v)}

structure Chunk =
   struct
      datatype t =
         OneChunk
       | ChunkPerFunc
       | Coalesce of {limit: int}

      val toString =
         fn OneChunk => "one chunk"
          | ChunkPerFunc => "chunk per function"
          | Coalesce {limit} => concat ["coalesce ", Int.toString limit]
   end

datatype chunk = datatype Chunk.t

val chunk = control {name = "chunk",
                     default = Coalesce {limit = 4096},
                     toString = Chunk.toString}

val closureConvertGlobalize = control {name = "closureConvertGlobalize",
                                       default = true,
                                       toString = Bool.toString}

val closureConvertShrink = control {name = "closureConvertShrink",
                                    default = true,
                                    toString = Bool.toString}

structure Codegen =
   struct
      datatype t =
         AMD64Codegen
       | CCodegen
       | LLVMCodegen
       | X86Codegen

      val all = [X86Codegen,AMD64Codegen,CCodegen,LLVMCodegen]

      val toString: t -> string =
         fn AMD64Codegen => "amd64"
          | CCodegen => "c"
          | LLVMCodegen => "llvm"
          | X86Codegen => "x86"
   end

datatype codegen = datatype Codegen.t

val codegen = control {name = "codegen",
                       default = Codegen.X86Codegen,
                       toString = Codegen.toString}

val contifyIntoMain = control {name = "contifyIntoMain",
                               default = false,
                               toString = Bool.toString}

val debug = control {name = "debug",
                     default = false,
                     toString = Bool.toString}

val defaultChar = control {name = "defaultChar",
                           default = "char8",
                           toString = fn s => s}
val defaultWideChar = control {name = "defaultWideChar",
                               default = "widechar32",
                               toString = fn s => s}
val defaultInt = control {name = "defaultInt",
                          default = "int32",
                          toString = fn s => s}
val defaultReal = control {name = "defaultReal",
                           default = "real64",
                           toString = fn s => s}
val defaultWord = control {name = "defaultWord",
                           default = "word32",
                           toString = fn s => s}

val diagPasses = 
   control {name = "diag passes",
            default = [],
            toString = List.toString 
                       (Layout.toString o 
                        Regexp.Compiled.layout)}

val dropPasses =
   control {name = "drop passes",
            default = [],
            toString = List.toString
                       (Layout.toString o
                        Regexp.Compiled.layout)}

structure Elaborate =
   struct
      structure DiagEIW =
         struct
            datatype t =
               Error
             | Ignore
             | Warn

            val fromString: string -> t option =
               fn "error" => SOME Error
                | "ignore" => SOME Ignore
                | "warn" => SOME Warn
                | _ => NONE

            val toString: t -> string = 
               fn Error => "error"
                | Ignore => "ignore"
                | Warn => "warn"
         end

      structure DiagDI =
         struct
            datatype t =
               Default
             | Ignore

            val fromString: string -> t option =
               fn "default" => SOME Default
                | "ignore" => SOME Ignore
                | _ => NONE

            val toString: t -> string = 
               fn Default => "default"
                | Ignore => "ignore"
         end

      structure ResolveScope =
         struct
            datatype t =
               Dec
             | Strdec
             | Topdec
             | Program

            val fromString: string -> t option =
               fn "dec" => SOME Dec
                | "strdec" => SOME Strdec
                | "topdec" => SOME Topdec
                | "program" => SOME Program
                | _ => NONE

            val toString: t -> string =
               fn Dec => "dec"
                | Strdec => "strdec"
                | Topdec => "topdec"
                | Program => "program"
         end

      structure Id =
         struct
            datatype t = T of {enabled: bool ref,
                               expert: bool,
                               name: string}
            fun equals (T {enabled = enabled1, ...}, 
                        T {enabled = enabled2, ...}) = 
               enabled1 = enabled2

            val enabled = fn (T {enabled, ...}) => !enabled
            val setEnabled = fn (T {enabled, ...}, b) => (enabled := b; true)
            val expert = fn (T {expert, ...}) => expert
            val name = fn (T {name, ...}) => name
         end
      structure Args =
         struct
            datatype t = T of {fillArgs: unit -> (unit -> unit),
                               processAnn: unit -> (unit -> unit),
                               processDef: unit -> bool}
            local
               fun make sel (T r) = sel r
            in
               fun processAnn args = (make #processAnn args) ()
               fun processDef args = (make #processDef args) ()
            end
         end
      datatype ('args, 'st) t = T of {args: 'args option ref,
                                      cur: 'st ref,
                                      def: 'st ref,
                                      id: Id.t}
      fun current (T {cur, ...}) = !cur
      fun default (T {def, ...}) = !def
      fun id (T {id, ...}) = id
      fun enabled ctrl = Id.enabled (id ctrl)
      fun expert ctrl = Id.expert (id ctrl)
      fun name ctrl = Id.name (id ctrl)
      fun equalsId (ctrl, id') = Id.equals (id ctrl, id')

      datatype ('a, 'b) parseResult =
         Bad | Deprecated of 'a | Good of 'b | Other
      val deGood = 
         fn Good z => z
          | _ => Error.bug "Control.Elaborate.deGood"

      val documentation: {choices: string list option,
                          expert: bool,
                          name: string} list ref = ref []

      fun document {expert} =
         let
            val all = !documentation
            val all =
               if expert then all
               else List.keepAll (all, not o #expert)
            val all =
               List.insertionSort
               (all, fn ({name = n, ...}, {name = n', ...}) => n <= n')
            open Layout
         in
            align
            (List.map
             (all, fn {choices, name, ...} =>
              str (concat [name,
                           case choices of
                              NONE => ""
                            | SOME cs =>
                                 concat [" {",
                                         concat (List.separate (cs, "|")),
                                         "}"]])))
         end

      local 
         fun make ({choices: 'st list option,
                    default: 'st,
                    expert: bool,
                    toString: 'st -> string,
                    name: string,
                    newCur: 'st * 'args -> 'st,
                    newDef: 'st * 'args -> 'st,
                    parseArgs: string list -> 'args option},
                   {parseId: string -> (Id.t list, Id.t) parseResult,
                    parseIdAndArgs: string -> ((Id.t * Args.t) list, (Id.t * Args.t)) parseResult,
                    withDef: unit -> (unit -> unit),
                    snapshot: unit -> unit -> (unit -> unit)}) =
            let
               val () =
                  List.push
                  (documentation,
                   {choices = Option.map (choices, fn cs =>
                                          List.map (cs, toString)),
                    expert = expert,
                    name = name})
               val ctrl as T {args = argsRef, cur, def, 
                              id as Id.T {enabled, ...}, ...} =
                  T {args = ref NONE,
                     cur = ref default,
                     def = control {name = concat ["elaborate ", name,
                                                   " (default)"],
                                    default = default,
                                    toString = toString},
                     id = Id.T {enabled = control {name = concat ["elaborate ", name,
                                                                  " (enabled)"],
                                                   default = true,
                                                   toString = Bool.toString},
                                expert = expert,
                                name = name}}
               val parseId = fn name' =>
                  if String.equals (name', name) 
                     then Good id 
                     else parseId name'
               val parseIdAndArgs = fn s =>
                  case String.tokens (s, Char.isSpace) of
                     name'::args' =>
                        if String.equals (name', name)
                           then 
                              case parseArgs args' of
                                 SOME v => 
                                    let
                                       fun fillArgs () =
                                          (argsRef := SOME v
                                           ; fn () => argsRef := NONE)
                                       fun processAnn () =
                                          if !enabled
                                             then let
                                                     val old = !cur
                                                     val new = newCur (old, v)
                                                  in
                                                     cur := new
                                                     ; fn () => cur := old
                                                  end
                                             else fn () => ()
                                       fun processDef () =
                                          let
                                             val old = !def
                                             val new = newDef (old, v)
                                          in
                                             def := new
                                             ; true
                                          end
                                       val args =
                                          Args.T {fillArgs = fillArgs,
                                                  processAnn = processAnn,
                                                  processDef = processDef}
                                    in
                                       Good (id, args)
                                    end
                               | NONE => Bad
                           else parseIdAndArgs s
                   | _ => Bad
               val withDef : unit -> (unit -> unit) =
                  fn () =>
                  let
                     val restore = withDef ()
                     val old = !cur
                  in
                     cur := !def
                     ; fn () => (cur := old
                                 ; restore ())
                  end
               val snapshot : unit -> unit -> (unit -> unit) =
                  fn () =>
                  let 
                     val withSaved = snapshot ()
                     val saved = !cur 
                  in
                     fn () =>
                     let
                        val restore = withSaved ()
                        val old = !cur
                     in
                        cur := saved
                        ; fn () => (cur := old
                                    ; restore ())
                     end
                  end
            in
               (ctrl, 
                {parseId = parseId,
                 parseIdAndArgs = parseIdAndArgs,
                 withDef = withDef,
                 snapshot = snapshot})
            end

         fun makeBool ({default: bool,
                        expert: bool,
                        name: string}, ac) =
            make ({choices = SOME (if default then [true, false]
                                   else [false, true]),
                   default = default,
                   expert = expert,
                   toString = Bool.toString,
                   name = name,
                   newCur = fn (_,b) => b,
                   newDef = fn (_,b) => b,
                   parseArgs = fn args' =>
                               case args' of
                                  [arg'] => Bool.fromString arg'
                                | _ => NONE}, 
                  ac)

         fun makeDiagnostic ({choices,
                              default,
                              diagToString,
                              diagFromString,
                              expert: bool,
                              name: string}, ac) =
             make ({choices = choices,
                    default = default,
                    expert = expert,
                    toString = diagToString,
                    name = name,
                    newCur = fn (_,d) => d,
                    newDef = fn (_,d) => d,
                    parseArgs = fn args' =>
                                case args' of
                                   [arg'] => diagFromString arg'
                                 | _ => NONE},
                   ac)
         fun makeDiagEIW ({default: DiagEIW.t,
                           expert: bool,
                           name: string}, ac) =
            makeDiagnostic ({choices = (SOME
                                        (let
                                            datatype z = datatype DiagEIW.t
                                         in
                                            case default of
                                               Error => [Error, Ignore, Warn]
                                             | Ignore => [Ignore, Error, Warn]
                                             | Warn => [Warn, Ignore, Error]
                                         end)),
                             default = default,
                             diagToString = DiagEIW.toString,
                             diagFromString = DiagEIW.fromString,
                             expert = expert,
                             name = name}, ac)
         fun makeDiagDI ({default: DiagDI.t,
                          expert: bool,
                          name: string}, ac) =
            makeDiagnostic ({choices = (SOME
                                        (let
                                            datatype z = datatype DiagDI.t
                                         in
                                            case default of
                                               Default => [Default, Ignore]
                                             | Ignore => [Ignore, Default]
                                         end)),
                             default = default,
                             diagToString = DiagDI.toString,
                             diagFromString = DiagDI.fromString,
                             expert = expert,
                             name = name}, ac)
      in
         val ac =
            {parseId = fn _ => Bad,
             parseIdAndArgs = fn _ => Bad,
             withDef = fn () => (fn () => ()),
             snapshot = fn () => fn () => (fn () => ())}
         val (allowConstant, ac) =
            makeBool ({name = "allowConstant", 
                       default = false, expert = true}, ac)
         val (allowFFI, ac) =
            makeBool ({name = "allowFFI",
                       default = false, expert = false}, ac)
         val (allowPrim, ac) =
            makeBool ({name = "allowPrim", 
                       default = false, expert = true}, ac)
         val (allowOverload, ac) =
            makeBool ({name = "allowOverload", 
                       default = false, expert = true}, ac)
         val (allowRebindEquals, ac) =
            makeBool ({name = "allowRebindEquals", 
                       default = false, expert = true}, ac)
         val (deadCode, ac) =
            makeBool ({name = "deadCode", 
                       default = false, expert = true}, ac)
         val (forceUsed, ac) =
            make ({choices = NONE,
                   default = false,
                   expert = false,
                   toString = Bool.toString,
                   name = "forceUsed",
                   newCur = fn (b,()) => b,
                   newDef = fn (_,()) => true,
                   parseArgs = fn args' =>
                               case args' of
                                  [] => SOME ()
                                | _ => NONE},
                  ac)
         val (ffiStr, ac) =
            make ({choices = SOME [SOME "<longstrid>"],
                   default = NONE,
                   expert = true,
                   toString = fn NONE => "" | SOME s => s,
                   name = "ffiStr",
                   newCur = fn (_,s) => SOME s,
                   newDef = fn _ => NONE,
                   parseArgs = fn args' =>
                               case args' of
                                  [s] => SOME s
                                | _ => NONE},
                  ac)
         val (nonexhaustiveExnMatch, ac) =
             makeDiagDI ({name = "nonexhaustiveExnMatch",
                          default = DiagDI.Default, expert = false}, ac)
         val (nonexhaustiveMatch, ac) =
             makeDiagEIW ({name = "nonexhaustiveMatch", 
                           default = DiagEIW.Warn, expert = false}, ac)
         val (redundantMatch, ac) =
             makeDiagEIW ({name = "redundantMatch", 
                           default = DiagEIW.Warn, expert = false}, ac)
         val (resolveScope, ac) =
            make ({choices = SOME [ResolveScope.Dec, ResolveScope.Strdec, ResolveScope.Topdec, ResolveScope.Program],
                   default = ResolveScope.Strdec,
                   expert = true,
                   toString = ResolveScope.toString,
                   name = "resolveScope",
                   newCur = fn (_,rs) => rs,
                   newDef = fn (_,rs) => rs,
                   parseArgs = fn args' =>
                               case args' of
                                  [arg'] => ResolveScope.fromString arg'
                                | _ => NONE},
                  ac)
         val (sequenceNonUnit, ac) =
            makeDiagEIW ({name = "sequenceNonUnit", 
                          default = DiagEIW.Ignore, expert = false}, ac)
         val (warnUnused, ac) =
            makeBool ({name = "warnUnused", 
                       default = false, expert = false}, ac)

         val {parseId, parseIdAndArgs, withDef, snapshot} = ac
      end

      local
         fun makeDeprecated ({alts: string list,
                              name: string,
                              parseArgs: string list -> string list option},
                             {parseId: string -> (Id.t list, Id.t) parseResult,
                              parseIdAndArgs: string -> ((Id.t * Args.t) list, (Id.t * Args.t)) parseResult}) =
            let
               val parseId = fn name' =>
                  if String.equals (name', name) 
                     then Deprecated (List.map (alts, deGood o parseId))
                     else parseId name'
               val parseIdAndArgs = fn s =>
                  case String.tokens (s, Char.isSpace) of
                     name'::args' =>
                        if String.equals (name', name)
                           then 
                              case parseArgs args' of
                                 SOME alts => 
                                    Deprecated (List.map (alts, deGood o parseIdAndArgs))
                               | NONE => Bad
                           else parseIdAndArgs s
                   | _ => Bad
            in
               {parseId = parseId,
                parseIdAndArgs = parseIdAndArgs}
            end
         fun makeDeprecatedBool ({altIds: string list,
                                  altArgs: bool -> string list list,
                                  name: string},
                                 ac) =
            let
               local
                  fun make b =
                     List.map2
                     (altIds, altArgs b, fn (altId, altArgs) =>
                      String.concatWith (altId::altArgs, " "))
               in
                  val trueAltIdAndArgs = make true
                  val falseAltIdAndArgs = make false
               end
            in
               makeDeprecated ({alts = altIds,
                                name = name,
                                parseArgs = fn args' =>
                                            case args' of
                                               [arg'] => 
                                                  (case Bool.fromString arg' of
                                                      SOME true => SOME trueAltIdAndArgs
                                                    | SOME false => SOME falseAltIdAndArgs
                                                    | NONE => NONE)
                                             | _ => NONE}, 
                               ac)
            end
         val _ = makeDeprecatedBool
      in
         val ac = {parseId = parseId, parseIdAndArgs = parseIdAndArgs}
         val {parseId, parseIdAndArgs} = ac
      end

      local
         fun checkPrefix (s, f) =
            case String.peeki (s, fn (_, c) => c = #":") of
               NONE => f s
             | SOME (i, _) =>
                  let
                     val comp = String.prefix (s, i)
                     val comp = String.deleteSurroundingWhitespace comp
                     val s = String.dropPrefix (s, i + 1)
                  in
                     if String.equals (comp, "mlton")
                        then f s
                        else Other
                  end
      in
         val parseId = fn s => checkPrefix (s, parseId)
         val parseIdAndArgs = fn s => checkPrefix (s, parseIdAndArgs)
      end

      val processDefault = fn s =>
         case parseIdAndArgs s of
            Bad => Bad
          | Deprecated alts =>
               List.fold
               (alts, Deprecated (List.map (alts, #1)), fn ((_,args),res) =>
                if Args.processDef args then res else Bad)
          | Good (_, args) => if Args.processDef args then Good () else Bad
          | Other => Bad

      val processEnabled = fn (s, b) =>
         case parseId s of
            Bad => Bad
          | Deprecated alts => 
               List.fold
               (alts, Deprecated alts, fn (id,res) =>
                if Id.setEnabled (id, b) then res else Bad)
          | Good id => if Id.setEnabled (id, b) then Good () else Bad
          | Other => Bad

      val withDef : (unit -> 'a) -> 'a = fn f =>
         let
            val restore = withDef ()
         in
            Exn.finally (f, restore)
         end

      val snapshot : unit -> (unit -> 'a) -> 'a = fn () =>
         let
            val withSaved = snapshot ()
         in
            fn f =>
            let
               val restore = withSaved ()
            in
               Exn.finally (f, restore)
            end
         end

   end

val elaborateOnly =
   control {name = "elaborate only",
            default = false,
            toString = Bool.toString}

val emitMain =
   control {name = "emit main",
            default = true,
            toString = Bool.toString}

val exportHeader =
   control {name = "export header",
            default = NONE,
            toString = Option.toString File.toString}

val exnHistory = control {name = "exn history",
                          default = false,
                          toString = Bool.toString}

structure Format =
   struct
      datatype t =
         Archive
       | Executable
       | LibArchive
       | Library

      (* Default option first for usage message. *)
      val all = [Executable, Archive, LibArchive, Library]

      val toString: t -> string =
        fn Archive => "archive"
         | Executable => "executable"
         | LibArchive => "libarchive"
         | Library => "library"
   end

datatype format = datatype Format.t

val format = control {name = "generated output format",
                      default = Format.Executable,
                      toString = Format.toString}

structure GcCheck =
   struct
      datatype t =
         Limit
       | First
       | Every

      local open Layout
      in
         val layout =
            fn Limit => str "Limit"
             | First => str "First"
             | Every => str "Every"
      end
      val toString = Layout.toString o layout
   end

datatype gcCheck = datatype GcCheck.t

val gcCheck = control {name = "gc check",
                       default = Limit,
                       toString = GcCheck.toString}

val indentation = control {name = "indentation",
                           default = 3,
                           toString = Int.toString}

val inlineIntoMain = control {name = "inlineIntoMain",
                              default = true,
                              toString = Bool.toString}

val inlineLeafA = 
   control {name = "inlineLeafA",
            default = {loops = true,
                       repeat = true,
                       size = SOME 20},
            toString =
            fn {loops, repeat, size} =>
            Layout.toString
            (Layout.record [("loops", Bool.layout loops),
                            ("repeat", Bool.layout repeat),
                            ("size", Option.layout Int.layout size)])}
val inlineLeafB = 
   control {name = "inlineLeafB",
            default = {loops = true,
                       repeat = true,
                       size = SOME 40},
            toString =
            fn {loops, repeat, size} =>
            Layout.toString
            (Layout.record [("loops", Bool.layout loops),
                            ("repeat", Bool.layout repeat),
                            ("size", Option.layout Int.layout size)])}

val inlineNonRec =
   control {name = "inlineNonRec",
            default = {small = 60,
                       product = 320},
            toString =
            fn {small, product} =>
            Layout.toString
            (Layout.record [("small", Int.layout small),
                            ("product", Int.layout product)])}

val inputFile = control {name = "input file",
                         default = "<bogus>",
                         toString = File.toString}

val keepAST = control {name = "keep AST",
                       default = false,
                       toString = Bool.toString}

val keepCoreML = control {name = "keep CoreML",
                          default = false,
                          toString = Bool.toString}

val keepDefUse = control {name = "keep def use",
                          default = true,
                          toString = Bool.toString}

val keepDot = control {name = "keep dot",
                       default = false,
                       toString = Bool.toString}

val keepMachine = control {name = "keep Machine",
                           default = false,
                           toString = Bool.toString}

val keepPasses = control {name = "keep passes",
                          default = [],
                          toString = List.toString
                                     (Layout.toString o
                                      Regexp.Compiled.layout)}

val keepRSSA = control {name = "keep RSSA",
                        default = false,
                        toString = Bool.toString}

val keepSSA = control {name = "keep SSA",
                       default = false,
                       toString = Bool.toString}

val keepSSA2 = control {name = "keep SSA2",
                        default = false,
                        toString = Bool.toString}

val keepSXML = control {name = "keep SXML",
                        default = false,
                        toString = Bool.toString}


val keepXML = control {name = "keep XML",
                       default = false,
                       toString = Bool.toString}

val labelsHaveExtra_ = control {name = "extra_",
                                default = false,
                                toString = Bool.toString}

val libDir = control {name = "lib dir",
                      default = "<libDir unset>",
                      toString = fn s => s}

val libTargetDir = control {name = "lib target dir",
                            default = "<libTargetDir unset>",
                            toString = fn s => s} 

val libname = ref ""

val loopPasses = control {name = "loop passes",
                          default = 1,
                          toString = Int.toString}

val markCards = control {name = "mark cards",
                         default = true,
                         toString = Bool.toString}

val maxFunctionSize = control {name = "max function size",
                               default = 10000,
                               toString = Int.toString}

val mlbPathVars =
   control
   {name = "mlb path vars",
    default = [],
    toString = List.toString
               (fn {var, path} =>
                   concat ["{var = ", var, ", path = ", path, "}"])}

structure Native =
   struct
      val commented = control {name = "native commented",
                               default = 0,
                               toString = Int.toString}

      val liveStack = control {name = "native live stack",
                               default = false,
                               toString = Bool.toString}

      val optimize = control {name = "native optimize",
                              default = 1,
                              toString = Int.toString}

      val moveHoist = control {name = "native move hoist",
                               default = true,
                               toString = Bool.toString}

      val copyProp = control {name = "native copy prop",
                              default = true,
                              toString = Bool.toString}

      val copyPropCutoff = control {name = "native copy prop cutoff",
                                    default = 1000,
                                    toString = Int.toString}

      val cutoff = control {name = "native cutoff",
                            default = 100,
                            toString = Int.toString}

      val liveTransfer = control {name = "native live transfer",
                                  default = 8,
                                  toString = Int.toString}

      val shuffle = control {name = "native shuffle",
                             default = true,
                             toString = Bool.toString}

      val IEEEFP = control {name = "native ieee fp",
                            default = false,
                            toString = Bool.toString}

      val split = control {name = "native split",
                           default = SOME 20000,
                           toString = Option.toString Int.toString}
   end

val optimizationPasses:
   {il: string, set: string -> unit Result.t, get: unit -> string} list ref =
   control {name = "optimizationPasses",
            default = [],
            toString = List.toString 
                       (fn {il,get,...} => concat ["<",il,"::",get (),">"])}

val polyvariance =
   control {name = "polyvariance",
            default = SOME {hofo = true,
                            rounds = 2,
                            small = 30,
                            product = 300},
            toString =
            fn p =>
            Layout.toString
            (Option.layout
             (fn {hofo, rounds, small, product} =>
              Layout.record [("hofo", Bool.layout hofo),
                             ("rounds", Int.layout rounds),
                             ("small", Int.layout small),
                             ("product", Int.layout product)])
             p)}

val positionIndependent = ref false

val preferAbsPaths = control {name = "prefer abs paths",
                              default = false,
                              toString = Bool.toString}

val profPasses = 
   control {name = "prof passes",
            default = [],
            toString = List.toString 
            (Layout.toString o 
             Regexp.Compiled.layout)}

structure Profile =
   struct
      datatype t =
         ProfileNone
       | ProfileAlloc
       | ProfileCallStack
       | ProfileCount
       | ProfileDrop
       | ProfileLabel
       | ProfileTimeField
       | ProfileTimeLabel

      val toString =
         fn ProfileNone => "None"
          | ProfileAlloc => "Alloc"
          | ProfileCallStack => "CallStack"
          | ProfileCount => "Count"
          | ProfileDrop => "Drop"
          | ProfileLabel => "Label"
          | ProfileTimeField => "TimeField"
          | ProfileTimeLabel => "TimeLabel"
   end

datatype profile = datatype Profile.t

val profile = control {name = "profile",
                       default = ProfileNone,
                       toString = Profile.toString}

val profileBranch = control {name = "profile branch",
                             default = false,
                             toString = Bool.toString}

val profileC = control {name = "profile C",
                        default = [],
                        toString = List.toString
                                   (Layout.toString o 
                                    Regexp.Compiled.layout)}

structure ProfileIL =
   struct
      datatype t = ProfileSource | ProfileSSA | ProfileSSA2

      val toString =
         fn ProfileSource => "ProfileSource"
          | ProfileSSA => "ProfileSSA"
          | ProfileSSA2 => "ProfileSSA2"
   end

datatype profileIL = datatype ProfileIL.t

val profileIL = control {name = "profile IL",
                         default = ProfileSource,
                         toString = ProfileIL.toString}

val profileInclExcl = 
   control {name = "profile include/exclude",
            default = [],
            toString = List.toString
                       (Layout.toString o 
                        (Layout.tuple2 (Regexp.Compiled.layout, 
                                        Bool.layout)))}

val profileRaise = control {name = "profile raise",
                            default = false,
                            toString = Bool.toString}

val profileStack = control {name = "profile stack",
                            default = false,
                            toString = Bool.toString}

val profileVal = control {name = "profile val",
                          default = false,
                          toString = Bool.toString}

val showBasis = control {name = "show basis",
                         default = NONE,
                         toString = Option.toString File.toString}

val showDefUse = control {name = "show def-use",
                          default = NONE,
                          toString = Option.toString File.toString}

val showTypes = control {name = "show types",
                         default = true,
                         toString = Bool.toString}

structure Target =
   struct
      datatype t =
         Cross of string
       | Self
         
      val toString =
         fn Cross s => s
          | Self => "self"
   end

datatype target = datatype Target.t

val target = control {name = "target",
                      default = Self,
                      toString = Target.toString}

structure Target =
   struct
      open Target
      
      datatype arch = datatype MLton.Platform.Arch.t
         
      val arch = control {name = "target arch",
                          default = X86,
                          toString = MLton.Platform.Arch.toString}

      datatype os = datatype MLton.Platform.OS.t

      val os = control {name = "target OS",
                        default = Linux,
                        toString = MLton.Platform.OS.toString}

      fun make s =
         let
            val r = ref NONE
            fun get () =
               case !r of
                  NONE => Error.bug ("ControlFlags.Target." ^ s ^ ": not set")
                | SOME x => x
            fun set x = r := SOME x
         in
            (get, set)
         end
      val (bigEndian: unit -> bool, setBigEndian) = make "bigEndian"

      structure Size =
         struct
            val (cint: unit -> Bits.t, set_cint) = make "Size.cint"
            val (cpointer: unit -> Bits.t, set_cpointer) = make "Size.cpointer"
            val (cptrdiff: unit -> Bits.t, set_cptrdiff) = make "Size.cptrdiff"
            val (csize: unit -> Bits.t, set_csize) = make "Size.csize"
            val (header: unit -> Bits.t, set_header) = make "Size.header"
            val (mplimb: unit -> Bits.t, set_mplimb) = make "Size.mplimb"
            val (objptr: unit -> Bits.t, set_objptr) = make "Size.objptr"
            val (seqIndex: unit -> Bits.t, set_seqIndex) = make "Size.seqIndex"
         end
      fun setSizes {cint, cpointer, cptrdiff, csize, 
                    header, mplimb, objptr, seqIndex} =
         (Size.set_cint cint
          ; Size.set_cpointer cpointer
          ; Size.set_cptrdiff cptrdiff
          ; Size.set_csize csize
          ; Size.set_header header
          ; Size.set_mplimb mplimb
          ; Size.set_objptr objptr
          ; Size.set_seqIndex seqIndex)
   end

fun mlbPathMap () =
   List.rev
      (List.concat
          [[{var = "LIB_MLTON_DIR",
             path = !libDir},
            {var = "TARGET",
             path = Target.toString (!target)},
            {var = "TARGET_ARCH",
             path = String.toLower (MLton.Platform.Arch.toString
                                    (!Target.arch))},
            {var = "TARGET_OS",
             path = String.toLower (MLton.Platform.OS.toString
                                    (!Target.os))},
            {var = "OBJPTR_REP",
             path = (case Bits.toInt (Target.Size.objptr ()) of
                        32 => "rep32"
                      | 64 => "rep64"
                      | _ => Error.bug "Control.mlbPathMap")},
            {var = "HEADER_WORD",
             path = (case Bits.toInt (Target.Size.header ()) of
                        32 => "word32"
                      | 64 => "word64"
                      | _ => Error.bug "Control.mlbPathMap")},
            {var = "SEQINDEX_INT",
             path = (case Bits.toInt (Target.Size.seqIndex ()) of
                        32 => "int32"
                      | 64 => "int64"
                      | _ => Error.bug "Control.mlbPathMap")},
            {var = "DEFAULT_CHAR",
             path = !defaultChar},
            {var = "DEFAULT_WIDECHAR",
             path = !defaultWideChar},
            {var = "DEFAULT_INT",
             path = !defaultInt},
            {var = "DEFAULT_REAL",
             path = !defaultReal},
            {var = "DEFAULT_WORD",
             path = !defaultWord}],
           !mlbPathVars])

val typeCheck = control {name = "type check",
                         default = false,
                         toString = Bool.toString}

structure Verbosity =
   struct
      datatype t =
         Silent
       | Top
       | Pass
       | Detail

      val toString =
         fn Silent => "Silent"
          | Top => "Top"
          | Pass => "Pass"
          | Detail => "Detail"
   end

datatype verbosity = datatype Verbosity.t

val verbosity = control {name = "verbosity",
                         default = Silent,
                         toString = Verbosity.toString}

val warnAnn = control {name = "warn unrecognized annotation",
                       default = true,
                       toString = Bool.toString}

val warnDeprecated = control {name = "warn deprecated features",
                              default = true,
                              toString = Bool.toString}

val zoneCutDepth: int ref =
   control {name = "zone cut depth",
            default = 100,
            toString = Int.toString}

val defaults = setDefaults

val _ = defaults ()

end
