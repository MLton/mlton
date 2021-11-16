(* Copyright (C) 2009-2012,2014-2017,2019-2021 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure ControlFlags: CONTROL_FLAGS =
struct

structure StrMap =
struct
   type t = (string, string) HashTable.t

   fun new (): t =
      HashTable.new {hash = String.hash, equals = String.equals}

   fun add (table, name, value) =
      (ignore o HashTable.lookupOrInsert)
      (table, name, fn () => value)

   fun load (f: File.t): t =
      let
         val table = new ()
         val () =
            File.withIn
            (f, fn ins =>
             In.foreachLine
             (ins, fn l =>
              let
                 fun err () =
                    Error.bug (concat ["Control.StrMap.load: strange line: ", l])
              in
                 case String.peeki (l, fn (_, c) => c = #"=") of
                    NONE => err ()
                  | SOME (i, _) =>
                       let
                          val name = String.prefix (l, i)
                          val name = String.deleteSurroundingWhitespace name
                          val value = String.dropPrefix (l, i + 1)
                          val value = String.deleteSurroundingWhitespace value
                       in
                          add (table, name, value)
                       end
              end))
      in
         table
      end

   fun peek (table, name) =
      HashTable.peek (table, name)

   fun lookup (table, name) =
      case peek (table, name) of
         NONE => Error.bug (concat ["Control.StrMap.lookup: ", name])
       | SOME value => value

   fun lookupIntInf (table, name) =
      let
         val value = lookup (table, name)
      in
         case IntInf.fromString value of
            NONE => Error.bug (concat ["Control.StrMap.lookupIntInf: ", name, ", ", value])
          | SOME ii => ii
      end

   fun lookupBool (table, name) =
      let
         val value = lookup (table, name)
      in
         case Bool.fromString value of
            NONE => Error.bug (concat ["Control.StrMap.lookupBool: ", name, ", ", value])
          | SOME b => b
      end
end

structure C = Control ()
open C
fun layout' {pre, suf} =
   let
      open Layout
      val (pre, suf) = (str pre, str suf)
   in
      align
      ((seq [pre, str "control flags:", suf]) ::
       (List.map
        (all (), fn {name, value} =>
         seq [pre, str "   ", str name, str ": ", str value, suf])))
   end
fun layout () = layout' {pre = "", suf = ""}

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
val bounceRssaLimit = control {name = "bounceRssaLimit",
                               default = SOME 8,
                               toString = Option.toString Int.toString}
val bounceRssaLiveCutoff = control {name = "bounceRssaLiveCutoff",
                               default = SOME 12,
                               toString = Option.toString Int.toString}
val bounceRssaLoopCutoff = control {name = "bounceRssaLoopCutoff",
                               default = SOME 40,
                               toString = Option.toString Int.toString}
val bounceRssaUsageCutoff = control {name = "bounceRssaUsageCutoff",
                               default = SOME 15,
                               toString = Option.toString Int.toString}

val chunkBatch = control {name = "chunkBatch",
                          default = Int.pow(2,15),
                          toString = Int.toString}

structure Chunkify =
   struct
      datatype t =
         Coalesce of {limit: int}
       | Func
       | One
       | Simple of {mainFns: bool,
                    sccC: bool,
                    sccR: bool,
                    singC: bool,
                    singR: bool}

      val simpleDefault =
         Simple {mainFns = true,
                 sccC = true,
                 sccR = true,
                 singC = true,
                 singR = true}

      fun toString c =
         case c of
            Coalesce {limit} => concat ["coalesce", Int.toString limit]
          | Func => "func"
          | One => "one"
          | Simple {mainFns, sccC, sccR, singC, singR} =>
               let
                  open Layout
               in
                  toString
                  (namedRecord
                   ("simple",
                    [("mainFns", Bool.layout mainFns),
                     ("sccC", Bool.layout sccC),
                     ("sccR", Bool.layout sccR),
                     ("singC", Bool.layout singC),
                     ("singR", Bool.layout singR)]))
               end
      fun fromString s =
         let
            open Parse
            infix 1 <|> >>=
            infix  3 <*> <* *>
            infixr 4 <$> <$$> <$$$> <$$$$> <$ <$?>
            val p =
               any
               [str "coalesce" *>
                (peek (nextSat Char.isDigit) *>
                 fromScan (Function.curry Int.scan StringCvt.DEC)) >>= (fn limit =>
                pure (Coalesce {limit = limit})),
                str "func" *> pure Func,
                str "one" *> pure One,
                str "simple" *>
                (cbrack (ffield ("mainFns", bool) >>= (fn mainFns =>
                         nfield ("sccC", bool) >>= (fn sccC =>
                         nfield ("sccR", bool) >>= (fn sccR =>
                         nfield ("singC", bool) >>= (fn singC =>
                         nfield ("singR", bool) >>= (fn singR =>
                         pure (Simple {mainFns = mainFns,
                                       sccC = sccC,
                                       sccR = sccR,
                                       singC = singC,
                                       singR = singR})))))))
                 <|>
                 pure simpleDefault)]
               <* failing next
         in
            case parseString (p, s) of
               No _ => NONE
             | Yes c => SOME c
         end
   end

val chunkify = control {name = "chunkify",
                        default = Chunkify.Coalesce {limit = 4096},
                        toString = Chunkify.toString}

val chunkJumpTable = control {name = "chunkJumpTable",
                              default = false,
                              toString = Bool.toString}
val chunkMayRToSelfOpt = control {name = "chunkMayRToSelfOpt",
                                  default = true,
                                  toString = Bool.toString}
val chunkMustRToOtherOpt = control {name = "chunkMustRToOtherOpt",
                                    default = true,
                                    toString = Bool.toString}
val chunkMustRToSelfOpt = control {name = "chunkMustRToSelfOpt",
                                   default = true,
                                   toString = Bool.toString}
val chunkMustRToSingOpt = control {name = "chunkMustRToSingOpt",
                                   default = true,
                                   toString = Bool.toString}
val chunkTailCall = control {name = "chunkTailCall",
                             default = false,
                             toString = Bool.toString}

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

val codegenComments = control {name = "codegen comments",
                               default = 0,
                               toString = Int.toString}

val codegenFuseOpAndChk = control {name = "fuse `op` and `opCheckP` primitives in codegen",
                                   default = false,
                                   toString = Bool.toString}

val constPropAbsValLayoutDepth = control {name = "cut-off depth for printing of abstract values in`ConstantPropagation`",
                                          default = 2,
                                          toString = Int.toString}

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

val executePasses =
   control {name = "execute passes",
            default = [],
            toString = List.toString
                       (Layout.toString o
                        (Layout.tuple2
                         (Regexp.Compiled.layout, Bool.layout)))}

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

      datatype 'a parseResult =
         Bad | Good of 'a | Other | Proxy of 'a list * {deprecated: bool}
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
                   {parseId: string -> Id.t parseResult,
                    parseIdAndArgs: string list -> (Id.t * Args.t) parseResult,
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
               val parseIdAndArgs = fn ss =>
                  case ss of
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
                           else parseIdAndArgs ss
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
         val (allowRedefineSpecialIds, ac) =
            makeBool ({name = "allowRedefineSpecialIds",
                       default = false, expert = true}, ac)
         val (allowSpecifySpecialIds, ac) =
            makeBool ({name = "allowSpecifySpecialIds",
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
         val (nonexhaustiveBind, ac) =
             makeDiagEIW ({name = "nonexhaustiveBind",
                           default = DiagEIW.Warn, expert = false}, ac)
         val (nonexhaustiveExnBind, ac) =
             makeDiagDI ({name = "nonexhaustiveExnBind",
                          default = DiagDI.Default, expert = false}, ac)
         val (redundantBind, ac) =
             makeDiagEIW ({name = "redundantBind",
                           default = DiagEIW.Warn, expert = false}, ac)
         val (nonexhaustiveMatch, ac) =
             makeDiagEIW ({name = "nonexhaustiveMatch",
                           default = DiagEIW.Warn, expert = false}, ac)
         val (nonexhaustiveExnMatch, ac) =
             makeDiagDI ({name = "nonexhaustiveExnMatch",
                          default = DiagDI.Default, expert = false}, ac)
         val (redundantMatch, ac) =
             makeDiagEIW ({name = "redundantMatch",
                           default = DiagEIW.Warn, expert = false}, ac)
         val (nonexhaustiveRaise, ac) =
             makeDiagEIW ({name = "nonexhaustiveRaise",
                           default = DiagEIW.Ignore, expert = false}, ac)
         val (nonexhaustiveExnRaise, ac) =
             makeDiagDI ({name = "nonexhaustiveExnRaise",
                          default = DiagDI.Ignore, expert = false}, ac)
         val (redundantRaise, ac) =
             makeDiagEIW ({name = "redundantRaise",
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
         val (valrecConstr, ac) =
            makeDiagEIW ({name = "valrecConstr",
                          default = DiagEIW.Warn, expert = false}, ac)
         val (warnUnused, ac) =
            makeBool ({name = "warnUnused", 
                       default = false, expert = false}, ac)

         (* Successor ML *)
         val (allowDoDecls, ac) =
            makeBool ({name = "allowDoDecls",
                       default = false, expert = false}, ac)
         val (allowExtendedNumConsts, ac) =
            makeBool ({name = "allowExtendedNumConsts",
                       default = false, expert = false}, ac)
         val (allowExtendedTextConsts, ac) =
            makeBool ({name = "allowExtendedTextConsts",
                       default = false, expert = false}, ac)
         val (allowLineComments, ac) =
            makeBool ({name = "allowLineComments",
                       default = false, expert = false}, ac)
         val (allowOptBar, ac) =
            makeBool ({name = "allowOptBar",
                       default = false, expert = false}, ac)
         val (allowOptSemicolon, ac) =
            makeBool ({name = "allowOptSemicolon",
                       default = false, expert = false}, ac)
         val (allowOrPats, ac) =
            makeBool ({name = "allowOrPats",
                       default = false, expert = false}, ac)
         val (allowRecordPunExps, ac) =
            makeBool ({name = "allowRecordPunExps",
                       default = false, expert = false}, ac)
         val (allowSigWithtype, ac) =
            makeBool ({name = "allowSigWithtype",
                       default = false, expert = false}, ac)
         val (allowVectorExps, ac) =
            makeBool ({name = "allowVectorExps",
                       default = false, expert = false}, ac)
         val (allowVectorPats, ac) =
            makeBool ({name = "allowVectorPats",
                       default = false, expert = false}, ac)
         val extendedConstsCtrls =
            [allowExtendedNumConsts, allowExtendedTextConsts]
         val vectorCtrls =
            [allowVectorExps, allowVectorPats]
         val successorMLCtrls =
            [allowDoDecls, allowExtendedNumConsts,
             allowExtendedTextConsts, allowLineComments, allowOptBar,
             allowOptSemicolon, allowOrPats, allowRecordPunExps,
             allowSigWithtype, allowVectorExps, allowVectorPats]


         val {parseId, parseIdAndArgs, withDef, snapshot} = ac
      end

      local
         fun makeProxy ({alts: (Id.t * ('args -> string list option)) list,
                         choices: 'args list option,
                         deprecated: bool,
                         expert: bool,
                         toString: 'args -> string,
                         name: string,
                         parseArgs: string list -> 'args option},
                        {parseId: string -> Id.t parseResult,
                         parseIdAndArgs: string list -> (Id.t * Args.t) parseResult}) =
            let
               val () =
                  if deprecated then () else
                  List.push
                  (documentation,
                   {choices = Option.map (choices, fn cs =>
                                          List.map (cs, toString)),
                    expert = expert,
                    name = name})
               val parseId = fn name' =>
                  if String.equals (name', name) 
                     then Proxy (List.map (alts, fn (id, _) => id), {deprecated = deprecated})
                     else parseId name'
               val parseIdAndArgs = fn ss =>
                  case ss of
                     name'::args' =>
                        if String.equals (name', name)
                           then
                              case parseArgs args' of
                                 SOME v => let
                                              val alts =
                                                 List.keepAllMap
                                                 (alts, fn (id, mkArgs) =>
                                                  Option.map
                                                  (mkArgs v, fn ss =>
                                                   deGood (parseIdAndArgs ((Id.name id)::ss))))
                                           in
                                              Proxy (alts, {deprecated = deprecated})
                                           end
                               | NONE => Bad
                           else parseIdAndArgs ss
                   | _ => Bad
            in
               {parseId = parseId,
                parseIdAndArgs = parseIdAndArgs}
            end

         fun makeProxyBoolSimple ({alts: Id.t list,
                                   default: bool,
                                   deprecated: bool,
                                   expert: bool,
                                   name: string}, ac) =
            makeProxy ({alts = List.map (alts, fn id => (id, fn b => SOME [Bool.toString b])),
                        choices = SOME (if default then [true, false]
                                                   else [false, true]),
                        deprecated = deprecated,
                        expert = expert,
                        toString = Bool.toString,
                        name = name,
                        parseArgs = fn args' =>
                                    case args' of
                                       [arg'] => Bool.fromString arg'
                                     | _ => NONE},
                       ac)
      in
         val ac = {parseId = parseId, parseIdAndArgs = parseIdAndArgs}

         (* Successor ML *)
         val ac =
            makeProxyBoolSimple ({alts = List.map (extendedConstsCtrls, id),
                                  default = false,
                                  deprecated = false,
                                  expert = false,
                                  name = "allowExtendedConsts"}, ac)
         val ac =
            makeProxyBoolSimple ({alts = List.map (vectorCtrls, id),
                                  default = false,
                                  deprecated = false,
                                  expert = false,
                                  name = "allowVectorExpsAndPats"}, ac)
         val ac =
            makeProxyBoolSimple ({alts = List.map (successorMLCtrls, id),
                                  default = false,
                                  deprecated = false,
                                  expert = false,
                                  name = "allowSuccessorML"}, ac)

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
         val parseIdAndArgs = fn s => checkPrefix (s, fn s => parseIdAndArgs (String.tokens (s, Char.isSpace)))
      end

      val processDefault = fn s =>
         case parseIdAndArgs s of
            Bad => Bad
          | Good (id, args) => if Args.processDef args then Good id else Bad
          | Proxy (alts, {deprecated}) =>
               List.fold
               (alts, Proxy (List.map (alts, #1), {deprecated = deprecated}),
                fn ((_,args),res) =>
                if Args.processDef args then res else Bad)
          | Other => Bad

      val processEnabled = fn (s, b) =>
         case parseId s of
            Bad => Bad
          | Proxy (alts, {deprecated}) =>
               List.fold
               (alts, Proxy (alts, {deprecated = deprecated}),
                fn (id, res) =>
                if Id.setEnabled (id, b) then res else Bad)
          | Good id => if Id.setEnabled (id, b) then Good id else Bad
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

val forceHandlesSignals = control {name = "force handles signals",
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

val globalizeArrays = control {name = "globalize arrays",
                               default = false,
                               toString = Bool.toString}

val globalizeRefs = control {name = "globalize refs",
                             default = true,
                             toString = Bool.toString}

val globalizeSmallIntInf = control {name = "globalize int-inf as small type)",
                                    default = true,
                                    toString = Bool.toString}

val globalizeSmallType = control {name = "globalize small type",
                                  default = 1,
                                  toString = Int.toString}

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


val limitCheckExpect = control {name = "limit check expect",
                                default = NONE,
                                toString = Option.toString Bool.toString}

structure LLVMAliasAnalysisMetaData =
   struct
      datatype t =
         None
       | Scope
       | TBAA of {gcstate: {offset: bool} option,
                  global: {cty: bool, index: bool} option,
                  heap: {cty: bool, kind: bool, offset: bool, tycon: bool} option,
                  other: bool,
                  stack: {offset: bool} option}

      val tbaaDefault =
         TBAA {gcstate = SOME {offset = false},
               global = SOME {cty = false, index = false},
               heap = SOME {cty = false, kind = false, offset = false, tycon = false},
               other = true,
               stack = SOME {offset = false}}

      fun toString aamd =
         case aamd of
            None => "none"
          | Scope => "scope"
          | TBAA {gcstate, global, heap, other, stack} =>
               let
                  open Layout
               in
                  toString
                  (namedRecord
                   ("tbaa",
                    [("gcstate",
                      Option.layout (fn {offset} =>
                                     record [("offset", Bool.layout offset)])
                      gcstate),
                     ("global",
                      Option.layout (fn {cty, index} =>
                                     record [("cty", Bool.layout cty),
                                             ("index", Bool.layout index)])
                      global),
                     ("heap",
                      Option.layout (fn {cty, kind, offset, tycon} =>
                                     record [("cty", Bool.layout cty),
                                             ("kind", Bool.layout kind),
                                             ("offset", Bool.layout offset),
                                             ("tycon", Bool.layout tycon)])
                      heap),
                     ("other", Bool.layout other),
                     ("stack",
                      Option.layout (fn {offset} =>
                                     record [("offset", Bool.layout offset)])
                      stack)]))
               end
      fun fromString s =
         let
            open Parse
            infix 1 <|> >>=
            infix  3 <*> <* *>
            infixr 4 <$> <$$> <$$$> <$$$$> <$ <$?>
            val p =
               any
               [kw "none" *> pure None,
                kw "scope" *> pure Scope,
                kw "tbaa" *>
                (cbrack (ffield ("gcstate", option (cbrack (ffield ("offset", bool) >>= (fn offset =>
                                                            pure {offset = offset})))) >>= (fn gcstate =>
                         nfield ("global", option (cbrack (ffield ("cty", bool) >>= (fn cty =>
                                                           nfield ("index", bool) >>= (fn index =>
                                                           pure {cty = cty,
                                                                 index = index}))))) >>= (fn global =>
                         nfield ("heap", option (cbrack (ffield ("cty", bool) >>= (fn cty =>
                                                         nfield ("kind", bool) >>= (fn kind =>
                                                         nfield ("offset", bool) >>= (fn offset =>
                                                         nfield ("tycon", bool) >>= (fn tycon =>
                                                         pure {cty = cty,
                                                               kind = kind,
                                                               offset = offset,
                                                               tycon = tycon}))))))) >>= (fn heap =>
                         nfield ("other", bool) >>= (fn other =>
                         nfield ("stack", option (cbrack (ffield ("offset", bool) >>= (fn offset =>
                                                          pure {offset = offset})))) >>= (fn stack =>
                         pure (TBAA {gcstate = gcstate, global = global, heap = heap, other = other, stack = stack})))))))
                <|>
                pure tbaaDefault)]
               <* failing next
         in
            case parseString (p, s) of
               No _ => NONE
             | Yes c => SOME c
         end
   end

val llvmAAMD =
      control {name = "llvmAAMD",
               default = LLVMAliasAnalysisMetaData.None,
               toString = LLVMAliasAnalysisMetaData.toString}

val llvmCC10 = control {name = "llvm 'cc10'",
                        default = false,
                        toString = Bool.toString}

val loopUnrollLimit = control {name = "loop unrolling limit",
                                default = 150,
                                toString = Int.toString}
val loopUnswitchLimit = control {name = "loop unswitching limit",
                                  default = 300,
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
      val elimALRedundant = control {name = "elim AL redundant",
                                     default = true,
                                     toString = Bool.toString}

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

      val pic = control {name = "native pic",
                         default = false,
                         toString = Bool.toString}
   end

val numExports: int ref = ref 0

val optFuel =
   control {name = "optFuel",
            default = NONE,
            toString = Option.toString Int.toString}

fun optFuelAvailAndUse () =
   case !optFuel of
      NONE => true
    | SOME i => if i > 0
                   then (optFuel := SOME (i - 1); true)
                   else false
(* Suppress unused variable warning
 * This variable is purposefully unused in production,
 * but is retained to make it easy to use in development of new
 * optimization passes.
 *)
val _ = optFuelAvailAndUse

(* Control IL-specific optimization passes *)
structure OptimizationPasses =
   struct
      val optPasses: {il: string, passes: string} list ref =
         control {name = "optimizationPasses",
                  default = [],
                  toString = List.toString (fn {il, passes} =>
                                            concat ["<",il,"::",passes,">"])}
      val optPassesSets: {il: string, set: string -> unit Result.t} list ref =
         ref []

      fun register {il, set} =
         let
            val set = fn passes =>
               (optPasses := List.map (!optPasses, fn z =>
                                       if String.equals (il, #il z)
                                          then {il = il, passes = passes}
                                          else z)
                ; set passes)
         in
            List.push (optPassesSets, {il = il, set = set})
         end

      fun set {il, passes} =
         case List.peek (!optPassesSets, fn z => String.equals (il, #il z)) of
            NONE => Error.bug (concat ["Control.OptimizationPasses.set: ", il, " not found"])
          | SOME {set, ...} => set passes
      fun setAll passes =
         List.foldr (!optPassesSets, Result.Yes (), fn ({il, set}, res) =>
                     case res of
                        Result.No s => Result.No s
                      | Result.Yes () => (case set passes of
                                             Result.No s => Result.No (concat [s, " (for ", il, ")"])
                                           | Result.Yes () => Result.Yes ()))
   end

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

structure PositionIndependentStyle =
   struct
      datatype t =
         NPI
       | PIC
       | PIE

      fun fromString s =
         case s of
            "npi" => SOME NPI
          | "pic" => SOME PIC
          | "pie" => SOME PIE
          | _ => NONE
      fun toString pis =
         case pis of
            NPI => "npi"
          | PIC => "pic"
          | PIE => "pie"
      fun toSuffix pis =
         case pis of
            NONE => ""
          | SOME NPI => "-npi"
          | SOME PIC => "-pic"
          | SOME PIE => "-pie"

      fun ccOpts pis =
         case pis of
            NONE => []
          | SOME NPI => ["-fno-pic", "-fno-pie"]
          | SOME PIC => ["-fPIC"]
          | SOME PIE => ["-fPIE"]
      fun llvm_llcOpts (pis, {targetDefault}) =
         let
            fun llcOpts pis =
               case pis of
                  NPI => [] (* ["--relocation-model=static"] *)
                | PIC => ["--relocation-model=pic"]
                | PIE => ["--relocation-model=pic"]
         in
            case pis of
               NONE => llcOpts targetDefault
             | SOME pis => llcOpts pis
         end
      fun linkOpts pis =
         case pis of
            NONE => []
          | SOME NPI => ["-fno-pic", "-fno-pie", "-no-pie"]
          | SOME PIC => ["-fno-pie", "-no-pie"]
          | SOME PIE => ["-fPIE", "-pie"]
   end

val positionIndependentStyle = control {name = "position independent style",
                                        default = NONE,
                                        toString = Option.toString PositionIndependentStyle.toString}


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
       | ProfileTime

      val toString =
         fn ProfileNone => "None"
          | ProfileAlloc => "Alloc"
          | ProfileCallStack => "CallStack"
          | ProfileCount => "Count"
          | ProfileDrop => "Drop"
          | ProfileTime => "Time"
   end

datatype profile = datatype Profile.t

val profile = control {name = "profile",
                       default = ProfileNone,
                       toString = Profile.toString}

val profileBlock = control {name = "profile block",
                            default = false,
                            toString = Bool.toString}

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

val showBasisCompact = control {name = "show basis compact",
                                default = false,
                                toString = Bool.toString}
val showBasisDef = control {name = "show basis def",
                            default = true,
                            toString = Bool.toString}
val showBasisFlat = control {name = "show basis flat",
                             default = true,
                             toString = Bool.toString}

val showDefUse = control {name = "show def-use",
                          default = NONE,
                          toString = Option.toString File.toString}

val showTypes = control {name = "show types",
                         default = true,
                         toString = Bool.toString}

structure SignalCheck =
   struct
      datatype t =
         Always
       | IfHandlesSignals
      val toString =
         fn Always => "always"
          | IfHandlesSignals => "if-handles-signals"
   end
val signalCheck = control {name = "signal check",
                           default = SignalCheck.IfHandlesSignals,
                           toString = SignalCheck.toString}
val signalCheckAtLimitCheck = control {name = "signal check at limit check",
                                       default = true,
                                       toString = Bool.toString}
val signalCheckExpect = control {name = "signal check expect",
                                 default = NONE,
                                 toString = Option.toString Bool.toString}

structure SplitTypesBool =
   struct
      datatype t =
         Always
       | Never
       | Smart (* split only when smaller than two, default *)
      val toString =
         fn Always => "always"
          | Never => "never"
          | Smart => "smart"
   end

datatype splitTypesBool = datatype SplitTypesBool.t

val splitTypesBool = control {name = "bool type splitting method",
                              default = Smart,
                              toString = SplitTypesBool.toString}

val stackCheckExpect = control {name = "stack check expect",
                                default = NONE,
                                toString = Option.toString Bool.toString}

val stopPasses = control {name = "stop passes",
                          default = [],
                          toString = List.toString
                                     (Layout.toString o
                                      Regexp.Compiled.layout)}
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

      val consts =
         Promise.delay
         (fn () =>
          StrMap.load
          (OS.Path.joinDirFile {dir = !libTargetDir,
                                file = "constants"}))

      datatype arch = datatype MLton.Platform.Arch.t
         
      val arch = control {name = "target arch",
                          default = X86,
                          toString = MLton.Platform.Arch.toString}

      datatype os = datatype MLton.Platform.OS.t

      val os = control {name = "target OS",
                        default = Linux,
                        toString = MLton.Platform.OS.toString}

      val bigEndian =
         Promise.lazy
         (fn () =>
          StrMap.lookupBool (Promise.force consts, "const::MLton_Platform_Arch_bigendian"))

      structure Size =
         struct
            fun make name =
               Promise.lazy
               (fn () =>
                (Bytes.toBits o Bytes.fromIntInf)
                (StrMap.lookupIntInf (Promise.force consts, "size::" ^ name)))

            val cint = make "cint"
            val cpointer = make "cpointer"
            val cptrdiff = make "cptrdiff"
            val csigatomic = make "csigatomic"
            val csize = make "csize"
            val header = make "header"
            val mplimb = make "mplimb"
            val normalMetaData = make "normalMetaData"
            val objptr = make "objptr"
            val seqIndex = make "seqIndex"
            val sequenceMetaData = make "sequenceMetaData"
         end
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
             path = "rep" ^ Bits.toString (Target.Size.objptr ())},
            {var = "SEQUENCE_METADATA_SIZE",
             path = "size" ^ Bits.toString (Target.Size.sequenceMetaData ())},
            {var = "NORMAL_METADATA_SIZE",
             path = "size" ^ Bits.toString (Target.Size.normalMetaData ())},
            {var = "SEQINDEX_INT",
             path = "int" ^ Bits.toString (Target.Size.seqIndex ())},
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

      fun compare (v1, v2) =
         case (v1, v2) of
            (Silent, Silent) => EQUAL
          | (Silent, _) => LESS
          | (_, Silent) => GREATER
          | (Top, Top) => EQUAL
          | (Top, _) => LESS
          | (_, Top) => GREATER
          | (Pass, Pass) => EQUAL
          | (Pass, _) => LESS
          | (_, Pass) => GREATER
          | (Detail, Detail) => EQUAL

      val {<, <=, ...} =
         Relation.compare compare
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


val commandLineConsts = StrMap.new ()
fun setCommandLineConst {name, value} =
   let
      fun make (fromString, control) =
         let
            fun set () =
               case fromString value of
                  NONE => Error.bug (concat ["bad value for ", name])
                | SOME v => control := v
         in
            set
         end
      val () =
         case List.peek ([("Exn.keepHistory",
                           make (Bool.fromString, exnHistory))],
                         fn (s, _) => s = name) of
            NONE => ()
          | SOME (_,set) => set ()
   in
      StrMap.add (commandLineConsts, "cmdLineConst::" ^ name, value)
   end

val buildConsts =
   Promise.delay
   (fn () =>
    let
       val bool = Bool.toString
       val int = Int.toString
       val buildConsts =
          [("MLton_Align_align", int (case !align of
                                         Align4 => 4
                                       | Align8 => 8)),
           ("MLton_Codegen_codegen", int (case !codegen of
                                             CCodegen => 0
                                           | X86Codegen => 1
                                           | AMD64Codegen => 2
                                           | LLVMCodegen => 3)),
           ("MLton_FFI_numExports", int (!numExports)),
           ("MLton_Platform_Format", (case !format of
                                         Archive => "archive"
                                       | Executable => "executable"
                                       | LibArchive => "libarchive"
                                       | Library => "library")),
           ("MLton_Profile_isOn", bool (case !profile of
                                           ProfileNone => false
                                         | ProfileCallStack => false
                                         | ProfileDrop => false
                                         | _ => true))]
       val table = StrMap.new ()
       val () = List.foreach (buildConsts, fn (name, value) =>
                              StrMap.add (table, "buildConst::" ^ name, value))
    in
       table
    end)

end
