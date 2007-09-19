(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Compile (S: COMPILE_STRUCTS): COMPILE =
struct

open S

(*---------------------------------------------------*)
(*              Intermediate Languages               *)
(*---------------------------------------------------*)

structure Symbol = Symbol ()
structure Field = Field (structure Symbol = Symbol)
structure Record = Record (val isSorted = false
                           structure Field = Field)
structure SortedRecord = Record (val isSorted = true
                                 structure Field = Field)
structure Tyvar = Tyvar ()
structure Ast = Ast (structure Record = Record
                     structure SortedRecord = SortedRecord
                     structure Symbol = Symbol
                     structure Tyvar = Tyvar)
local
   open Ast.Tycon
in
   structure CharSize = CharSize
   structure IntSize = IntSize
   structure RealSize = RealSize
   structure WordSize = WordSize
end
structure Atoms = Atoms (structure CharSize = CharSize
                         structure Field = Field
                         structure IntSize = IntSize
                         structure RealSize = RealSize
                         structure Record = Record
                         structure SortedRecord = SortedRecord
                         structure Tyvar = Tyvar
                         structure WordSize = WordSize)
local
   open Atoms
in
   structure Const = Const
   structure ConstType = Const.ConstType
   structure Ffi = Ffi
   structure WordX = WordX
end
structure TypeEnv = TypeEnv (Atoms)
structure CoreML = CoreML (open Atoms
                           structure Type =
                              struct
                                 open TypeEnv.Type

                                 val makeHom =
                                    fn {con, var} =>
                                    makeHom {con = con,
                                             expandOpaque = true,
                                             var = var}

                                 fun layout t = 
                                    layoutPrettyAux 
                                    (t, {expandOpaque = true,
                                         localTyvarNames = false})
                              end)
structure Xml = Xml (open Atoms)
structure Sxml = Sxml (open Xml)
structure Ssa = Ssa (open Atoms)
structure Ssa2 = Ssa2 (open Atoms)
structure Machine = Machine (open Atoms
                             structure Label = Ssa.Label)
local
   open Machine
in
   structure Runtime = Runtime
end

(*---------------------------------------------------*)
(*                  Compiler Passes                  *)
(*---------------------------------------------------*)

structure FrontEnd = FrontEnd (structure Ast = Ast)
structure MLBFrontEnd = MLBFrontEnd (structure Ast = Ast
                                     structure FrontEnd = FrontEnd)
structure DeadCode = DeadCode (structure CoreML = CoreML)
structure Defunctorize = Defunctorize (structure CoreML = CoreML
                                       structure Xml = Xml)
structure Elaborate = Elaborate (structure Ast = Ast
                                 structure CoreML = CoreML
                                 structure TypeEnv = TypeEnv)
local
   open Elaborate
in
   structure Env = Env
end
structure LookupConstant = LookupConstant (structure Const = Const
                                           structure ConstType = ConstType
                                           structure Ffi = Ffi)
structure Monomorphise = Monomorphise (structure Xml = Xml
                                       structure Sxml = Sxml)
structure ClosureConvert = ClosureConvert (structure Ssa = Ssa
                                           structure Sxml = Sxml)
structure SsaToSsa2 = SsaToSsa2 (structure Ssa = Ssa
                                 structure Ssa2 = Ssa2)
structure Backend = Backend (structure Ssa = Ssa2
                             structure Machine = Machine
                             fun funcToLabel f = f)
structure CCodegen = CCodegen (structure Ffi = Ffi
                               structure Machine = Machine)
structure Bytecode = Bytecode (structure CCodegen = CCodegen
                               structure Machine = Machine)
structure x86Codegen = x86Codegen (structure CCodegen = CCodegen
                                   structure Machine = Machine)
structure amd64Codegen = amd64Codegen (structure CCodegen = CCodegen
                                       structure Machine = Machine)


(* ------------------------------------------------- *)
(*                 Lookup Constant                   *)
(* ------------------------------------------------- *)

val commandLineConstants: {name: string, value: string} list ref = ref []
fun setCommandLineConstant (c as {name, value}) =
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
                           make (Bool.fromString, Control.exnHistory))],
                         fn (s, _) => s = name) of
            NONE => ()
          | SOME (_,set) => set ()
   in
      List.push (commandLineConstants, c)
   end

val allConstants: (string * ConstType.t) list ref = ref []
val amBuildingConstants: bool ref = ref false

val lookupConstant =
   let
      val zero = Const.word (WordX.fromIntInf (0, WordSize.word32))
      val f =
         Promise.lazy
         (fn () =>
          if !amBuildingConstants
             then (fn ({name, default, ...}, t) =>
                   let
                      (* Don't keep constants that already have a default value.
                       * These are defined by _command_line_const and set by
                       * -const, and shouldn't be looked up.
                       *)
                      val () =
                         if isSome default
                            then ()
                         else List.push (allConstants, (name, t))
                   in
                      zero
                   end)
          else
             File.withIn
             (concat [!Control.libTargetDir, "/constants"], fn ins =>
              LookupConstant.load (ins, !commandLineConstants)))
   in
      fn z => f () z
   end

(* ------------------------------------------------- *)   
(*                   Primitive Env                   *)
(* ------------------------------------------------- *)

local
   structure Con = TypeEnv.Con
   structure Tycon = TypeEnv.Tycon
   structure Type = TypeEnv.Type
   structure Tyvar = TypeEnv.Tyvar

   val primitiveDatatypes =
      Vector.new3
      ({tycon = Tycon.bool,
        tyvars = Vector.new0 (),
        cons = Vector.new2 ({con = Con.falsee, arg = NONE},
                            {con = Con.truee, arg = NONE})},
       let
          val a = Tyvar.newNoname {equality = false}
       in
          {tycon = Tycon.list,
           tyvars = Vector.new1 a,
           cons = Vector.new2 ({con = Con.nill, arg = NONE},
                               {con = Con.cons,
                                arg = SOME (Type.tuple
                                            (Vector.new2
                                             (Type.var a,
                                              Type.list (Type.var a))))})}
       end,
       let
          val a = Tyvar.newNoname {equality = false}
       in
          {tycon = Tycon.reff,
           tyvars = Vector.new1 a,
           cons = Vector.new1 {con = Con.reff, arg = SOME (Type.var a)}}
       end)

   val primitiveExcons =
      let
         open CoreML.Con
      in
         [bind, match, overflow]
      end

   structure Con =
      struct
         open Con

         fun toAst c =
            Ast.Con.fromSymbol (Symbol.fromString (Con.toString c),
                                Region.bogus)
      end

   structure Env =
      struct
         open Env 

         structure Tycon =
            struct
               open Tycon

               fun toAst c =
                  Ast.Tycon.fromSymbol (Symbol.fromString (Tycon.toString c),
                                        Region.bogus)
            end
         structure Type = TypeEnv.Type
         structure Scheme = TypeEnv.Scheme

         fun addPrim (E: t): unit =
            let
               val _ =
                  List.foreach
                  (Tycon.prims, fn {kind, name, tycon, ...} =>
                   extendTycon
                   (E, Ast.Tycon.fromSymbol (Symbol.fromString name,
                                             Region.bogus),
                    TypeStr.tycon (tycon, kind),
                    {forceUsed = false, isRebind = false}))
               val _ =
                  Vector.foreach
                  (primitiveDatatypes, fn {tyvars, tycon, cons} =>
                   let
                      val cons =
                         Env.newCons
                         (E, Vector.map (cons, fn {con, ...} =>
                                         {con = con, name = Con.toAst con}))
                         (Vector.map
                          (cons, fn {arg, ...} =>
                           let
                              val resultType =
                                 Type.con (tycon, Vector.map (tyvars, Type.var))
                           in
                              Scheme.make
                              {canGeneralize = true,
                               ty = (case arg of
                                        NONE => resultType
                                      | SOME t => Type.arrow (t, resultType)),
                               tyvars = tyvars}
                           end))
                   in
                      extendTycon
                      (E, Tycon.toAst tycon,
                       TypeStr.data (tycon,
                                     TypeStr.Kind.Arity (Vector.length tyvars),
                                     cons),
                       {forceUsed = false, isRebind = false})
                   end)
               val _ =
                  extendTycon (E,
                               Ast.Tycon.fromSymbol (Symbol.unit, Region.bogus),
                               TypeStr.def (Scheme.fromType Type.unit,
                                            TypeStr.Kind.Arity 0),
                               {forceUsed = false, isRebind = false})
               val scheme = Scheme.fromType Type.exn
               val _ = List.foreach (primitiveExcons, fn c =>
                                     extendExn (E, Con.toAst c, c, SOME scheme))
            in
               ()
            end
      end

   val primitiveDecs: CoreML.Dec.t list =
      let
         open CoreML.Dec
      in
         List.concat [[Datatype primitiveDatatypes],
                      List.map
                      (primitiveExcons, fn c =>
                       Exception {con = c, arg = NONE})]
      end

in

   fun addPrim E =
      (Env.addPrim E
       ; primitiveDecs)
end


(* ------------------------------------------------- *)
(*                 parseAndElaborateMLB              *)
(* ------------------------------------------------- *)

fun quoteFile s = concat ["\"", String.escapeSML s, "\""]

structure MLBString:>
   sig
      type t

      val fromFile: File.t -> t
      val fromString: string -> t
      val lexAndParseMLB: t -> Ast.Basdec.t
   end =
   struct
      type t = string

      val fromFile = quoteFile

      val fromString = fn s => s

      val lexAndParseMLB = MLBFrontEnd.lexAndParseString
   end

val lexAndParseMLB = MLBString.lexAndParseMLB

val lexAndParseMLB: MLBString.t -> Ast.Basdec.t = 
   fn input =>
   let
      val ast = lexAndParseMLB input
      val _ = Control.checkForErrors "parse"
   in
      ast
   end

fun sourceFilesMLB {input} =
   Ast.Basdec.sourceFiles (lexAndParseMLB (MLBString.fromFile input))

val elaborateMLB = Elaborate.elaborateMLB

val displayEnvDecs =
   Control.Layout
   (fn (_, ds) => 
    Vector.layout
    (fn (d, b) =>
     Layout.record
     [("deadCode", Bool.layout b),
      ("decs", List.layout CoreML.Dec.layout d)])
    ds)

fun parseAndElaborateMLB (input: MLBString.t)
   : Env.t * (CoreML.Dec.t list * bool) vector =
   Control.pass
   {name = "parseAndElaborate",
    suffix = "core-ml",
    style = Control.ML,
    thunk = (fn () =>
             (Const.lookup := lookupConstant
              ; elaborateMLB (lexAndParseMLB input, {addPrim = addPrim}))),
    display = displayEnvDecs}

(* ------------------------------------------------- *)
(*                   Basis Library                   *)
(* ------------------------------------------------- *)

fun outputBasisConstants (out: Out.t): unit =
   let
      val _ = amBuildingConstants := true
      val (_, decs) =
         parseAndElaborateMLB (MLBString.fromFile "$(SML_LIB)/basis/primitive/primitive.mlb")
      val decs = Vector.concatV (Vector.map (decs, Vector.fromList o #1))
      (* Need to defunctorize so the constants are forced. *)
      val _ = Defunctorize.defunctorize (CoreML.Program.T {decs = decs})
      val _ = LookupConstant.build (!allConstants, out)
   in
      ()
   end

(* ------------------------------------------------- *)
(*                      compile                      *)
(* ------------------------------------------------- *)

exception Done

fun elaborate {input: MLBString.t}: Xml.Program.t =
   let
      val (E, decs) = parseAndElaborateMLB input
      val _ =
         case !Control.showBasis of
            NONE => ()
          | SOME f =>
               File.withOut
               (f, fn out =>
                Layout.outputl (Env.layoutCurrentScope E, out))
      val _ = Env.processDefUse E
      val _ =
         case !Control.exportHeader of
            NONE => ()
          | SOME f => 
               File.withOut
               (f, fn out =>
                let
                   val _ =
                      File.outputContents
                      (concat [!Control.libDir, "/include/ml-types.h"], out)
                   fun print s = Out.output (out, s)
                   val _ = print "\n"
                   val _ = print "typedef void* CPointer;\n"
                   val _ = print "typedef Pointer Objptr;\n"
                   val _ = print "\n"
                   val _ = Ffi.declareHeaders {print = print}
                in
                   ()
                end)
      val _ = if !Control.elaborateOnly then raise Done else ()
      val decs =
         Control.pass
         {name = "deadCode",
          suffix = "core-ml",
          style = Control.ML,
          thunk = fn () => let
                              val {prog = decs} =
                                 DeadCode.deadCode {prog = decs}
                           in
                              decs
                           end,
          display = Control.Layout (Vector.layout (List.layout CoreML.Dec.layout))}
      val decs = Vector.concatV (Vector.map (decs, Vector.fromList))
      val coreML = CoreML.Program.T {decs = decs}
(*
      val _ = Control.message (Control.Detail, fn () =>
                               CoreML.Program.layoutStats coreML)
*)
      (* Set GC_state offsets and sizes. *)
      val _ =
         let
            fun get (name: string): Bytes.t =
               case lookupConstant ({default = NONE, name = name},
                                    ConstType.Word WordSize.word32) of
                  Const.Word w => Bytes.fromInt (WordX.toInt w)
                | _ => Error.bug "Compile.elaborate: GC_state offset must be an int"
         in
            Runtime.GCField.setOffsets
            {
             atomicState = get "atomicState_Offset",
             cardMapAbsolute = get "generationalMaps.cardMapAbsolute_Offset",
             currentThread = get "currentThread_Offset",
             curSourceSeqsIndex = get "sourceMaps.curSourceSeqsIndex_Offset",
             exnStack = get "exnStack_Offset",
             frontier = get "frontier_Offset",
             limit = get "limit_Offset",
             limitPlusSlop = get "limitPlusSlop_Offset",
             maxFrameSize = get "maxFrameSize_Offset",
             signalIsPending = get "signalsInfo.signalIsPending_Offset",
             stackBottom = get "stackBottom_Offset",
             stackLimit = get "stackLimit_Offset",
             stackTop = get "stackTop_Offset"
             };
            Runtime.GCField.setSizes
            {
             atomicState = get "atomicState_Size",
             cardMapAbsolute = get "generationalMaps.cardMapAbsolute_Size",
             currentThread = get "currentThread_Size",
             curSourceSeqsIndex = get "sourceMaps.curSourceSeqsIndex_Size",
             exnStack = get "exnStack_Size",
             frontier = get "frontier_Size",
             limit = get "limit_Size",
             limitPlusSlop = get "limitPlusSlop_Size",
             maxFrameSize = get "maxFrameSize_Size",
             signalIsPending = get "signalsInfo.signalIsPending_Size",
             stackBottom = get "stackBottom_Size",
             stackLimit = get "stackLimit_Size",
             stackTop = get "stackTop_Size"
             }
         end
      (* Setup endianness *)
      val _ =
         let
            fun get (name:string): bool =
                case lookupConstant ({default = NONE, name = name},
                                     ConstType.Bool) of
                   Const.Word w => 1 = WordX.toInt w
                 | _ => Error.bug "Compile.elaborate: endian unknown"
         in
            Control.Target.setBigEndian (get "MLton_Platform_Arch_bigendian")
         end
      val xml =
         Control.passTypeCheck
         {name = "defunctorize",
          suffix = "xml",
          style = Control.ML,
          thunk = fn () => Defunctorize.defunctorize coreML,
          display = Control.Layout Xml.Program.layout,
          typeCheck = Xml.typeCheck}
   in
      xml
   end

fun preCodegen {input: MLBString.t}: Machine.Program.t =
   let
      val xml = elaborate {input = input}
      val _ = Control.message (Control.Detail, fn () =>
                               Xml.Program.layoutStats xml)
      val xml =
          Control.passTypeCheck
          {name = "xmlSimplify",
           suffix = "xml",
           style = Control.ML,
           thunk = fn () => Xml.simplify xml,
           display = Control.Layout Xml.Program.layout,
           typeCheck = Xml.typeCheck}
      val _ = Control.message (Control.Detail, fn () =>
                               Xml.Program.layoutStats xml)
      val sxml =
         Control.passTypeCheck
         {name = "monomorphise",
          suffix = "sxml",
          style = Control.ML,
          thunk = fn () => Monomorphise.monomorphise xml,
          display = Control.Layout Sxml.Program.layout,
          typeCheck = Sxml.typeCheck}
      val _ = Control.message (Control.Detail, fn () =>
                               Sxml.Program.layoutStats sxml)
      val sxml =
         Control.passTypeCheck
         {name = "sxmlSimplify",
          suffix = "sxml",
          style = Control.ML,
          thunk = fn () => Sxml.simplify sxml,
          display = Control.Layout Sxml.Program.layout,
          typeCheck = Sxml.typeCheck}
      val _ = Control.message (Control.Detail, fn () =>
                               Sxml.Program.layoutStats sxml)
      val ssa =
         Control.passTypeCheck
         {name = "closureConvert",
          suffix = "ssa",
          style = Control.No,
          thunk = fn () => ClosureConvert.closureConvert sxml,
          typeCheck = Ssa.typeCheck,
          display = Control.Layouts Ssa.Program.layouts}
      val ssa =
         Control.passTypeCheck
         {name = "ssaSimplify",
          suffix = "ssa",
          style = Control.No,
          thunk = fn () => Ssa.simplify ssa,
          typeCheck = Ssa.typeCheck,
          display = Control.Layouts Ssa.Program.layouts}
      val _ =
         let
            open Control
         in
            if !keepSSA
               then saveToFile ({suffix = "ssa"}, No, ssa,
                                 Layouts Ssa.Program.layouts)
            else ()
         end
      val ssa2 =
         Control.passTypeCheck
         {name = "toSsa2",
          suffix = "ssa2",
          style = Control.No,
          thunk = fn () => SsaToSsa2.convert ssa,
          typeCheck = Ssa2.typeCheck,
          display = Control.Layouts Ssa2.Program.layouts}
      val ssa2 =
         Control.passTypeCheck
         {name = "ssa2Simplify",
          suffix = "ssa2",
          style = Control.No,
          thunk = fn () => Ssa2.simplify ssa2,
          typeCheck = Ssa2.typeCheck,
          display = Control.Layouts Ssa2.Program.layouts}
      val _ =
         let
            open Control
         in
            if !keepSSA2
               then saveToFile ({suffix = "ssa2"}, No, ssa2,
                                 Layouts Ssa2.Program.layouts)
            else ()
         end
      val codegenImplementsPrim =
         case !Control.codegen of
            Control.Bytecode => Bytecode.implementsPrim
          | Control.CCodegen => CCodegen.implementsPrim
          | Control.x86Codegen => x86Codegen.implementsPrim
          | Control.amd64Codegen => amd64Codegen.implementsPrim
      val machine =
         Control.pass
         {name = "backend",
          suffix = "machine",
          style = Control.No,
          thunk = fn () => (Backend.toMachine
                            (ssa2,
                             {codegenImplementsPrim = codegenImplementsPrim})),
          display = Control.Layouts Machine.Program.layouts}
      val _ =
         let
            open Control
         in
            if !keepMachine
               then saveToFile ({suffix = "machine"}, No, machine,
                                 Layouts Machine.Program.layouts)
            else ()
         end
      val _ =
         (*
          * For now, machine type check is too slow to run.
          *)
         if !Control.typeCheck
            then
               Control.trace (Control.Pass, "machine type check")
               Machine.Program.typeCheck machine
         else ()
   in
      machine
   end

fun compile {input: MLBString.t, outputC, outputS}: unit =
   let
      val machine =
         Control.trace (Control.Top, "pre codegen")
         preCodegen {input = input}
      fun clearNames () =
         (Machine.Program.clearLabelNames machine
          ; Machine.Label.printNameAlphaNumeric := true)
      val () =
         case !Control.codegen of
            Control.Bytecode =>
               Control.trace (Control.Top, "bytecode gen")
               Bytecode.output {program = machine,
                                outputC = outputC}
          | Control.CCodegen =>
               (clearNames ()
                ; (Control.trace (Control.Top, "C code gen")
                   CCodegen.output {program = machine,
                                    outputC = outputC}))
          | Control.x86Codegen =>
               (clearNames ()
                ; (Control.trace (Control.Top, "x86 code gen")
                   x86Codegen.output {program = machine,
                                      outputC = outputC,
                                      outputS = outputS}))
          | Control.amd64Codegen =>
               (clearNames ()
                ; (Control.trace (Control.Top, "amd64 code gen")
                   amd64Codegen.output {program = machine,
                                        outputC = outputC,
                                        outputS = outputS}))
      val _ = Control.message (Control.Detail, PropertyList.stats)
      val _ = Control.message (Control.Detail, HashSet.stats)
   in
      ()
   end handle Done => ()

fun compileMLB {input: File.t, outputC, outputS}: unit =
   compile {input = MLBString.fromFile input,
            outputC = outputC,
            outputS = outputS}

val elaborateMLB =
   fn {input: File.t} =>
   (ignore (elaborate {input = MLBString.fromFile input}))
   handle Done => ()

local
   fun genMLB {input: File.t list}: MLBString.t =
      let
         val basis = "$(SML_LIB)/basis/default.mlb"
      in
         MLBString.fromString
         (case input of
             [] => basis
           | _ =>
                let
                   val input = List.map (input, quoteFile)
                in
                   String.concat
                   ["local\n",
                    basis, "\n",
                    "in\n",
                    String.concat (List.separate (input, "\n")), "\n",
                    "end\n"]
                end)
      end
in
   fun compileSML {input: File.t list, outputC, outputS}: unit =
      compile {input = genMLB {input = input},
               outputC = outputC,
               outputS = outputS}
   val elaborateSML =
      fn {input: File.t list} =>
      (ignore (elaborate {input = genMLB {input = input}}))
      handle Done => ()
end

end
