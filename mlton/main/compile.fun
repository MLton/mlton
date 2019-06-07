(* Copyright (C) 2011,2014-2015,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Compile (S: COMPILE_STRUCTS): COMPILE =
struct

open S

(*---------------------------------------------------*)
(*              Intermediate Languages               *)
(*---------------------------------------------------*)

structure Atoms = Atoms ()
local
   open Atoms
in
   structure Const = Const
   structure ConstType = Const.ConstType
   structure Ffi = Ffi
   structure Symbol = Symbol
   structure WordSize = WordSize
   structure WordX = WordX
end
structure Ast = Ast (open Atoms)
structure TypeEnv = TypeEnv (open Atoms)
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
                                    #1 (layoutPretty
                                        (t, {expandOpaque = true,
                                             layoutPrettyTycon = Tycon.layout,
                                             layoutPrettyTyvar = Tyvar.layout}))
                              end)
structure Xml = Xml (open Atoms)
structure Sxml = Sxml (open Xml)
structure Ssa = Ssa (open Atoms)
structure Ssa2 = Ssa2 (open Atoms)
structure Machine = Machine (open Atoms)

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
structure CCodegen = CCodegen (structure Machine = Machine)
structure LLVMCodegen = LLVMCodegen (structure CCodegen = CCodegen
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
   structure Tyvar =
      struct
         open TypeEnv.Tyvar
         open TypeEnv.TyvarExt
      end

   val primitiveDatatypes =
      Vector.new3
      ({tycon = Tycon.bool,
        tyvars = Vector.new0 (),
        cons = Vector.new2 ({con = Con.falsee, arg = NONE},
                            {con = Con.truee, arg = NONE})},
       let
          val a = Tyvar.makeNoname {equality = false}
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
          val a = Tyvar.makeNoname {equality = false}
       in
          {tycon = Tycon.reff,
           tyvars = Vector.new1 a,
           cons = Vector.new1 {con = Con.reff, arg = SOME (Type.var a)}}
       end)

   val primitiveExcons =
      let
         open CoreML.Con
      in
         [bind, match]
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
                  (Tycon.prims, fn {name, tycon, ...} =>
                   if List.contains ([Tycon.arrow, Tycon.tuple], tycon, Tycon.equals)
                      then ()
                      else extendTycon
                           (E, Ast.Tycon.fromSymbol (Symbol.fromString name,
                                                     Region.bogus),
                            TypeStr.tycon tycon,
                            {forceUsed = false, isRebind = false}))
               val _ =
                  Vector.foreach
                  (primitiveDatatypes, fn {tyvars, tycon, cons} =>
                   let
                      val cons =
                         Vector.map
                         (cons, fn {con, arg} =>
                          let
                             val res =
                                Type.con (tycon, Vector.map (tyvars, Type.var))
                             val ty =
                                case arg of
                                   NONE => res
                                 | SOME arg => Type.arrow (arg, res)
                             val scheme =
                                Scheme.make
                                {canGeneralize = true,
                                 ty = ty,
                                 tyvars = tyvars}
                          in
                             {con = con,
                              name = Con.toAst con,
                              scheme = scheme}
                          end)
                      val cons = Env.newCons (E, cons)
                   in
                      extendTycon
                      (E, Tycon.toAst tycon,
                       TypeStr.data (tycon, cons),
                       {forceUsed = false, isRebind = false})
                   end)
               val _ =
                  extendTycon (E,
                               Ast.Tycon.fromSymbol (Symbol.unit, Region.bogus),
                               TypeStr.def (Scheme.fromType Type.unit),
                               {forceUsed = false, isRebind = false})
               val scheme = Scheme.fromType Type.exn
               val _ = List.foreach (primitiveExcons, fn c =>
                                     extendExn (E, Con.toAst c, c, scheme))
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
      val toFile: {display: t Control.display, style: Control.style, suffix: string}
   end =
   struct
      type t = string

      val fromFile = quoteFile

      val fromString = fn s => s

      val lexAndParseMLB = MLBFrontEnd.lexAndParseString

      val toFile =
         {display = Control.NoDisplay,
          style = Control.No,
          suffix = "mlb"}
   end

val lexAndParseMLB: MLBString.t -> Ast.Basdec.t = 
   fn input =>
   let
      val ast = MLBString.lexAndParseMLB input
      val _ = Control.checkForErrors ()
   in
      ast
   end

fun sourceFilesMLB {input} =
   Ast.Basdec.sourceFiles (lexAndParseMLB (MLBString.fromFile input))

fun parseAndElaborateMLB (input: MLBString.t): (CoreML.Dec.t list * bool) vector =
   let
      fun parseAndElaborateMLB input =
         let
            val _ = if !Control.keepAST
                       then File.remove (concat [!Control.inputFile, ".ast"])
                       else ()
            val _ = Const.lookup := lookupConstant
            val (E, decs) = Elaborate.elaborateMLB (lexAndParseMLB input, {addPrim = addPrim})
            val _ = Control.checkForErrors ()
            val _ = Option.map (!Control.showBasis, fn f => Env.showBasis (E, f))
            val _ = Env.processDefUse E
            val _ = Option.app (!Control.exportHeader, Ffi.exportHeader)
         in
            decs
         end
   in
      Control.translatePass
      {arg = input,
       doit = parseAndElaborateMLB,
       name = "parseAndElaborate",
       srcToFile = MLBString.toFile,
       tgtStats = fn _ => Layout.empty,
       tgtToFile = {display = (Control.Layouts
                            (fn (decss, output) =>
                             (output (Layout.str "\n");
                              Vector.foreach
                              (decss, fn (decs, dc) =>
                               (output (Layout.seq [Layout.str "(* deadCode: ",
                                                    Bool.layout dc,
                                                    Layout.str " *)"]);
                                List.foreach
                                (decs, output o CoreML.Dec.layout)))))),
                    style = #style CoreML.Program.toFile,
                    suffix = #suffix CoreML.Program.toFile},
       tgtTypeCheck = NONE}
   end

(* ------------------------------------------------- *)
(*                   Basis Library                   *)
(* ------------------------------------------------- *)

fun outputBasisConstants (out: Out.t): unit =
   let
      val _ = amBuildingConstants := true
      val decs =
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

fun elaborate {input: MLBString.t}: Xml.Program.t =
   let
      val decs = parseAndElaborateMLB input
      val coreML =
         let
            fun deadCode decs =
               let
                  val {prog = decs} =
                     DeadCode.deadCode {prog = decs}
                  val decs = Vector.concatV (Vector.map (decs, Vector.fromList))
                  val coreML = CoreML.Program.T {decs = decs}
               in
                  coreML
               end
            val coreML =
               Control.translatePass
               {arg = decs,
                doit = deadCode,
                name = "deadCode",
                srcToFile = {display = (Control.Layouts
                                        (fn (decss, output) =>
                                         (output (Layout.str "\n");
                                          Vector.foreach
                                          (decss, fn (decs, dc) =>
                                           (output (Layout.seq [Layout.str "(* deadCode: ",
                                                                Bool.layout dc,
                                                                Layout.str " *)"]);
                                            List.foreach
                                            (decs, output o CoreML.Dec.layout)))))),
                             style = #style CoreML.Program.toFile,
                             suffix = #suffix CoreML.Program.toFile},
                tgtStats = fn _ => Layout.empty,
                tgtToFile = CoreML.Program.toFile,
                tgtTypeCheck = NONE}
         in
            coreML
         end
      val _ =
         let
            open Control
         in
            if !keepCoreML
               then saveToFile {arg = coreML, name = NONE, toFile = CoreML.Program.toFile, verb = Pass}
            else ()
         end
      val xml =
         Control.translatePass
         {arg = coreML,
          doit = Defunctorize.defunctorize,
          name = "defunctorize",
          srcToFile = CoreML.Program.toFile,
          tgtStats = Xml.Program.layoutStats,
          tgtToFile = Xml.Program.toFile,
          tgtTypeCheck = SOME Xml.typeCheck}
   in
      xml
   end

fun simplifyXml xml =
   let
      val xml =
         Control.simplifyPass
         {arg = xml,
          doit = Xml.simplify,
          execute = true,
          name = "xmlSimplify",
          stats = Xml.Program.layoutStats,
          toFile = Xml.Program.toFile,
          typeCheck = Xml.typeCheck}
      open Control
      val _ =
         if !keepXML
            then saveToFile {arg = xml, name = NONE, toFile = Xml.Program.toFile, verb = Pass}
            else ()
   in
      xml
   end

fun makeSxml xml =
   Control.translatePass
   {arg = xml,
    doit = Monomorphise.monomorphise,
    name = "monomorphise",
    srcToFile = Xml.Program.toFile,
    tgtStats = Sxml.Program.layoutStats,
    tgtToFile = Sxml.Program.toFile,
    tgtTypeCheck = SOME Sxml.typeCheck}

fun simplifySxml sxml =
   let
      val sxml =
         Control.simplifyPass
         {arg = sxml,
          doit = Sxml.simplify,
          execute = true,
          name = "sxmlSimplify",
          stats = Sxml.Program.layoutStats,
          toFile = Sxml.Program.toFile,
          typeCheck = Sxml.typeCheck}
      open Control
      val _ =
         if !keepSXML
            then saveToFile {arg = sxml, name = NONE, toFile = Sxml.Program.toFile, verb = Pass}
            else ()
   in
      sxml
   end

fun makeSsa sxml =
   Control.translatePass
   {arg = sxml,
    doit = ClosureConvert.closureConvert,
    name = "closureConvert",
    srcToFile = Sxml.Program.toFile,
    tgtStats = Ssa.Program.layoutStats,
    tgtToFile = Ssa.Program.toFile,
    tgtTypeCheck = SOME Ssa.typeCheck}

fun simplifySsa ssa =
   let
      val ssa =
         Control.simplifyPass
         {arg = ssa,
          doit = Ssa.simplify,
          execute = true,
          name = "ssaSimplify",
          stats = Ssa.Program.layoutStats,
          toFile = Ssa.Program.toFile,
          typeCheck = Ssa.typeCheck}
      open Control
      val _ =
         if !keepSSA
            then saveToFile {arg = ssa, name = NONE, toFile = Ssa.Program.toFile, verb = Pass}
         else ()
   in
      ssa
   end

fun makeSsa2 ssa =
   Control.translatePass
   {arg = ssa,
    doit = SsaToSsa2.convert,
    name = "toSsa2",
    srcToFile = Ssa.Program.toFile,
    tgtStats = Ssa2.Program.layoutStats,
    tgtToFile = Ssa2.Program.toFile,
    tgtTypeCheck = SOME Ssa2.typeCheck}

fun simplifySsa2 ssa2 =
   let
      val ssa2 =
         Control.simplifyPass
         {arg = ssa2,
          doit = Ssa2.simplify,
          execute = true,
          name = "ssa2Simplify",
          stats = Ssa2.Program.layoutStats,
          toFile = Ssa2.Program.toFile,
          typeCheck = Ssa2.typeCheck}
      open Control
      val _ =
         if !keepSSA2
            then saveToFile {arg = ssa2, name = NONE, toFile = Ssa2.Program.toFile, verb = Pass}
         else ()
   in
      ssa2
   end

fun makeMachine ssa2 =
   let
      val codegenImplementsPrim =
         case !Control.codegen of
            Control.AMD64Codegen => amd64Codegen.implementsPrim
          | Control.CCodegen => CCodegen.implementsPrim
          | Control.LLVMCodegen => LLVMCodegen.implementsPrim
          | Control.X86Codegen => x86Codegen.implementsPrim
      val machine =
         Control.translatePass
         {arg = (ssa2, {codegenImplementsPrim = codegenImplementsPrim}),
          doit = Backend.toMachine,
          name = "backend",
          srcToFile = {display = Control.Layouts (fn ((ssa2, _), output) =>
                                                  Ssa2.Program.layouts (ssa2, output)),
                       style = #style Ssa2.Program.toFile,
                       suffix = #suffix Ssa2.Program.toFile},
          tgtStats = fn _ => Layout.empty,
          tgtToFile = Machine.Program.toFile,
          tgtTypeCheck = SOME Machine.Program.typeCheck}
      val _ =
         let
            open Control
         in
            if !keepMachine
               then saveToFile {arg = machine, name = NONE, toFile = Machine.Program.toFile, verb = Pass}
            else ()
         end
   in
      machine
   end

fun setupConstants() : unit = 
   (* Set GC_state offsets and sizes. *)
   let
      val _ =
         let
            fun get (name: string): Bytes.t =
               case lookupConstant ({default = NONE, name = name},
                                    ConstType.Word WordSize.word32) of
                  Const.Word w => Bytes.fromInt (WordX.toInt w)
                | _ => Error.bug "Compile.setupConstants: GC_state offset must be an int"
         in
            Runtime.GCField.setOffsets
            {
             atomicState = get "atomicState_Offset",
             cardMapAbsolute = get "generationalMaps.cardMapAbsolute_Offset",
             currentThread = get "currentThread_Offset",
             curSourceSeqIndex = get "sourceMaps.curSourceSeqIndex_Offset",
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
             curSourceSeqIndex = get "sourceMaps.curSourceSeqIndex_Size",
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
                 | _ => Error.bug "Compile.setupConstants: endian unknown"
         in
            Control.Target.setBigEndian (get "MLton_Platform_Arch_bigendian")
         end
   in
      ()
   end


fun preCodegen (input: MLBString.t): Machine.Program.t =
   let
      val xml = elaborate {input = input}
      val _ = setupConstants ()
      val xml = simplifyXml xml
      val sxml = makeSxml xml
      val sxml = simplifySxml sxml
      val ssa = makeSsa sxml
      val ssa = simplifySsa ssa
      val ssa2 = makeSsa2 ssa
      val ssa2 = simplifySsa2 ssa2
   in
      makeMachine ssa2
   end

fun compile {input: 'a, resolve: 'a -> Machine.Program.t, outputC, outputLL, outputS}: unit =
   let
      val machine =
         Control.trace (Control.Top, "pre codegen")
         resolve input
      fun clearNames () =
         (Machine.Program.clearLabelNames machine
          ; Machine.Label.printNameAlphaNumeric := true)
      val () =
         case !Control.codegen of
            Control.AMD64Codegen =>
               (clearNames ()
                ; (Control.trace (Control.Top, "amd64 code gen")
                   amd64Codegen.output {program = machine,
                                        outputC = outputC,
                                        outputS = outputS}))
          | Control.CCodegen =>
               (clearNames ()
                ; (Control.trace (Control.Top, "C code gen")
                   CCodegen.output {program = machine,
                                    outputC = outputC}))
          | Control.LLVMCodegen =>
               (clearNames ()
                ; (Control.trace (Control.Top, "llvm code gen")
                   LLVMCodegen.output {program = machine,
                                       outputC = outputC,
                                      outputLL = outputLL}))
          | Control.X86Codegen =>
               (clearNames ()
                ; (Control.trace (Control.Top, "x86 code gen")
                   x86Codegen.output {program = machine,
                                      outputC = outputC,
                                      outputS = outputS}))
      val _ = Control.message (Control.Detail, PropertyList.stats)
      val _ = Control.message (Control.Detail, HashSet.stats)
   in
      ()
   end

fun compileMLB {input: File.t, outputC, outputLL, outputS}: unit =
   compile {input = MLBString.fromFile input,
            resolve = preCodegen,
            outputC = outputC,
            outputLL = outputLL,
            outputS = outputS}

val elaborateMLB =
   fn {input: File.t} =>
   (ignore (elaborate {input = MLBString.fromFile input}))

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
   fun compileSML {input: File.t list, outputC, outputLL, outputS}: unit =
      compile {input = genMLB {input = input},
               resolve = preCodegen,
               outputC = outputC,
               outputLL = outputLL,
               outputS = outputS}
   val elaborateSML =
      fn {input: File.t list} =>
      (ignore (elaborate {input = genMLB {input = input}}))
end

fun genFromXML (input: File.t): Machine.Program.t =
   let
      val _ = setupConstants()
      val xml =
         Control.passTypeCheck
         {name = ("xmlParse", NONE),
          stats = Xml.Program.layoutStats,
          thunk = (fn () => case
                     Parse.parseFile(Xml.Program.parse (), input)
                        of Result.Yes x => x
                         | Result.No msg => (Control.error
                           (Region.bogus, Layout.str "Xml Parse failed", Layout.str msg);
                            Control.checkForErrors();
                            (* can't be reached *)
                            raise Fail "parse")
                   ),
          toFile = Xml.Program.toFile,
          typeCheck = Xml.typeCheck}
      val xml = simplifyXml xml
      val sxml = makeSxml xml
      val sxml = simplifySxml sxml
      val ssa = makeSsa sxml
      val ssa = simplifySsa ssa
      val ssa2 = makeSsa2 ssa
      val ssa2 = simplifySsa2 ssa2
   in
      makeMachine ssa2
   end
fun compileXML {input: File.t, outputC, outputLL, outputS}: unit =
   compile {input = input,
            resolve = genFromXML,
            outputC = outputC,
            outputLL = outputLL,
            outputS = outputS}

fun genFromSXML (input: File.t): Machine.Program.t =
   let
      val _ = setupConstants()
      val sxml =
         Control.passTypeCheck
         {name = ("sxmlParse", NONE),
          stats = Sxml.Program.layoutStats,
          thunk = (fn () => case
                     Parse.parseFile(Sxml.Program.parse (), input)
                        of Result.Yes x => x
                         | Result.No msg => (Control.error 
                           (Region.bogus, Layout.str "Sxml Parse failed", Layout.str msg);
                            Control.checkForErrors();
                            (* can't be reached *)
                            raise Fail "parse")
                   ),
          toFile = Sxml.Program.toFile,
          typeCheck = Sxml.typeCheck}
      val sxml = simplifySxml sxml
      val ssa = makeSsa sxml
      val ssa = simplifySsa ssa
      val ssa2 = makeSsa2 ssa
      val ssa2 = simplifySsa2 ssa2
   in
      makeMachine ssa2
   end
fun compileSXML {input: File.t, outputC, outputLL, outputS}: unit =
   compile {input = input,
            resolve = genFromSXML,
            outputC = outputC,
            outputLL = outputLL,
            outputS = outputS}

fun genFromSsa (input: File.t): Machine.Program.t =
   let
      val _ = setupConstants()
      val ssa =
         Control.passTypeCheck
         {name = ("ssaParse", NONE),
          stats = Ssa.Program.layoutStats,
          thunk = (fn () => case
                     Parse.parseFile(Ssa.Program.parse (), input)
                        of Result.Yes x => x
                         | Result.No msg => (Control.error 
                           (Region.bogus, Layout.str "Ssa Parse failed", Layout.str msg);
                            Control.checkForErrors();
                            (* can't be reached *)
                            raise Fail "parse")
                   ),
          toFile = Ssa.Program.toFile,
          typeCheck = Ssa.typeCheck}
      val ssa = simplifySsa ssa
      val ssa2 = makeSsa2 ssa
      val ssa2 = simplifySsa2 ssa2
   in
      makeMachine ssa2
   end
fun compileSSA {input: File.t, outputC, outputLL, outputS}: unit =
   compile {input = input,
            resolve = genFromSsa,
            outputC = outputC,
            outputLL = outputLL,
            outputS = outputS}

fun genFromSsa2 (input: File.t): Machine.Program.t =
               let
                  val _ = setupConstants()
                  val ssa2 =
                     Control.passTypeCheck
                     {name = ("ssa2Parse", NONE),
                      stats = Ssa2.Program.layoutStats,
                      thunk = (fn () => case
                                 Parse.parseFile(Ssa2.Program.parse (), input)
                                    of Result.Yes x => x
                                     | Result.No msg => (Control.error
                                       (Region.bogus, Layout.str "Ssa2 Parse failed", Layout.str msg);
                                        Control.checkForErrors();
                                        (* can't be reached *)
                                        raise Fail "parse")
                               ),
                      toFile = Ssa2.Program.toFile,
                      typeCheck = Ssa2.typeCheck}
                  (*val ssa2 = makeSsa2 ssa*)
                  val ssa2 = simplifySsa2 ssa2
               in
                  makeMachine ssa2
               end

 fun compileSSA2 {input: File.t, outputC, outputLL, outputS}: unit =
               compile {input = input,
                        resolve = genFromSsa2,
                        outputC = outputC,
                        outputLL = outputLL,
                        outputS = outputS}


end
