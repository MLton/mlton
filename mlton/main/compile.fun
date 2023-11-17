(* Copyright (C) 2011,2014-2015,2017,2019-2021,2023 Matthew Fluet.
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
   structure Ffi = Ffi
   structure Symbol = Symbol
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
structure BackendAtoms = BackendAtoms (open Atoms)
structure Rssa = Rssa (open BackendAtoms)
structure Machine = Machine (open BackendAtoms)

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
structure Monomorphise = Monomorphise (structure Xml = Xml
                                       structure Sxml = Sxml)
structure ClosureConvert = ClosureConvert (structure Ssa = Ssa
                                           structure Sxml = Sxml)
structure SsaToSsa2 = SsaToSsa2 (structure Ssa = Ssa
                                 structure Ssa2 = Ssa2)
structure Ssa2ToRssa = Ssa2ToRssa (structure Rssa = Rssa
                                   structure Ssa2 = Ssa2)
structure Backend = Backend (structure Machine = Machine
                             structure Rssa = Rssa
                             fun funcToLabel f = f)
structure CCodegen = CCodegen (structure Machine = Machine)
structure LLVMCodegen = LLVMCodegen (structure CCodegen = CCodegen
                                     structure Machine = Machine)
structure x86Codegen = x86Codegen (structure CCodegen = CCodegen
                                   structure Machine = Machine)
structure amd64Codegen = amd64Codegen (structure CCodegen = CCodegen
                                       structure Machine = Machine)

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

structure MLBString:>
   sig
      type t

      val fromMLBFile: File.t -> t
      val fromSMLFile: File.t -> t
      val lexAndParseMLB: t -> Ast.Basdec.t
   end =
   struct
      type t = string

      fun quoteFile s = concat ["\"", String.escapeSML s, "\""]

      val fromMLBFile = quoteFile

      fun fromSMLFile input =
         let
            val basis = "$(SML_LIB)/basis/default.mlb"
         in
            String.concat
            ["local\n",
             basis, "\n",
             "in\n",
             quoteFile input, "\n",
             "end\n"]
         end

      val lexAndParseMLB = MLBFrontEnd.lexAndParseString
   end

val lexAndParseMLB: MLBString.t -> Ast.Basdec.t = 
   fn input =>
   let
      val ast = MLBString.lexAndParseMLB input
      val _ = Control.checkForErrors ()
   in
      ast
   end

fun parseAndElaborateMLB (input: MLBString.t): (CoreML.Dec.t list * bool) vector =
   let
      fun parseAndElaborateMLB input =
         let
            val _ = if !Control.keepAST
                       then File.remove (concat [!Control.inputFile, ".ast"])
                       else ()
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
       keepIL = false,
       name = "parseAndElaborate",
       srcToFile = NONE,
       tgtStats = SOME (fn coreML => Control.sizeMessage ("coreML program", coreML)),
       tgtToFile = SOME {display = (Control.Layouts
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
(*                      compile                      *)
(* ------------------------------------------------- *)

fun mkCompile {outputC, outputLL, outputS} =
   let
      local
         val sourceFiles = Ast.Basdec.sourceFiles o lexAndParseMLB
      in
         val mlbSourceFiles = sourceFiles o MLBString.fromMLBFile
         val smlSourceFiles = sourceFiles o MLBString.fromSMLFile
      end

      fun deadCode decs =
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
                keepIL = !Control.keepCoreML,
                name = "deadCode",
                srcToFile = SOME {display = (Control.Layouts
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
                tgtStats = SOME CoreML.Program.layoutStats,
                tgtToFile = SOME CoreML.Program.toFile,
                tgtTypeCheck = NONE}
         in
            coreML
         end
      fun defunctorize coreML =
         Control.translatePass
         {arg = coreML,
          doit = (fn coreML =>
                  Defunctorize.defunctorize coreML
                  before Control.checkForErrors ()),
          keepIL = false,
          name = "defunctorize",
          srcToFile = SOME CoreML.Program.toFile,
          tgtStats = SOME Xml.Program.layoutStats,
          tgtToFile = SOME Xml.Program.toFile,
          tgtTypeCheck = SOME (Xml.typeCheck, NONE)}
      fun frontend input =
         Control.translatePass
         {arg = input,
          doit = defunctorize o deadCode o parseAndElaborateMLB,
          keepIL = false,
          name = "frontend",
          srcToFile = NONE,
          tgtStats = SOME Xml.Program.layoutStats,
          tgtToFile = SOME Xml.Program.toFile,
          tgtTypeCheck = SOME (Xml.typeCheck, SOME true)}
      val mlbFrontend = frontend o MLBString.fromMLBFile
      val smlFrontend = frontend o MLBString.fromSMLFile

      fun regionFromLocation (file, {column, line}) =
        let
           val sourcePos = SourcePos.make
              {column=column, file=file, line=line}
        in
           Region.make {left=sourcePos, right=sourcePos}
        end

      fun mkFrontend {parse, stats, toFile, typeCheck} =
         let
            val name = #suffix toFile
         in
            fn input =>
            Control.translatePass
            {arg = input,
             doit = (fn input =>
                     case Parse.parseFile (parse (), input) of
                        Parse.Yes program => program
                      | Parse.No (msg, location) =>
                           (Control.error
                            (regionFromLocation (input, location),
                             Layout.str (concat [name, "Parse failed"]),
                             msg)
                            ; Control.checkForErrors ()
                            ; Error.bug "unreachable")),
             keepIL = false,
             name = concat [name, "Parse"],
             srcToFile = NONE,
             tgtStats = SOME stats,
             tgtToFile = SOME toFile,
             tgtTypeCheck = SOME (typeCheck, SOME true)}
         end
      val xmlFrontend =
         mkFrontend
         {parse = Xml.Program.parse,
          stats = Xml.Program.layoutStats,
          toFile = Xml.Program.toFile,
          typeCheck = Xml.typeCheck}
      val sxmlFrontend =
         mkFrontend
         {parse = Sxml.Program.parse,
          stats = Sxml.Program.layoutStats,
          toFile = Sxml.Program.toFile,
          typeCheck = Sxml.typeCheck}
      val ssaFrontend =
         mkFrontend
         {parse = Ssa.Program.parse,
          stats = Ssa.Program.layoutStats,
          toFile = Ssa.Program.toFile,
          typeCheck = Ssa.typeCheck}
      val ssa2Frontend =
         mkFrontend
         {parse = Ssa2.Program.parse,
          stats = Ssa2.Program.layoutStats,
          toFile = Ssa2.Program.toFile,
          typeCheck = Ssa2.typeCheck}

      fun xmlSimplify xml =
         let
            val xml =
               Control.simplifyPass
               {arg = xml,
                doit = Xml.simplify,
                execute = true,
                forceTypeCheck = SOME true,
                keepIL = !Control.keepXML,
                name = "xmlSimplify",
                stats = Xml.Program.layoutStats,
                toFile = Xml.Program.toFile,
                typeCheck = Xml.typeCheck}
         in
            xml
         end
      fun toSxml xml =
         Control.translatePass
         {arg = xml,
          doit = Monomorphise.monomorphise,
          keepIL = false,
          name = "monomorphise",
          srcToFile = SOME Xml.Program.toFile,
          tgtStats = SOME Sxml.Program.layoutStats,
          tgtToFile = SOME Sxml.Program.toFile,
          tgtTypeCheck = SOME (Sxml.typeCheck, SOME true)}
      fun sxmlSimplify sxml =
         let
            val sxml =
               Control.simplifyPass
               {arg = sxml,
                doit = Sxml.simplify,
                forceTypeCheck = SOME true,
                execute = true,
                keepIL = !Control.keepSXML,
                name = "sxmlSimplify",
                stats = Sxml.Program.layoutStats,
                toFile = Sxml.Program.toFile,
                typeCheck = Sxml.typeCheck}
         in
            sxml
         end
      fun toSsa sxml =
         Control.translatePass
         {arg = sxml,
          doit = ClosureConvert.closureConvert,
          keepIL = false,
          name = "closureConvert",
          srcToFile = SOME Sxml.Program.toFile,
          tgtStats = SOME Ssa.Program.layoutStats,
          tgtToFile = SOME Ssa.Program.toFile,
          tgtTypeCheck = SOME (Ssa.typeCheck, SOME true)}
      fun ssaSimplify ssa =
         let
            val ssa =
               Control.simplifyPass
               {arg = ssa,
                doit = Ssa.simplify,
                execute = true,
                forceTypeCheck = SOME true,
                keepIL = !Control.keepSSA,
                name = "ssaSimplify",
                stats = Ssa.Program.layoutStats,
                toFile = Ssa.Program.toFile,
                typeCheck = Ssa.typeCheck}
         in
            ssa
         end
      fun toSsa2 ssa =
         Control.translatePass
         {arg = ssa,
          doit = SsaToSsa2.convert,
          keepIL = false,
          name = "toSsa2",
          srcToFile = SOME Ssa.Program.toFile,
          tgtStats = SOME Ssa2.Program.layoutStats,
          tgtToFile = SOME Ssa2.Program.toFile,
          tgtTypeCheck = SOME (Ssa2.typeCheck, SOME true)}
      fun ssa2Simplify ssa2 =
         let
            val ssa2 =
               Control.simplifyPass
               {arg = ssa2,
                doit = Ssa2.simplify,
                forceTypeCheck = SOME true,
                execute = true,
                keepIL = !Control.keepSSA2,
                name = "ssa2Simplify",
                stats = Ssa2.Program.layoutStats,
                toFile = Ssa2.Program.toFile,
                typeCheck = Ssa2.typeCheck}
         in
            ssa2
         end
      fun toRssa ssa2 =
         let
            val codegenImplementsPrim =
               case !Control.codegen of
                  Control.AMD64Codegen => amd64Codegen.implementsPrim
                | Control.CCodegen => CCodegen.implementsPrim
                | Control.LLVMCodegen => LLVMCodegen.implementsPrim
                | Control.X86Codegen => x86Codegen.implementsPrim
            fun toRssa ssa2 =
               Ssa2ToRssa.convert
               (ssa2, {codegenImplementsPrim = codegenImplementsPrim})
            val rssa =
               Control.translatePass
               {arg = ssa2,
                doit = toRssa,
                keepIL = false,
                name = "toRssa",
                srcToFile = SOME Ssa2.Program.toFile,
                tgtStats = SOME Rssa.Program.layoutStats,
                tgtToFile = SOME Rssa.Program.toFile,
                tgtTypeCheck = SOME (Rssa.typeCheck, SOME true)}
         in
            rssa
         end
      fun rssaSimplify rssa =
         Control.simplifyPass
         {arg = rssa,
          doit = Rssa.simplify,
          execute = true,
          forceTypeCheck = SOME true,
          keepIL = !Control.keepRSSA,
          name = "rssaSimplify",
          stats = Rssa.Program.layoutStats,
          toFile = Rssa.Program.toFile,
          typeCheck = Rssa.typeCheck}
      fun toMachine rssa =
         let
            val machine =
               Control.translatePass
               {arg = rssa,
                doit = Backend.toMachine,
                keepIL = false,
                name = "backend",
                srcToFile = SOME Rssa.Program.toFile,
                tgtStats = SOME Machine.Program.layoutStats,
                tgtToFile = SOME Machine.Program.toFile,
                (* Machine type check is too slow to run by default. *)
                tgtTypeCheck = SOME (Machine.Program.typeCheck, SOME false)}
         in
            machine
         end
      fun machineSimplify machine =
         Control.simplifyPass
         {arg = machine,
          doit = Machine.simplify,
          execute = true,
          (* Machine type check is too slow to run by default. *)
          forceTypeCheck = SOME false,
          keepIL = !Control.keepMachine,
          name = "machineSimplify",
          stats = Machine.Program.layoutStats,
          toFile = Machine.Program.toFile,
          typeCheck = Machine.Program.typeCheck}
      fun codegen machine =
         let
            val _ = Machine.Program.clearLabelNames machine
            val _ = Machine.Label.printNameAlphaNumeric := true
            fun codegen machine =
               case !Control.codegen of
                  Control.AMD64Codegen =>
                     amd64Codegen.output {program = machine,
                                          outputC = outputC,
                                          outputS = outputS}
                | Control.CCodegen =>
                     CCodegen.output {program = machine,
                                      outputC = outputC}
                | Control.LLVMCodegen =>
                     LLVMCodegen.output {program = machine,
                                         outputC = outputC,
                                         outputLL = outputLL}
                | Control.X86Codegen =>
                     x86Codegen.output {program = machine,
                                        outputC = outputC,
                                        outputS = outputS}
         in
            Control.translatePass
            {arg = machine,
             doit = codegen,
             keepIL = false,
             name = concat [Control.Codegen.toString (!Control.codegen), "Codegen"],
             srcToFile = SOME Machine.Program.toFile,
             tgtStats = NONE,
             tgtToFile = NONE,
             tgtTypeCheck = NONE}
         end

      val goCodegen = codegen
      val goMachineSimplify = goCodegen o machineSimplify
      val goToMachine = goMachineSimplify o toMachine
      val goRssaSimplify = goToMachine o rssaSimplify
      val goToRssa = goRssaSimplify o toRssa
      val goSsa2Simplify = goToRssa o ssa2Simplify
      val goToSsa2 = goSsa2Simplify o toSsa2
      val goSsaSimplify = goToSsa2 o ssaSimplify
      val goToSsa = goSsaSimplify o toSsa
      val goSxmlSimplify = goToSsa o sxmlSimplify
      val goToSxml = goSxmlSimplify o toSxml
      val goXmlSimplify = goToSxml o xmlSimplify

      fun mk (il, sourceFiles, frontend, compile) =
         {sourceFiles = sourceFiles,
          frontend = Control.trace (Control.Top, "Type Check " ^ il) (ignore o frontend),
          compile = Control.trace (Control.Top, "Compile " ^ il) (compile o frontend)}
   in
      {mlb = mk ("SML", mlbSourceFiles, mlbFrontend, goXmlSimplify),
       sml = mk ("SML", smlSourceFiles, smlFrontend, goXmlSimplify),
       xml = mk ("XML", Vector.new1, xmlFrontend, goXmlSimplify),
       sxml = mk ("SXML", Vector.new1, sxmlFrontend, goSxmlSimplify),
       ssa = mk ("SSA", Vector.new1, ssaFrontend, goSsaSimplify),
       ssa2 = mk ("SSA2", Vector.new1, ssa2Frontend, goSsa2Simplify)}
   end
end
