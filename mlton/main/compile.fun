(* Copyright (C) 2011,2014 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
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
   Control.Layouts
   (fn ((_, decs),output) =>
    (output (Layout.str "\n\n")
     ; Vector.foreach
       (decs, fn (dec, dc) =>
        (output o Layout.record)
        [("deadCode", Bool.layout dc),
         ("decs", List.layout CoreML.Dec.layout dec)])))

fun parseAndElaborateMLB (input: MLBString.t)
   : Env.t * (CoreML.Dec.t list * bool) vector =
   Control.pass
   {display = displayEnvDecs,
    name = "parseAndElaborate",
    stats = fn _ => Layout.empty,
    style = Control.ML,
    suffix = "core-ml",
    thunk = (fn () =>
             (Const.lookup := lookupConstant
              ; elaborateMLB (lexAndParseMLB input, {addPrim = addPrim})))}

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
                   fun print s = Out.output (out, s)
                   val libname = !Control.libname
                   val libcap = CharVector.map Char.toUpper libname
                   val _ = print ("#ifndef __" ^ libcap ^ "_ML_H__\n")
                   val _ = print ("#define __" ^ libcap ^ "_ML_H__\n")
                   val _ = print "\n"
                   val _ =
                      File.outputContents
                      (concat [!Control.libDir, "/include/ml-types.h"], out)
                   val _ = print "\n"
                   val _ =
                      File.outputContents
                      (concat [!Control.libDir, "/include/export.h"], out)
                   val _ = print "\n"
                   (* How do programs link against this library by default *)
                   val defaultLinkage =
                      case !Control.format of
                         Control.Archive    => "STATIC_LINK"
                       | Control.Executable => "PART_OF"
                       | Control.LibArchive => "NO_DEFAULT_LINK"
                       | Control.Library    => "DYNAMIC_LINK"
                   val _ = 
                      print ("#if !defined(PART_OF_"      ^ libcap ^ ") && \\\n\
                             \    !defined(STATIC_LINK_"  ^ libcap ^ ") && \\\n\
                             \    !defined(DYNAMIC_LINK_" ^ libcap ^ ")\n")
                   val _ = 
                      print ("#define " ^ defaultLinkage ^ "_" ^ libcap ^ "\n")
                   val _ = print "#endif\n"
                   val _ = print "\n"
                   val _ = print ("#if defined(PART_OF_" ^ libcap ^ ")\n")
                   val _ = print "#define MLLIB_PRIVATE(x) PRIVATE x\n"
                   val _ = print "#define MLLIB_PUBLIC(x) PUBLIC x\n"
                   val _ = print ("#elif defined(STATIC_LINK_" ^ libcap ^ ")\n")
                   val _ = print "#define MLLIB_PRIVATE(x)\n"
                   val _ = print "#define MLLIB_PUBLIC(x) PUBLIC x\n"
                   val _ = print ("#elif defined(DYNAMIC_LINK_" ^ libcap ^ ")\n")
                   val _ = print "#define MLLIB_PRIVATE(x)\n"
                   val _ = print "#define MLLIB_PUBLIC(x) EXTERNAL x\n"
                   val _ = print "#else\n"
                   val _ = print ("#error Must specify linkage for " ^ libname ^ "\n")
                   val _ = print "#define MLLIB_PRIVATE(x)\n"
                   val _ = print "#define MLLIB_PUBLIC(x)\n"
                   val _ = print "#endif\n"
                   val _ = print "\n"
                   val _ = print "#ifdef __cplusplus\n"
                   val _ = print "extern \"C\" {\n"
                   val _ = print "#endif\n"
                   val _ = print "\n"
                   val _ = 
                      if !Control.format = Control.Executable then () else
                          (print ("MLLIB_PUBLIC(void " ^ libname ^ "_open(int argc, const char** argv);)\n")
                          ;print ("MLLIB_PUBLIC(void " ^ libname ^ "_close();)\n"))
                   val _ = Ffi.declareHeaders {print = print} 
                   val _ = print "\n"
                   val _ = print "#undef MLLIB_PRIVATE\n"
                   val _ = print "#undef MLLIB_PUBLIC\n"
                   val _ = print "\n"
                   val _ = print "#ifdef __cplusplus\n"
                   val _ = print "}\n"
                   val _ = print "#endif\n"
                   val _ = print "\n"
                   val _ = print ("#endif /* __" ^ libcap ^ "_ML_H__ */\n")
                in
                   ()
                end)
      val _ = if !Control.elaborateOnly then raise Done else ()
      val decs =
         Control.pass
         {display = Control.Layouts (fn (decss,output) =>
                                     (output (Layout.str "\n\n")
                                      ; Vector.foreach (decss, fn decs =>
                                        List.foreach (decs, fn dec =>
                                        output (CoreML.Dec.layout dec))))),
          name = "deadCode",
          suffix = "core-ml",
          style = Control.ML,
          stats = fn _ => Layout.empty,
          thunk = fn () => let
                              val {prog = decs} =
                                 DeadCode.deadCode {prog = decs}
                           in
                              decs
                           end}
      val decs = Vector.concatV (Vector.map (decs, Vector.fromList))
      val coreML = CoreML.Program.T {decs = decs}
      val _ =
         let
            open Control
         in
            if !keepCoreML
               then saveToFile ({suffix = "core-ml"}, No, coreML,
                                Layouts CoreML.Program.layouts)
            else ()
         end

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
         {display = Control.Layouts Xml.Program.layouts,
          name = "defunctorize",
          stats = Xml.Program.layoutStats,
          style = Control.ML,
          suffix = "xml",
          thunk = fn () => Defunctorize.defunctorize coreML,
          typeCheck = Xml.typeCheck}
   in
      xml
   end

fun preCodegen {input: MLBString.t}: Machine.Program.t =
   let
      val xml = elaborate {input = input}
      val xml =
          Control.passTypeCheck
          {display = Control.Layouts Xml.Program.layouts,
           name = "xmlSimplify",
           stats = Xml.Program.layoutStats,
           style = Control.ML,
           suffix = "xml",
           thunk = fn () => Xml.simplify xml,
           typeCheck = Xml.typeCheck}
      val _ =
         let
            open Control
         in
            if !keepXML
               then saveToFile ({suffix = "xml"}, No, xml,
                                Layouts Xml.Program.layouts)
            else ()
         end
      val sxml =
         Control.passTypeCheck
         {display = Control.Layouts Sxml.Program.layouts,
          name = "monomorphise",
          stats = Sxml.Program.layoutStats,
          style = Control.ML,
          suffix = "sxml",
          thunk = fn () => Monomorphise.monomorphise xml,
          typeCheck = Sxml.typeCheck}
      val sxml =
         Control.passTypeCheck
         {display = Control.Layouts Sxml.Program.layouts,
          name = "sxmlSimplify",
          stats = Sxml.Program.layoutStats,
          style = Control.ML,
          suffix = "sxml",
          thunk = fn () => Sxml.simplify sxml,
          typeCheck = Sxml.typeCheck}
      val _ =
         let
            open Control
         in
            if !keepSXML
               then saveToFile ({suffix = "sxml"}, No, sxml,
                                Layouts Sxml.Program.layouts)
            else ()
         end
      val ssa =
         Control.passTypeCheck
         {display = Control.Layouts Ssa.Program.layouts,
          name = "closureConvert",
          stats = Ssa.Program.layoutStats,
          style = Control.No,
          suffix = "ssa",
          thunk = fn () => ClosureConvert.closureConvert sxml,
          typeCheck = Ssa.typeCheck}
      val ssa =
         Control.passTypeCheck
         {display = Control.Layouts Ssa.Program.layouts,
          name = "ssaSimplify",
          stats = Ssa.Program.layoutStats,
          style = Control.No,
          suffix = "ssa",
          thunk = fn () => Ssa.simplify ssa,
          typeCheck = Ssa.typeCheck}
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
         {display = Control.Layouts Ssa2.Program.layouts,
          name = "toSsa2",
          stats = Ssa2.Program.layoutStats,
          style = Control.No,
          suffix = "ssa2",
          thunk = fn () => SsaToSsa2.convert ssa,
          typeCheck = Ssa2.typeCheck}
      val ssa2 =
         Control.passTypeCheck
         {display = Control.Layouts Ssa2.Program.layouts,
          name = "ssa2Simplify",
          stats = Ssa2.Program.layoutStats,
          style = Control.No,
          suffix = "ssa2",
          thunk = fn () => Ssa2.simplify ssa2,
          typeCheck = Ssa2.typeCheck}
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
            Control.AMD64Codegen => amd64Codegen.implementsPrim
          | Control.CCodegen => CCodegen.implementsPrim
          | Control.LLVMCodegen => LLVMCodegen.implementsPrim
          | Control.X86Codegen => x86Codegen.implementsPrim
      val machine =
         Control.passTypeCheck
         {display = Control.Layouts Machine.Program.layouts,
          name = "backend",
          stats = fn _ => Layout.empty,
          style = Control.No,
          suffix = "machine",
          thunk = fn () =>
                  (Backend.toMachine
                   (ssa2,
                    {codegenImplementsPrim = codegenImplementsPrim})),
          typeCheck = fn machine =>
                      (* For now, machine type check is too slow to run. *)
                      (if !Control.typeCheck
                          then Machine.Program.typeCheck machine
                       else ())}
      val _ =
         let
            open Control
         in
            if !keepMachine
               then saveToFile ({suffix = "machine"}, No, machine,
                                Layouts Machine.Program.layouts)
            else ()
         end
   in
      machine
   end

fun compile {input: MLBString.t, outputC, outputLL, outputS}: unit =
   let
      val machine =
         Control.trace (Control.Top, "pre codegen")
         preCodegen {input = input}
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
   end handle Done => ()

fun compileMLB {input: File.t, outputC, outputLL, outputS}: unit =
   compile {input = MLBString.fromFile input,
            outputC = outputC,
            outputLL = outputLL,
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
   fun compileSML {input: File.t list, outputC, outputLL, outputS}: unit =
      compile {input = genMLB {input = input},
               outputC = outputC,
               outputLL = outputLL,
               outputS = outputS}
   val elaborateSML =
      fn {input: File.t list} =>
      (ignore (elaborate {input = genMLB {input = input}}))
      handle Done => ()
end

end
