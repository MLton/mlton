(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
   structure IntSize = IntSize
   structure RealSize = RealSize
   structure WordSize = WordSize
end
structure Atoms = Atoms (structure Field = Field
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
				    
				 val layout = layoutPretty
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
   structure ConstType = ConstType
   structure Env = Env
   structure Decs = Decs
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


(* ------------------------------------------------- *)
(*                 Lookup Constant                   *)
(* ------------------------------------------------- *)

val allConstants: (string * ConstType.t) list ref = ref []
val amBuildingConstants: bool ref = ref false
   
val lookupConstant =
   let
      val zero = Const.word (WordX.fromIntInf (0, WordSize.default))
      val f =
	 Promise.lazy
	 (fn () =>
	  if !amBuildingConstants
	     then fn ct => (List.push (allConstants, ct)
			    ; zero)
	  else
	     File.withIn
	     (concat [!Control.libTargetDir, "/constants"], fn ins =>
	      LookupConstant.load ins))
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
		  (Tycon.prims, fn (tycon, kind, _) =>
		   extendTycon
		   (E, Ast.Tycon.fromSymbol (Symbol.fromString
					     (Tycon.originalName tycon),
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
				     extendExn (E, Con.toAst c, c, scheme))
	    in
	       ()
	    end
      end

   val primitiveDecs: CoreML.Dec.t vector =
      let
	 open CoreML.Dec
      in
	 Vector.concat [Vector.new1 (Datatype primitiveDatatypes),
			Vector.fromListMap
			(primitiveExcons, fn c =>
			 Exception {con = c, arg = NONE})]
      end

in

   fun addPrim E =
      (Env.addPrim E
       ; Decs.fromVector primitiveDecs)
end


(* ------------------------------------------------- *)
(*                 parseAndElaborateMLB              *)
(* ------------------------------------------------- *)

datatype input = File of File.t | String of String.t

local
   val (lexAndParseMLBFile, lexAndParseMLBFileMsg) =
      Control.traceBatch (Control.Pass, "lex and parse")
      MLBFrontEnd.lexAndParseFile
   val (lexAndParseMLBString, lexAndParseMLBStringMsg) =
      Control.traceBatch (Control.Pass, "lex and parse")
      MLBFrontEnd.lexAndParseString
   val lexAndParseMLBMsgRef = ref lexAndParseMLBFileMsg
in
   fun lexAndParseMLB fs =
      case fs of
	 File f => (lexAndParseMLBMsgRef := lexAndParseMLBFileMsg
		    ; lexAndParseMLBFile f)
       | String s => (lexAndParseMLBMsgRef := lexAndParseMLBStringMsg
		      ; lexAndParseMLBString s)
   fun lexAndParseMLBMsg () =
      (!lexAndParseMLBMsgRef) ()
end

val lexAndParseMLB : input -> Ast.Basdec.t * File.t vector = fn (fs: input) => 
   let
      val (ast, files) = lexAndParseMLB fs
      val _ = Control.checkForErrors "parse"
   in (ast, files)
   end

val filesMLB = fn {input} => #2 (lexAndParseMLB (File input))
val lexAndParseMLB = #1 o lexAndParseMLB

val (elaborateMLB, elaborateMLBMsg) =
   Control.traceBatch (Control.Pass, "elaborate") Elaborate.elaborateMLB

val displayEnvDecs =
   Control.Layout
   (fn (_, ds) => 
    Vector.layout
    (fn (d, b) =>
     Layout.record
     [("deadCode", Bool.layout b),
      ("decs", Decs.layout d)])
    ds)
fun parseAndElaborateMLB (fs: input): Env.t * (Decs.t * bool) vector =
   Control.pass
   {name = "parseAndElaborate",
    suffix = "core-ml",
    style = Control.ML,
    thunk = fn () => 
    Ref.fluidLet
    (Elaborate.Ctrls.lookupConstant, lookupConstant, fn () =>
     elaborateMLB (lexAndParseMLB fs, {addPrim = addPrim})),
    display = displayEnvDecs}
   
(* ------------------------------------------------- *)
(*                   Basis Library                   *)
(* ------------------------------------------------- *)

fun outputBasisConstants (out: Out.t): unit =
   let
      val _ = amBuildingConstants := true
      val (_, decs) =
	 parseAndElaborateMLB (File "$(MLTON_ROOT)/basis/libs/primitive.mlb")
      val decs = Vector.map (decs, fn (decs, _) => Decs.toList decs)
      val decs = Vector.concatV (Vector.map (decs, Vector.fromList))
      (* Need to defunctorize so the constants are forced. *)
      val _ =
	 Defunctorize.defunctorize
	 (CoreML.Program.T {decs = decs})
      val _ = LookupConstant.build (!allConstants, out)
   in
      ()
   end

(* ------------------------------------------------- *)
(*                      compile                      *)
(* ------------------------------------------------- *)

exception Done

fun elaborate {input: input}: Xml.Program.t =
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
		      (concat [!Control.libDir, "/include/types.h"], out)
		   fun print s = Out.output (out, s)
		   val _ = print "\n"
		   val _ = Ffi.declareHeaders {print = print}
		in
		   ()
		end)
      val _ = (lexAndParseMLBMsg (); elaborateMLBMsg ())
      val _ = if !Control.elaborateOnly then raise Done else ()

      val decs =
	 Control.pass
	 {name = "deadCode",
	  suffix = "basis",
	  style = Control.ML,
	  thunk = fn () => let
			      val decs = 
				 Vector.map (decs, fn (decs, b) => 
					     (Decs.toList decs, b))
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
      (* Set GC_state offsets. *)
      val _ =
	 let
	    fun get (s: string): Bytes.t =
	       case lookupConstant (s, ConstType.Word) of
		  Const.Word w => Bytes.fromInt (WordX.toInt w)
		| _ => Error.bug "GC_state offset must be an int"
	 in
	    Runtime.GCField.setOffsets
	    {
	     canHandle = get "canHandle",
	     cardMap = get "cardMapForMutator",
	     currentThread = get "currentThread",
	     exnStack = get "exnStack",
	     frontier = get "frontier",
	     limit = get "limit",
	     limitPlusSlop = get "limitPlusSlop",
	     maxFrameSize = get "maxFrameSize",
	     signalIsPending = get "signalIsPending",
	     stackBottom = get "stackBottom",
	     stackLimit = get "stackLimit",
	     stackTop = get "stackTop"
	     }
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
      
fun preCodegen {input}: Machine.Program.t =
   let
      val xml = elaborate {input = input}
      val xml =
	  Control.passTypeCheck
	  {name = "simplify Xml",
	   suffix = "xml",
	   style = Control.ML,
	   thunk = fn () => Xml.simplify xml,
	   display = Control.Layout Xml.Program.layout,
	   typeCheck = Xml.typeCheck}
      val _ = Control.message (Control.Detail, fn () =>
			       Xml.Program.layoutStats xml)
      val sxml =
	 Control.passSimplify
	 {name = "mono",
	  suffix = "sxml",
	  style = Control.ML,
	  thunk = fn () => Monomorphise.monomorphise xml,
	  display = Control.Layout Sxml.Program.layout,
	  typeCheck = Sxml.typeCheck,
	  simplify = Sxml.simplify}
      val _ = Control.message (Control.Detail, fn () =>
			       Sxml.Program.layoutStats sxml)
      val ssa =
	 Control.passSimplify
	 {name = "closureConvert",
	  suffix = "ssa",
	  style = Control.No,
	  thunk = fn () => ClosureConvert.closureConvert sxml,
	  typeCheck = Ssa.typeCheck,
	  display = Control.Layouts Ssa.Program.layouts,
	  simplify = Ssa.simplify}
      val ssa =
	 Control.passSimplify
	 {name = "toSsa2",
	  suffix = "ssa",
	  style = Control.No,
	  thunk = fn () => SsaToSsa2.convert ssa,
	  typeCheck = Ssa2.typeCheck,
	  display = Control.Layouts Ssa2.Program.layouts,
	  simplify = Ssa2.simplify}
      val _ =
	 let
	    open Control
	 in
	    if !keepSSA
	       then saveToFile ({suffix = "ssa"}, No, ssa,
				 Layouts Ssa2.Program.layouts)
	    else ()
	 end
      val codegenImplementsPrim =
	 case !Control.codegen of
	    Control.Bytecode => Bytecode.implementsPrim
	  | Control.CCodegen => CCodegen.implementsPrim
	  | Control.Native => x86Codegen.implementsPrim
      val machine =
	 Control.pass
	 {name = "backend",
	  suffix = "machine",
	  style = Control.No,
	  thunk = fn () => (Backend.toMachine
			    (ssa,
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
 
fun compile {input: input, outputC, outputS}: unit =
   let
      val machine =
	 Control.trace (Control.Top, "pre codegen")
	 preCodegen {input = input}
      val _ = Machine.Program.clearLabelNames machine
      val _ = Machine.Label.printNameAlphaNumeric := true
      val _ =
	 case !Control.codegen of
	    Control.Bytecode =>
	       Control.trace (Control.Top, "Byte code gen")
	       Bytecode.output {program = machine,
				outputC = outputC}
	  | Control.CCodegen =>
	       Control.trace (Control.Top, "C code gen")
	       CCodegen.output {program = machine,
				outputC = outputC}
	  | Control.Native =>
	       Control.trace (Control.Top, "x86 code gen")
	       x86Codegen.output {program = machine,
				  outputC = outputC,
				  outputS = outputS}
      val _ = Control.message (Control.Detail, PropertyList.stats)
      val _ = Control.message (Control.Detail, HashSet.stats)
   in
      ()
   end handle Done => ()

fun compileMLB {input: File.t, outputC, outputS}: unit =
   compile {input = File input,
	    outputC = outputC,
	    outputS = outputS}

val elaborateMLB =
   fn {input: File.t} =>
   (ignore (elaborate {input = File input}))
   handle Done => ()

local
   fun genMLB {input: File.t list} =
      let
	 val basis =
	    String.concat
	    ["$(MLTON_ROOT)/basis/",!Control.basisLibrary,".mlb\n"]
	 val s =
	    if List.length input = 0
	       then basis
	       else 
		  String.concat
		  ["local\n",
		   basis,
		   "in\n",
		   String.concat (List.separate(input, "\n")), "\n",
		   "end\n"]
      in
	 String s
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
