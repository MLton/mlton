(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Compile (S: COMPILE_STRUCTS): COMPILE =
struct

(*---------------------------------------------------*)
(*              Intermediate Languages               *)
(*---------------------------------------------------*)
   
structure Field = Field ()
structure Record = Record (val isSorted = false
			   structure Field = Field)
structure SortedRecord = Record (val isSorted = true
				 structure Field = Field)
structure Tyvar = Tyvar ()
structure Ast = Ast (structure Record = Record
		     structure SortedRecord = SortedRecord
		     structure Tyvar = Tyvar)
local
   open Ast.Tycon
in
   structure IntSize = IntSize
   structure RealSize = RealSize
   structure WordSize = WordSize
end
structure Atoms = Atoms (structure Ast = Ast
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
   structure Ffi = Ffi
   structure IntX = IntX
end
structure TypeEnv = TypeEnv (Atoms)
structure CoreML = CoreML (open Atoms
			   structure Type =
			      struct
				 open TypeEnv.Type

				 val layout = layoutPretty
			      end)
structure Xml = Xml (open Atoms)
structure Sxml = Sxml (open Xml)
structure Ssa = Ssa (open Atoms)
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
   structure Decs = Decs
   structure Env = Env
end
structure LookupConstant = LookupConstant (structure Const = Const
					   structure ConstType = ConstType
					   structure Ffi = Ffi)
structure Monomorphise = Monomorphise (structure Xml = Xml
				       structure Sxml = Sxml)
structure ClosureConvert = ClosureConvert (structure Ssa = Ssa
					   structure Sxml = Sxml)
structure Backend = Backend (structure Ssa = Ssa
			     structure Machine = Machine
			     fun funcToLabel f = f)
structure CCodegen = CCodegen (structure Ffi = Ffi
			       structure Machine = Machine)
structure x86Codegen = x86Codegen (structure CCodegen = CCodegen
				   structure Machine = Machine)

local
   open Elaborate
in 
   structure Decs = Decs
end
   
(* ------------------------------------------------- *)
(*                 parseAndElaborate                 *)
(* ------------------------------------------------- *)

val (lexAndParse, lexAndParseMsg) =
   Control.traceBatch (Control.Pass, "lex and parse") FrontEnd.lexAndParse

fun lexAndParseFile (f: File.t): Ast.Program.t =
   let
      val ast = lexAndParse f
      val _ = Control.checkForErrors "parse"
   in ast
   end

fun lexAndParseFiles (fs: File.t list): Ast.Program.t =
   List.fold
   (fs, Ast.Program.empty, fn (f, ast) =>
    Ast.Program.append (ast, lexAndParseFile f))

val (elaborate, elaborateMsg) =
   Control.traceBatch (Control.Pass, "elaborate") Elaborate.elaborateProgram

fun elaborateProg z: Decs.t =
   let
      val decs = elaborate z
      val _ = Control.checkForErrors "elaborate"
   in
      decs
   end

val displayDecs =
   Control.Layout
   (fn ds => CoreML.Program.layout (CoreML.Program.T
				    {decs = Decs.toVector ds}))
   
fun parseAndElaborateFiles (fs: File.t list, E: Env.t, lookupConstant): Decs.t =
   Control.pass
   {name = "parseAndElaborate",
    suffix = "core-ml",
    style = Control.ML,
    thunk = fn () => (List.fold
		      (fs, Decs.empty, fn (f, ds) =>
		       Decs.append 
		       (ds, elaborateProg (lexAndParseFile f,
					   E,
					   lookupConstant)))),
    display = displayDecs}

(* ------------------------------------------------- *)   
(*                   Primitive Env                   *)
(* ------------------------------------------------- *)

local
   structure Con = TypeEnv.Con
   structure Scheme = TypeEnv.Scheme
   structure Tycon = TypeEnv.Tycon
   structure Type = TypeEnv.Type
   structure Tyvar = TypeEnv.Tyvar
in
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

	 fun toAst c = Ast.Con.fromString (Con.toString c, Region.bogus)
      end

   structure Tycon =
      struct
	 open Tycon

	 fun toAst c = Ast.Tycon.fromString (Tycon.toString c, Region.bogus)
      end
   
   structure Env =
      struct
	 open Env 

	 structure Type = TypeEnv.Type
	 structure Scheme = TypeEnv.Scheme

	 fun addPrim (E: t): unit =
	    let
	       val _ =
		  List.foreach
		  (Tycon.prims, fn (tycon, kind) =>
		   extendTycon
		   (E, Ast.Tycon.fromString (Tycon.originalName tycon,
					     Region.bogus),
		    TypeStr.tycon (tycon, kind)))
	       val _ =
		  Vector.foreach
		  (primitiveDatatypes, fn {tyvars, tycon, cons} =>
		   let
		      val cs =
			 Vector.map
			 (cons, fn {arg, con} =>
			  let
			     val resultType =
				Type.con (tycon, Vector.map (tyvars, Type.var))
			     val scheme =
				Scheme.make
				{canGeneralize = true,
				 ty = (case arg of
					  NONE => resultType
					| SOME t => Type.arrow (t, resultType)),
				 tyvars = tyvars}
			  in
			     {con = con,
			      name = Con.toAst con,
			      scheme = scheme}
			  end)
		      val _ =
			 Vector.foreach (cs, fn {con, name, scheme} =>
					 extendCon (E, name, con, scheme))
		   in
		      extendTycon
		      (E, Tycon.toAst tycon,
		       TypeStr.data (tycon,
				     TypeStr.Kind.Arity (Vector.length tyvars),
				     cs))
		   end)
	       val _ =
		  extendTycon (E, Ast.Tycon.fromString ("unit", Region.bogus),
			       TypeStr.def (Scheme.fromType Type.unit,
					    TypeStr.Kind.Arity 0))
	       val scheme = Scheme.fromType Type.exn
	       val _ = List.foreach (primitiveExcons, fn c =>
				     extendCon (E, Con.toAst c, c, scheme))
	    in
	       ()
	    end
      end
end

(* ------------------------------------------------- *)
(*                   Basis Library                   *)
(* ------------------------------------------------- *)

val basisEnv = Env.empty ()

val allConstants: (string * ConstType.t) list ref = ref []

val amBuildingConstants: bool ref = ref false
   
val lookupConstant =
   let
      val zero = Const.int (IntX.make (0, IntSize.default))
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

local
   val dir = ref NONE
in
   fun setBasisLibraryDir (d: Dir.t): unit =
      dir := SOME d
   fun basisLibrary ()
      : {build: Decs.t,
	 localTopFinish: (unit -> Decs.t * Decs.t * Decs.t) -> 
	 Decs.t * Decs.t * Decs.t,
	 libs: {name: string,
		bind: Ast.Program.t,
		prefix: Ast.Program.t,
		suffix: Ast.Program.t} list} =
       let
	  val d =
	     case !dir of
		NONE => Error.bug "basis library dir not set"
	      | SOME d => d
	  fun basisFile f = String./ (d, f)
	  fun libsFile f = basisFile (String./ ("libs", f))
	  fun withFiles (f, g) =
	     let
	        val fs = File.foldLines
		         (f, [], fn (s, ac) =>
			  if s <> "\n" andalso #"#" <> String.sub (s, 0)
			     then basisFile (String.dropLast s) :: ac
			  else ac)
	     in
	        g (List.rev fs)
	     end
	  val (build, localTopFinish) =
	     Env.localTop
	     (basisEnv,
	      fn () =>
	      (Env.addPrim basisEnv
	       ; withFiles (libsFile "build", 
			    fn fs => parseAndElaborateFiles (fs, basisEnv,
							     lookupConstant))))
	  val _ =
	     Env.Structure.ffi
	     := SOME (Env.lookupLongstrid
		      (basisEnv,
		       Ast.Longstrid.short
		       (Ast.Strid.fromString ("MLtonFFI", Region.bogus))))
	  val localTopFinish = fn g =>
	     (localTopFinish g) before ((* Env.addEquals basisEnv *)
					Env.clean basisEnv)

	  fun doit name =
	    let
	      fun libFile f = libsFile (String./ (name, f))
	      val bind = withFiles (libFile "bind", lexAndParseFiles)
	      val prefix = withFiles (libFile "prefix", lexAndParseFiles)
	      val suffix = withFiles (libFile "suffix", lexAndParseFiles)
	    in
	      {name = name,
	       bind = bind,
	       prefix = prefix,
	       suffix = suffix}
	    end
       in
	  {build = build,
	   localTopFinish = localTopFinish,
	   libs = List.map (Control.basisLibs, doit)}
       end
end

val basisLibrary = Promise.lazy basisLibrary
    
fun forceBasisLibrary d =
   (setBasisLibraryDir d
    ; basisLibrary ()
    ; ())

val primitiveDecs: CoreML.Dec.t vector =
   let
      open CoreML.Dec
   in
      Vector.concat [Vector.new1 (Datatype primitiveDatatypes),
		     Vector.fromListMap
		     (primitiveExcons, fn c =>
		      Exception {con = c, arg = NONE})]
   end

fun outputBasisConstants (out: Out.t): unit =
   let
      val _ = amBuildingConstants := true
      val {build, ...} = basisLibrary ()
      (* Need to defunctorize so the constants are forced. *)
      val _ =
	 Defunctorize.defunctorize
	 (CoreML.Program.T {decs = Vector.concat [primitiveDecs,
						  Decs.toVector build]})
      val _ = LookupConstant.build (!allConstants, out)
   in
      ()
   end

fun lookupConstantError _ = Error.bug "const in user input"

fun selectBasisLibrary () =
   let
     val {build, localTopFinish, libs} = basisLibrary ()
     val lib = !Control.basisLibrary
   in
      case List.peek (libs, fn {name, ...} => name = lib) of
	 NONE => Error.bug (concat ["Missing basis library: ", lib])
       | SOME {bind, prefix, suffix, ...} =>
	   let
	     val (bind, prefix, suffix) = 
	        localTopFinish 
		(fn () =>
		 (elaborateProg (bind, basisEnv, lookupConstantError),
		  elaborateProg (prefix, basisEnv, lookupConstantError),
		  elaborateProg (suffix, basisEnv, lookupConstantError)))
	   in
	     {basis = Decs.append (build, bind),
	      prefix = prefix,
	      suffix = suffix}
	   end
   end

fun layoutBasisLibrary () = 
   let val _ = selectBasisLibrary ()
   in Env.layoutPretty basisEnv
   end

(* ------------------------------------------------- *)
(*                      compile                      *)
(* ------------------------------------------------- *)

fun elaborate {input: File.t list} =
   let
      fun parseElabMsg () = (lexAndParseMsg (); elaborateMsg ())
      val decs =
	 let 
	    val {basis, prefix, suffix, ...} = selectBasisLibrary ()
	    val _ = Elaborate.allowRebindEquals := false
	    fun parseAndElab () =
	       parseAndElaborateFiles (input, basisEnv, lookupConstantError)
	    val input =
	       if !Control.showBasisUsed
		  then let
			  val input =
			     Elaborate.Env.scopeAll (basisEnv, parseAndElab)
			  val _ =
			     Layout.outputl
			     (Elaborate.Env.layoutUsed basisEnv,
			      Out.standard)
		       in
			  Process.succeed ()
		       end
	       else
		  parseAndElab () 
	    val _ =
	       if not (!Control.exportHeader)
		  then ()
	       else 
		  let
		     val _ =
			File.outputContents
			(concat [!Control.libDir, "/include/types.h"],
			 Out.standard)
		     val _ = print "\n"
		     val _ = Ffi.declareHeaders {print = print}
		  in
		     Process.succeed ()
		  end
	    val user = Decs.appends [prefix, input, suffix]
	    val _ = parseElabMsg ()
	    val basis = Decs.toList basis
	    val user = Decs.toList user
 	    val basis = 
 	       Control.pass
 	       {name = "deadCode",
 		suffix = "basis",
 		style = Control.ML,
 		thunk = fn () => DeadCode.deadCode {basis = basis,
 						    user = user},
 		display = Control.Layout (List.layout CoreML.Dec.layout)}
	 in
	    Vector.concat [primitiveDecs,
			   Vector.fromList basis,
			   Vector.fromList user]
	 end
      val coreML = CoreML.Program.T {decs = decs}
(*       val _ = Control.message (Control.Detail, fn () =>
 * 			       CoreML.Program.layoutStats coreML)
 *)
      (* Set GC_state offsets. *)
      val _ =
	 let
	    fun get (s: string): int =
	       case lookupConstant (s, ConstType.Int) of
		  Const.Int i => IntX.toInt i
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
      val _ =
	 let
	    open Control
	 in
	    if !keepSSA
	       then saveToFile ({suffix = "ssa"}, No, ssa,
				 Layouts Ssa.Program.layouts)
	    else ()
	 end
      val machine =
	 Control.pass
	 {name = "backend",
	  suffix = "machine",
	  style = Control.No,
	  thunk = fn () => Backend.toMachine ssa,
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
	 Control.trace (Control.Pass, "machine type check")
	 Machine.Program.typeCheck machine
   in
      machine
   end

fun typeCheck {input: File.t list}: unit =
   (elaborate {input = input}; ())
   
fun compile {input: File.t list, outputC, outputS}: unit =
   let
      val machine =
	 Control.trace (Control.Top, "pre codegen")
	 preCodegen {input = input}
      val _ =
	 if !Control.Native.native
	    then
	       Control.trace (Control.Top, "x86 code gen")
	       x86Codegen.output {program = machine,
				  outputC = outputC,
				  outputS = outputS}
	 else
	    Control.trace (Control.Top, "C code gen")
	    CCodegen.output {program = machine,
			     outputC = outputC}
      val _ = Control.message (Control.Detail, PropertyList.stats)
      val _ = Control.message (Control.Detail, HashSet.stats)
   in
      ()
   end
   
end
