(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Compile: COMPILE =
struct

(*---------------------------------------------------*)
(*              Intermediate Languages               *)
(*---------------------------------------------------*)
   
structure Ast = Ast ()
local
   open Ast.Tycon
in
   structure IntSize = IntSize
   structure RealSize = RealSize
   structure WordSize = WordSize
end
structure Atoms = Atoms (structure Ast = Ast
			 structure IntSize = IntSize
			 structure RealSize = RealSize
			 structure WordSize = WordSize)
local
   open Atoms
in
   structure Const = Const
   structure IntX = IntX
end
structure CoreML = CoreML (open Atoms
			   structure Type = Prim.Type)
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
structure Elaborate = Elaborate (structure Ast = Ast
				 structure CoreML = CoreML)
structure LookupConstant = LookupConstant (structure CoreML = CoreML)
structure Infer = Infer (structure CoreML = CoreML
			 structure LookupConstant = LookupConstant
			 structure Xml = Xml)
local
   open Infer
in
   structure BuildConst = BuildConst
end
structure Monomorphise = Monomorphise (structure Xml = Xml
				       structure Sxml = Sxml)
structure ClosureConvert = ClosureConvert (structure Ssa = Ssa
					   structure Sxml = Sxml)
structure Backend = Backend (structure Ssa = Ssa
			     structure Machine = Machine
			     fun funcToLabel f = f)
structure CCodegen = CCodegen (structure Machine = Machine)
structure x86Codegen = x86Codegen (structure CCodegen = CCodegen
				   structure Machine = Machine)

local open Elaborate
in 
   structure Decs = Decs
   structure Env = Env
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

fun elaborateProg (ast: Ast.Program.t, E: Env.t): Decs.t =
   let
      val decs = elaborate (ast, E)
      val _ = Control.checkForErrors "elaborate"
   in decs
   end

val displayDecs =
   Control.Layout
   (fn ds => CoreML.Program.layout (CoreML.Program.T
				    {decs = Decs.toVector ds}))
   
fun parseAndElaborateFiles (fs: File.t list, E: Env.t): Decs.t =
   Control.pass
   {name = "parseAndElaborate",
    suffix = "core-ml",
    style = Control.ML,
    thunk = fn () => List.fold (fs, Decs.empty, fn (f, ds) =>
				Decs.append 
				(ds, elaborateProg (lexAndParseFile f, E))),
    display = displayDecs}

(* ------------------------------------------------- *)   
(*                   Primitive Env                   *)
(* ------------------------------------------------- *)

local
   open CoreML
   open Scheme Type
in
   val primitiveDatatypes =
      Vector.new3
      ({tycon = Tycon.bool,
	tyvars = Vector.new0 (),
	cons = Vector.new2 ({con = Con.falsee, arg = NONE},
			    {con = Con.truee, arg = NONE})},
       let val a = Tyvar.newNoname {equality = false}
       in {tycon = Tycon.list,
	   tyvars = Vector.new1 a,
	   cons = Vector.new2 ({con = Con.nill, arg = NONE},
			       {con = Con.cons,
				arg = SOME (tuple
					    (Vector.new2
					     (var a, list (var a))))})}
       end,
       let val a = Tyvar.newNoname {equality = false}
       in {tycon = Tycon.reff,
	   tyvars = Vector.new1 a,
	   cons = Vector.new1 {con = Con.reff, arg = SOME (var a)}}
       end)
end

val primitiveExcons =
   let open CoreML.Con
   in [bind, match, overflow]
   end

structure Env =
   struct
      open Env 

      fun addPrim (E: t): unit =
	 let
	    open CoreML
	    val _ =
	       List.foreach
	       (Tycon.prims, fn tycon =>
		extendTycon
		(E, Ast.Tycon.fromString (Tycon.originalName tycon,
					  Region.bogus),
		 TypeStr.tycon tycon))
	    val _ =
	       Vector.foreach
	       (primitiveDatatypes, fn {tyvars, tycon, cons} =>
		let
		   val cs =
		      Vector.map
		      (cons, fn {con, arg} =>
		       let
			  val resultType =
			     Type.con (tycon, Vector.map (tyvars, Type.var))
		       in {name = Con.toAst con,
			   con = con}
		       end)
		   val _ =
		      Vector.foreach (cs, fn {name, con} =>
				      extendCon (E, name, con))
		in extendTycon (E, Tycon.toAst tycon,
				TypeStr.data (tycon, cs))
		end)
	    val _ = extendTycon (E, Ast.Tycon.fromString ("unit", Region.bogus),
				 TypeStr.def (Scheme.make0 Type.unit))
	    val _ = addEquals E
	    val _ = List.foreach (primitiveExcons, fn c =>
				  extendCon (E, Con.toAst c, c))
	 in ()
	 end
   end

(* ------------------------------------------------- *)
(*                   Basis Library                   *)
(* ------------------------------------------------- *)

val basisEnv = Env.empty ()

local
   val dir = ref NONE
in
   fun setBasisLibraryDir (d: Dir.t): unit =
      dir := SOME d
   val basisLibrary : unit -> {build: Decs.t,
			       localTopFinish: (unit -> Decs.t * Decs.t * Decs.t) -> 
			                       Decs.t * Decs.t * Decs.t,
			       libs: {name: string,
				      bind: Ast.Program.t,
				      prefix: Ast.Program.t,
				      suffix: Ast.Program.t} list} =
      Promise.lazy
      (fn () =>
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
	      fn () => (Env.addPrim basisEnv
			; withFiles (libsFile "build", 
				     fn fs => parseAndElaborateFiles (fs, basisEnv))))
	  val localTopFinish = fn g =>
	     (localTopFinish g) before (Env.addEquals basisEnv
					; Env.clean basisEnv)

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
       end)
end

fun forceBasisLibrary d =
   (setBasisLibraryDir d
    ; basisLibrary ()
    ; ())
   
fun buildDecs () =
   let
      val {build, ...} = basisLibrary ()
   in
      Decs.toVector build
   end
   
fun outputBasisConstants (out: Out.t): unit =
   LookupConstant.build (buildDecs (), out)

fun selectBasisLibrary () =
   let
     val {build, localTopFinish, libs} = basisLibrary ()
     val lib = !Control.basisLibrary
   in
      case List.peek (libs, fn {name, ...} => name = lib) of
	 NONE => Error.bug ("Missing basis library: " ^ lib)
       | SOME {bind, prefix, suffix, ...} =>
	   let
	     val (bind, prefix, suffix) = 
	        localTopFinish 
		(fn () => (elaborateProg (bind, basisEnv),
			   elaborateProg (prefix, basisEnv),
			   elaborateProg (suffix, basisEnv)))
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

fun preCodegen {input, docc}: Machine.Program.t =
   let
      fun parseElabMsg () = (lexAndParseMsg (); elaborateMsg ())
      val primitiveDecs: CoreML.Dec.t vector =
	 let
	    open CoreML.Dec
	    fun make n = makeRegion (n, Region.bogus)
	 in
	    Vector.concat [Vector.new1 (make (Datatype primitiveDatatypes)),
			   Vector.fromListMap
			   (primitiveExcons, fn c =>
			    make (Exception {con = c, arg = NONE}))]
	 end
      val decs =
	 let 
	    val {basis, prefix, suffix, ...} = selectBasisLibrary ()
	    val input =
	       if !Control.showBasisUsed
		  then let
			  val input =
			     Elaborate.Env.scopeAll
			     (basisEnv, fn () =>
			      parseAndElaborateFiles (input, basisEnv))
			  val _ =
			     Layout.outputl
			     (Elaborate.Env.layoutUsed basisEnv,
			      Out.standard)
		       in
			 Process.succeed ()
		       end
	       else parseAndElaborateFiles (input, basisEnv)
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
	 in Vector.concat [primitiveDecs,
			   Vector.fromList basis,
			   Vector.fromList user]
	 end
      val coreML = CoreML.Program.T {decs = decs}
      val _ = Control.message (Control.Detail, fn () =>
			       CoreML.Program.layoutStats coreML)
      val buildConstants =
	 let
	    val bool = BuildConst.Bool
	    val int = BuildConst.Int
	    open Control
	 in
	    [("Exn_keepHistory", bool (!exnHistory)),
	     ("MLton_detectOverflow", bool (!detectOverflow)),
	     ("MLton_native", bool (!Native.native)),
	     ("MLton_profile_isOn", bool (!profile <> ProfileNone)),
	     ("MLton_safe", bool (!safe)),
	     ("TextIO_bufSize", int (!textIOBufSize))]
	 end
      fun lookupBuildConstant (c: string) =
	 case List.peek (buildConstants, fn (c', _) => c = c') of
	    NONE => Error.bug (concat ["strange build constant: ", c])
	  | SOME (_, v) => v
      val lookupConstant =
	 File.withIn
	 (concat [!Control.libDir, "/constants"], fn ins =>
	  LookupConstant.load (buildDecs (), ins))
      (* Set GC_state offsets. *)
      val _ =
	 let
	    fun get s =
	       case lookupConstant s of
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
	 Control.passSimplify
	 {name = "infer",
	  suffix = "xml",
	  style = Control.ML,
	  thunk = fn () => (Infer.infer
			    {program = coreML,
			     lookupBuildConstant = lookupBuildConstant,
			     lookupConstant = lookupConstant}),
	  display = Control.Layout Xml.Program.layout,
	  typeCheck = Xml.typeCheck,
	  simplify = Xml.simplify}
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

fun compile {input: File.t list, outputC, outputS, docc}: unit =
   let
      val machine =
	 Control.trace (Control.Top, "pre codegen")
	 preCodegen {input = input, docc = docc}
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
   in ()
   end
   
end
