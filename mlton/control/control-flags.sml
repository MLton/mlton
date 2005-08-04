(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
   
val basisLibs = ["basis-2002", "basis-1997", "basis-none"]
   
val basisLibrary = control {name = "basis library",
			    default = "basis-2002",
			    toString = fn s => s}

val build = concat ["(built ", Date.toString (Date.now ()),
		    " on ", Process.hostName (), ")"]

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

structure Codegen =
   struct
      datatype t =
	 Bytecode
       | CCodegen
       | Native

      val toString: t -> string =
	 fn Bytecode => "Bytecode"
	  | CCodegen => "C"
	  | Native => "Native"
   end

datatype codegen = datatype Codegen.t
   
val codegen = control {name = "codegen",
		       default = Native,
		       toString = Codegen.toString}

val contifyIntoMain = control {name = "contifyIntoMain",
			       default = false,
			       toString = Bool.toString}

val debug = control {name = "debug",
		     default = false,
		     toString = Bool.toString}

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

      structure Id =
	 struct
	    datatype t = T of {enabled: bool ref,
			       expert: bool,
			       name: string}
	    fun equals (T {enabled = enabled1, ...}, 
			T {enabled = enabled2, ...}) = 
	       enabled1 = enabled2

	    val enabled = fn (T {enabled, ...}) => !enabled
	    val setEnabled = fn (T {enabled, expert, ...}, b) =>
	       if expert
		  then false
		  else (enabled := b; true)
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

      local 
	 fun make ({default: 'st,
		    expert: bool,
		    toString: 'st -> string,
		    name: string,
		    newCur: 'st * 'args -> 'st,
		    newDef: 'st * 'args -> 'st,
		    parseArgs: string list -> 'args option},
		   {parseId: string -> Id.t option,
		    parseIdAndArgs: string -> (Id.t * Args.t) option,
		    withDef: unit -> (unit -> unit),
		    snapshot: unit -> unit -> (unit -> unit)}) =
	    let
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
		     then SOME id 
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
					  if expert
					     then false
					     else let
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
				       SOME (id, args)
				    end
			       | NONE => NONE
			   else parseIdAndArgs s
		   | _ => NONE
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
	    make ({default = default,
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

	 fun makeDiagnostic ({default,
			      diagToString,
			      diagFromString,
			      expert: bool,
			      name: string}, ac) =
	     make ({default = default,
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
	    makeDiagnostic ({default = default,
			     diagToString = DiagEIW.toString,
			     diagFromString = DiagEIW.fromString,
			     expert = expert,
			     name = name}, ac)
	 fun makeDiagDI ({default: DiagDI.t,
			  expert: bool,
			  name: string}, ac) =
	    makeDiagnostic ({default = default,
			     diagToString = DiagDI.toString,
			     diagFromString = DiagDI.fromString,
			     expert = expert,
			     name = name}, ac)

	fun setCur (T {cur, ...}, x) = cur := x
	fun setDef (T {def, ...}, x) = def := x
      in
	 val ac =
	    {parseId = fn _ => NONE,
	     parseIdAndArgs = fn _ => NONE,
	     withDef = fn () => (fn () => ()),
	     snapshot = fn () => fn () => (fn () => ())}
	 val (allowConstant, ac) =
	    makeBool ({name = "allowConstant", 
		       default = false, expert = true}, ac)
	 val (allowExport, ac) =
	    makeBool ({name = "allowExport", 
		       default = false, expert = false}, ac)
	 val (allowImport, ac) =
	    makeBool ({name = "allowImport", 
		       default = false, expert = false}, ac)
	 val (allowPrim, ac) =
	    makeBool ({name = "allowPrim", 
		       default = false, expert = true}, ac)
	 val (allowOverload, ac) =
	    makeBool ({name = "allowOverload", 
		       default = false, expert = false}, ac)
	 val (allowRebindEquals, ac) =
	    makeBool ({name = "allowRebindEquals", 
		       default = false, expert = true}, ac)
	 val (allowSymbol, ac) =
	    makeBool ({name = "allowSymbol", 
		       default = false, expert = false}, ac)
	 val (deadCode, ac) =
	    makeBool ({name = "deadCode", 
		       default = false, expert = false}, ac)
	 val (allowFFI, ac) =
	    make ({default = false,
	           expert = false,
	           toString = Bool.toString,
	           name = "allowFFI",
	           newCur = fn (_, b) => (setCur (allowExport, b)
	          			  ; setCur (allowImport, b)
	          			  ; setCur (allowSymbol, b)
	          			  ; b),
	           newDef = fn (_, b) => (setDef (allowExport, b)
	          			  ; setDef (allowImport, b)
	          			  ; setDef (allowSymbol, b)
	          			  ; b),
		   parseArgs = fn args' =>
		               case args' of
				  [arg'] => Bool.fromString arg'
				| _ => NONE}, 
		  ac)
	 val (forceUsed, ac) =
	    make ({default = false,
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
	    make ({default = NONE,
		   expert = true,
		   toString = Option.toString String.toString,
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
	 val (sequenceUnit, ac) =
	    makeBool ({name = "sequenceUnit", 
		       default = false, expert = false}, ac)
	 val (warnMatch, ac) =
	    make ({default = true,
		   expert = false,
		   toString = Bool.toString,
		   name = "warnMatch",
		   newCur = fn (_, b) =>
			       let
				  val d = if b 
					     then DiagEIW.Warn 
					     else DiagEIW.Ignore
			       in
				  setCur (nonexhaustiveMatch, d)
				  ; setCur (redundantMatch, d)
				  ; b
			       end,
		   newDef = fn (_, b) =>
			       let
				  val d = if b 
					     then DiagEIW.Warn 
					     else DiagEIW.Ignore
			       in
				  setDef (nonexhaustiveMatch, d)
				  ; setDef (redundantMatch, d)
				  ; b
			       end,
		   parseArgs = fn [arg'] => Bool.fromString arg'
				| _ => NONE}, 
		  ac)
	 val (warnUnused, ac) =
	    makeBool ({name = "warnUnused", 
		       default = false, expert = false}, ac)
	 val {parseId, parseIdAndArgs, withDef, snapshot} = ac
      end

      val processDefault = fn s =>
	 case parseIdAndArgs s of
	    SOME (_, args) => Args.processDef args
	  | NONE => false
      val processEnabled = fn (s, b) =>
	 case parseId s of
	    SOME id => Id.setEnabled (id, b)
	  | NONE => false

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

val exportHeader =
   control {name = "export header",
	    default = NONE,
	    toString = Option.toString File.toString}
   
val exnHistory = control {name = "exn history",
			  default = false,
			  toString = Bool.toString}
   
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

structure Inline =
   struct
      datatype t =
	 NonRecursive of {product: int,
			  small: int}
       | Leaf of {size: int option}
       | LeafNoLoop of {size: int option}

      local open Layout
	 val iol = Option.layout Int.layout
      in
	 val layout = 
	    fn NonRecursive {product, small} =>
	    seq [str "NonRecursive ",
		record [("product", Int.layout product),
		       ("small", Int.layout small)]]
	     | Leaf {size} => seq [str "Leaf ", iol size]
	     | LeafNoLoop {size} => seq [str "LeafNoLoop ", iol size]
      end
      val toString = Layout.toString o layout
   end

datatype inline = datatype Inline.t

val inline = control {name = "inline",
		      default = NonRecursive {product = 320,
					      small = 60},
		      toString = Inline.toString}

fun setInlineSize (size: int): unit =
   inline := (case !inline of
		 NonRecursive {small, ...} =>
		    NonRecursive {product = size, small = small}
	       | Leaf _ => Leaf {size = SOME size}
	       | LeafNoLoop _ => LeafNoLoop {size = SOME size})

val inlineIntoMain = control {name = "inlineIntoMain",
			      default = true,
			      toString = Bool.toString}

val inputFile = control {name = "input file",
			 default = "<bogus>",
			 toString = File.toString}

val keepMachine = control {name = "keep Machine",
			   default = false,
			   toString = Bool.toString}
   
val keepRSSA = control {name = "keep RSSA",
			default = false,
			toString = Bool.toString}

val keepSSA = control {name = "keep SSA",
		       default = false,
		       toString = Bool.toString}

val keepSSA2 = control {name = "keep SSA2",
			default = false,
			toString = Bool.toString}

val keepDot = control {name = "keep dot",
		       default = false,
		       toString = Bool.toString}

val keepPasses = control {name = "keep passes",
			  default = [],
			  toString = List.toString 
			             (Layout.toString o 
				      Regexp.Compiled.layout)}

val labelsHaveExtra_ = control {name = "extra_",
				default = false,
				toString = Bool.toString}

val libDir = control {name = "lib dir",
		      default = "<libDir unset>",
		      toString = fn s => s}

val libTargetDir = control {name = "lib target dir",
			    default = "<libTargetDir unset>",
			    toString = fn s => s} 
   
val loopPasses = control {name = "loop passes",
			  default = 1,
			  toString = Int.toString}

val markCards = control {name = "mark cards",
			 default = true,
			 toString = Bool.toString}

val maxFunctionSize = control {name = "max function size",
			       default = 10000,
			       toString = Int.toString}
   
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

structure OptimizationPasses =
   struct
      datatype t = 
	 OptPassesCustom of string
       | OptPassesDefault
       | OptPassesMinimal

      local open Layout
      in
	 val layout =
	    fn OptPassesCustom s => seq [str "Limit: ", str s]
	     | OptPassesDefault => str "Default"
	     | OptPassesMinimal => str "Minimal"
      end
      val toString = Layout.toString o layout
   end
datatype optimizationPasses = datatype OptimizationPasses.t
val optimizationPassesSet : 
   (string * (optimizationPasses -> unit Result.t)) list ref =
   control {name = "optimizationPassesSet",
	    default = [],
	    toString = List.toString 
	               (fn (s,_) => concat ["<",s,"PassesSet>"])}

val polyvariance =
   control {name = "polyvariance",
	    default = SOME {rounds = 2,
			    small = 30,
			    product = 300},
	    toString =
	    fn p =>
	    Layout.toString
	    (Option.layout
	     (fn {rounds, small, product} =>
	      Layout.record [("rounds", Int.layout rounds),
			     ("small", Int.layout small),
			     ("product", Int.layout product)])
	     p)}

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
       | ProfileTime

      val toString =
	 fn ProfileNone => "None"
	  | ProfileAlloc => "Alloc"
	  | ProfileCallStack => "CallStack"
	  | ProfileCount => "Count"
	  | ProfileDrop => "Drop"
	  | ProfileLabel => "Label"
	  | ProfileTime => "Time"
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
      datatype t = ProfileSSA | ProfileSSA2 | ProfileSource

      val toString =
	 fn ProfileSSA => "ProfileSSA"
	  | ProfileSSA2 => "ProfileSSA2"
	  | ProfileSource => "ProfileSource"
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

val showBasis = control {name = "show basis",
			 default = NONE,
			 toString = Option.toString File.toString}
   
val showDefUse = control {name = "show def-use",
			  default = NONE,
			  toString = Option.toString File.toString}

val showTypes = control {name = "show types",
			 default = false,
			 toString = Bool.toString}

val ssaPassesSet : (optimizationPasses -> unit Result.t) ref = 
   control {name = "ssaPassesSet",
	    default = fn _ => Error.bug ("ControlFlags.ssaPassesSet: not installed"),
	    toString = fn _ => "<ssaPassesSet>"}
val ssaPasses : string list ref = 
   control {name = "ssaPasses",
	    default = ["default"],
	    toString = List.toString String.toString}
val ssa2PassesSet : (optimizationPasses -> unit Result.t) ref = 
   control {name = "ssa2PassesSet",
	    default = fn _ => Error.bug ("ControlFlags.ssa2PassesSet: not installed"),
	    toString = fn _ => "<ssa2PassesSet>"}
val ssa2Passes : string list ref = 
   control {name = "ssa2Passes",
	    default = ["default"],
	    toString = List.toString String.toString}

val sxmlPassesSet : (optimizationPasses -> unit Result.t) ref = 
   control {name = "sxmlPassesSet",
	    default = fn _ => Error.bug ("ControlFlags.sxmlPassesSet: not installed"),
	    toString = fn _ => "<sxmlPassesSet>"}
val sxmlPasses : string list ref = 
   control {name = "sxmlPasses",
	    default = ["default"],
	    toString = List.toString String.toString}

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

datatype arch = datatype MLton.Platform.Arch.t

val targetArch = control {name = "target arch",
			  default = X86,
			  toString = MLton.Platform.Arch.toString}

local
   val r: bool option ref = ref NONE
in
   fun setTargetBigEndian b = r := SOME b
   fun targetIsBigEndian () =
      case !r of
	 NONE => Error.bug "ControlFlags.targetIsBigEndian: not set"
       | SOME b => b
end

datatype os = datatype MLton.Platform.OS.t

val targetOS = control {name = "target OS",
			default = Linux,
			toString = MLton.Platform.OS.toString}

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

val version = "MLton MLTONVERSION"

val warnAnn = control {name = "warn unrecognized annotation",
		       default = true,
		       toString = Bool.toString}

val xmlPassesSet: (optimizationPasses -> unit Result.t) ref = 
   control {name = "xmlPassesSet",
	    default = fn _ => Error.bug ("ControlFlags.xmlPassesSet: not installed"),
	    toString = fn _ => "<xmlPassesSet>"}
val xmlPasses: string list ref = 
   control {name = "xmlPasses",
	    default = ["default"],
	    toString = List.toString String.toString}

val zoneCutDepth: int ref =
   control {name = "zone cut depth",
	    default = 100,
	    toString = Int.toString}

val defaults = setDefaults

val _ = defaults ()

end
