(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Control: CONTROL =
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
      in
	 val ac =
	    {parseId = fn _ => NONE,
	     parseIdAndArgs = fn _ => NONE,
	     withDef = fn () => (fn () => ()),
	     snapshot = fn () => fn () => (fn () => ())}
	 val (allowConstant, ac) =
	    makeBool ({name = "allowConstant", default = false, expert = true}, ac)
	 val (allowExport, ac) =
	    makeBool ({name = "allowExport", default = false, expert = false}, ac)
	 val (allowImport, ac) =
	    makeBool ({name = "allowImport", default = false, expert = false}, ac)
	 val (allowPrim, ac) =
	    makeBool ({name = "allowPrim", default = false, expert = true}, ac)
	 val (allowOverload, ac) =
	    makeBool ({name = "allowOverload", default = false, expert = false}, ac)
	 val (allowRebindEquals, ac) =
	    makeBool ({name = "allowRebindEquals", default = false, expert = true}, ac)
	 val (deadCode, ac) =
	    makeBool ({name = "deadCode", default = false, expert = false}, ac)
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
	 val (sequenceUnit, ac) =
	    makeBool ({name = "sequenceUnit", default = false, expert = false}, ac)
	 val (warnMatch, ac) =
	    makeBool ({name = "warnMatch", default = true, expert = false}, ac)
	 val (warnUnused, ac) =
	    makeBool ({name = "warnUnused", default = false, expert = false}, ac)
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
	 let val restore = withDef ()
	 in DynamicWind.wind (f, restore)
	 end
      val snapshot : unit -> (unit -> 'a) -> 'a = fn () =>
	 let val withSaved = snapshot () in fn f =>
	 let val restore = withSaved ()
	 in DynamicWind.wind (f, restore)
	 end end

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
       | ProfileMark
       | ProfileTime

      val toString =
	 fn ProfileNone => "None"
	  | ProfileAlloc => "Alloc"
	  | ProfileCallStack => "CallStack"
	  | ProfileCount => "Count"
	  | ProfileMark => "Mark"
	  | ProfileTime => "Time"
   end

datatype profile = datatype Profile.t
   
val profile = control {name = "profile",
		       default = ProfileNone,
		       toString = Profile.toString}

val profileBasis = control {name = "profile basis",
			    default = false,
			    toString = Bool.toString}

val profileBranch = control {name = "profile branch",
			     default = false,
			     toString = Bool.toString}

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

val ssaPassesSet : (string -> string list Result.t) ref = 
   control {name = "ssaPassesSet",
	    default = fn _ => Error.bug ("ssaPassesSet not installed"),
	    toString = fn _ => "<ssaPassesSet>"}
val ssaPasses : string list ref = 
   control {name = "ssaPasses",
	    default = ["default"],
	    toString = List.toString String.toString}
val ssa2PassesSet : (string -> string list Result.t) ref = 
   control {name = "ssa2PassesSet",
	    default = fn _ => Error.bug ("ssa2PassesSet not installed"),
	    toString = fn _ => "<ssa2PassesSet>"}
val ssa2Passes : string list ref = 
   control {name = "ssa2Passes",
	    default = ["default"],
	    toString = List.toString String.toString}

val sxmlPassesSet : (string -> string list Result.t) ref = 
   control {name = "sxmlPassesSet",
	    default = fn _ => Error.bug ("sxmlPassesSet not installed"),
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
	 NONE => Error.bug "targetIsBigEndian not set"
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

      val op <= =
	 fn (Silent, _) => true
	  | (Top, Silent) => false
	  | (Top, _) => true
	  | (Pass, Pass) => true
	  | (_, Detail) => true
	  | _ => false
   end

datatype verbosity = datatype Verbosity.t
   
val verbosity = control {name = "verbosity",
			 default = Silent,
			 toString = Verbosity.toString}

val version = "MLton MLTONVERSION"

val warnAnn = control {name = "warn unrecognized annotation",
		       default = true,
		       toString = Bool.toString}

val xmlPassesSet: (string -> string list Result.t) ref = 
   control {name = "xmlPassesSet",
	    default = fn _ => Error.bug ("xmlPassesSet not installed"),
	    toString = fn _ => "<xmlPassesSet>"}
val xmlPasses: string list ref = 
   control {name = "xmlPasses",
	    default = ["default"],
	    toString = List.toString String.toString}

val zoneCutDepth: int ref =
   control {name = "zone cut depth",
	    default = 100,
	    toString = Int.toString}
   
datatype style = No | Assembly | C | Dot | ML

fun preSuf style =
   let
      val (p, s) =
	 case style of
	    No => ("", "")
	  | Assembly => ("/* ", " */")
	  | C => ("/* ", " */")
	  | Dot => ("// ", "")
	  | ML => ("(* ", " *)")
   in (p, s)
   end

val build = concat ["(built ", Date.toString (Date.now ()),
		    " on ", Process.hostName (), ")"]

fun outputHeader (style: style, output: Layout.t -> unit) =
   let
      val (pre, suf) = preSuf style
      val lines =
	 concat [version, " ", build]
	 :: concat ["  created this file on ", Date.toString (Date.now ()), "."]
	 :: "Do not edit this file."
	 :: "Flag settings: "
	 :: (List.map (all (), fn {name, value} =>
		       concat ["   ", name, ": ", value]))
   in List.foreach (lines, fn l => output (Layout.str (concat [pre, l, suf])))
   end

fun outputHeader' (style, out: Out.t) =
   outputHeader (style, fn l =>
		 (Layout.output (l, out);
		  Out.newline out))

val depth: int ref = ref 0
fun getDepth () = !depth
fun indent () = depth := !depth + 3
fun unindent () = depth := !depth - 3

fun message (verb: Verbosity.t, th: unit -> Layout.t): unit =
   if Verbosity.<= (verb, !verbosity)
      then let val out = Out.error
	   in Layout.output (Layout.indent (th (), !depth), out)
	      ; Out.newline out
	   end
   else ()
      
fun messageStr (verb, s: string): unit =
   message (verb, fn () => Layout.str s)

val defaults = setDefaults

val _ = defaults ()

fun time () =
   let
      open Time
      val {children, self, gc, ...} = times ()
      fun add {utime, stime} = utime + stime
   in
      (add self + add children, add gc)
   end

fun timeToString {total, gc} =
   let
      fun fmt (x, n) = Real.format (x, Real.Format.fix (SOME n))
      val toReal = Real.fromIntInf o Time.toMilliseconds
      val per =
	 if Time.equals (total, Time.zero)
	    then "0"
	 else fmt (100.0 * (toReal gc / toReal total), 0)
      fun t2s t =
	 fmt (Real./ (toReal t, 1000.0), 2)
   in concat [t2s (Time.- (total, gc)), " + ", t2s gc, " (", per, "% GC)"]
   end

fun trace (verb, name: string) (f: 'a -> 'b) (a: 'a): 'b =
   if Verbosity.<= (verb, !verbosity)
      then
	 let
	    val _ = messageStr (verb, concat [name, " starting"])
	    val (t, gc) = time ()
	    val _ = indent ()
	    fun done () =
	       let
		  val _ = unindent ()
		  val (t', gc') = time ()
	       in
		  timeToString {total = Time.- (t', t),
				gc = Time.- (gc', gc)}
	       end
	 in (f a
	     before messageStr (verb, concat [name, " finished in ", done ()]))
	    handle e =>
	       (messageStr (verb, concat [name, " raised in ", done ()])
		; (case Exn.history e of
		      [] => ()
		    | history =>
			 (messageStr (verb, concat [name, " raised with history: "])
			  ; (List.foreach
			     (history, fn s =>
			      messageStr (verb, concat ["\t", s])))))
		; raise e)
	 end
   else
      f a

type traceAccum = {verb: verbosity, 
		   total: Time.t ref, 
		   totalGC: Time.t ref}

val traceAccum: (verbosity * string) -> (traceAccum * (unit -> unit)) =
   fn (verb, name) =>
   let
     val total = ref Time.zero
     val totalGC = ref Time.zero
   in
     ({verb = verb, total = total, totalGC = totalGC},
      fn () => messageStr (verb,
			   concat [name, 
				   " totals ",
				   timeToString
				   {total = !total,
				    gc = !totalGC}]))
   end

val ('a, 'b) traceAdd: (traceAccum * string) -> ('a -> 'b) -> 'a -> 'b =
   fn ({verb, total, totalGC}, name) =>
   fn f =>
   fn a =>
   if Verbosity.<= (verb, !verbosity)
     then let
	    val (t, gc) = time ()
	    fun done () 
	      = let
		  val (t', gc') = time ()
		in
		  total := Time.+ (!total, Time.- (t', t))
		  ; totalGC := Time.+ (!totalGC, Time.- (gc', gc))
		end
	  in
	    (f a
	     before done ())
	    handle e => 
	       (messageStr (verb, concat [name, " raised"])
		; (case Exn.history e of
		      [] => ()
		    | history =>
			 (messageStr (verb, concat [name, " raised with history: "])
			  ; (List.foreach
			     (history, fn s =>
			      messageStr (verb, concat ["\t", s])))))
		; raise e)
	  end
     else f a

val ('a, 'b) traceBatch: (verbosity * string) -> ('a -> 'b) ->
                         (('a -> 'b) * (unit -> unit)) =
   fn (verb, name) =>
   let
     val (ta,taMsg) = traceAccum (verb, name)
   in
     fn f =>
     (traceAdd (ta,name) f, taMsg)
   end

(*------------------------------------*)
(*               Errors               *)
(*------------------------------------*)

val numErrors: int ref = ref 0

val errorThreshhold: int ref = ref 20

val die = Process.fail

local
   fun msg (kind: string, r: Region.t, msg: Layout.t, extra: Layout.t): unit =
      let
	 open Layout
	 val p =
	    case Region.left r of
	       NONE => "<bogus>"
	     | SOME p => SourcePos.toString p
	 val msg = Layout.toString msg
	 val msg =
	    Layout.str
	    (concat [String.fromChar (Char.toUpper (String.sub (msg, 0))),
		     String.dropPrefix (msg, 1),
		     "."])
	 in
	    outputl (align [seq [str (concat [kind, ": "]), str p, str "."],
			    indent (align [msg,
					   indent (extra, 2)],
				    2)],
		     Out.error)
      end
in
   fun warning (r, m, e) = msg ("Warning", r, m, e)
   fun error (r, m, e) =
      let
	 val _ = Int.inc numErrors
	 val _ = msg ("Error", r, m, e)
      in
	 if !numErrors = !errorThreshhold
	    then die "compilation aborted: too many errors"
	 else ()
      end
end

fun errorStr (r, msg) = error (r, Layout.str msg, Layout.empty)

fun checkForErrors (name: string) =
   if !numErrors > 0
      then die (concat ["compilation aborted: ", name, " reported errors"])
   else ()

fun checkFile (f: File.t, error: string -> 'a, k: unit -> 'a): 'a =
   let
      fun check (test, msg, k) =
	 if not (test f)
	    then error (concat ["File ", f, " ", msg])
	 else k ()
   in
      check (File.doesExist, "does not exist", fn () =>
	     check (File.canRead, "cannot be read", k))
   end

(*---------------------------------------------------*)
(*                  Compiler Passes                  *)
(*---------------------------------------------------*)

datatype 'a display =
   NoDisplay
  | Layout of 'a -> Layout.t
  | Layouts of 'a * (Layout.t -> unit) -> unit

fun 'a sizeMessage (name: string, a: 'a): Layout.t =
   let open Layout
   in str (concat [name, " size is ",
		   Int.toCommaString (MLton.size a), " bytes"])
   end

val diagnosticWriter: (Layout.t -> unit) option ref = ref NONE

fun diagnostics f =
   case !diagnosticWriter of
      NONE => ()
    | SOME w => f w

fun diagnostic f = diagnostics (fn disp => disp (f ()))

fun saveToFile ({suffix: string},
		style,
		a: 'a,
		d: 'a display): unit =
   let
      fun doit f =
	 trace (Pass, "display")
	 Ref.fluidLet
	 (inputFile, concat [!inputFile, ".", suffix], fn () =>
	  File.withOut (!inputFile, fn out =>
			f (fn l => (Layout.outputl (l, out)))))
   in
      case d of
	 NoDisplay => ()
       | Layout layout =>
	    doit (fn output =>
		  (outputHeader (style, output)
		   ; output (layout a)))
       | Layouts layout =>
	    doit (fn output =>
		  (outputHeader (style, output)
		   ; layout (a, output)))
   end

fun maybeSaveToFile ({name: string, suffix: string},
		     style: style,
		     a: 'a,
		     d: 'a display): unit =
   if not (List.exists (!keepPasses, fn re =>
			Regexp.Compiled.matchesAll (re, name)))
      then ()
   else saveToFile ({suffix = concat [name, ".", suffix]}, style, a, d)

fun pass {name: string,
	  suffix: string,
	  style: style,
	  display = disp,
	  thunk: unit -> 'a}: 'a =
   let
      val result = 
	 if not (List.exists (!diagPasses, fn re =>
			      Regexp.Compiled.matchesAll (re, name)))
	    then trace (Pass, name) thunk ()
	 else
	    let
	       val result = ref NONE
	       val _ =
		  saveToFile
		  ({suffix = concat [name, ".diagnostic"]}, No, (),
		   Layouts (fn ((), disp) =>
			    (diagnosticWriter := SOME disp
			     ; result := SOME (trace (Pass, name) thunk ())
			     ; diagnosticWriter := NONE)))
	    in
	       valOf (!result)
	    end
      val verb = Detail
      val _ = message (verb, fn () => sizeMessage (suffix, result))
      val _ = message (verb, PropertyList.stats)
      val _ = message (verb, HashSet.stats)
      val _ = checkForErrors name
      val _ = maybeSaveToFile ({name = name, suffix = suffix},
			       style, result, disp)
   in
      result
   end

(* Code for diagnosing a pass. *)
val pass =
   fn z as {name, ...} =>
   if MLton.Profile.isOn
      then if not (List.exists (!profPasses, fn re =>
				Regexp.Compiled.matchesAll (re, name)))
	      then pass z
	   else let
		   open MLton.Profile
		   val d = Data.malloc ()
		   val result = withData (d, fn () => pass z)
		   val _ = Data.write (d, concat [!inputFile, ".", name, ".mlmon"])
		   val _ = Data.free d
		in
		   result
		end
   else pass z

fun passTypeCheck {name: string,
		   suffix: string,
		   style: style,
		   display,
		   thunk: unit -> 'a,
		   typeCheck = tc: 'a -> unit}: 'a =
   let
      val result = pass {name = name,
			 suffix = suffix,
			 display = display,
			 style = style,
			 thunk = thunk}
      val _ =
	 if !typeCheck
	    then trace (Pass, "typeCheck") tc result
	 else ()
   in
      result
   end

end
