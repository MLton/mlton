(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
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
   
val basisLibs = ["basis-2002", "basis-2002-strict", "basis-1997", "basis-none"]
   
val basisLibrary = control {name = "basis library",
			    default = "basis-2002",
			    toString = fn s => s}

val cardSizeLog2 = control {name = "log2 (card size)",
			    default = 8,
			    toString = Int.toString}
   
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

val deadCode = control {name = "dead code",
			default = true,
			toString = Bool.toString}
   
val debug = control {name = "debug",
		     default = false,
		     toString = Bool.toString}

val defines = control {name = "defines",
		       default = [],
		       toString = List.toString String.toString}

val detectOverflow = control {name = "detect overflow",
			      default = true,
			      toString = Bool.toString}

val dropPasses =
   control {name = "drop passes",
	    default = [],
	    toString = List.toString
	               (Layout.toString o
			Regexp.Compiled.layout)}

val elaborateOnly =
   control {name = "elaborate only",
	    default = false,
	    toString = Bool.toString}

val eliminateOverflow =
   control {name = "eliminate overflow",
	    default = true,
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

structure Handlers =
   struct
      datatype t = Flow | Simple

      val toString =
	 fn Flow => "Flow"
	  | Simple => "Simple"
   end

datatype handlers = datatype Handlers.t

val handlers = control {name = "handlers",
			default = Flow,
			toString = Handlers.toString}

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

val layoutInline = Inline.layout

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

val inputFile = control {name = "input file",
			 default = "<bogus>",
			 toString = File.toString}

val instrument = control {name = "instrument",
			  default = false,
			  toString = Bool.toString}

val instrumentSxml = control {name = "instrument Sxml",
			      default = false,
			      toString = Bool.toString}

val keepDefUse = control {name = "keep def-use",
			  default = false,
			  toString = Bool.toString}
   
val keepMachine = control {name = "keep Machine",
			   default = false,
			   toString = Bool.toString}
   
val keepRSSA = control {name = "keep RSSA",
			default = false,
			toString = Bool.toString}

val keepSSA = control {name = "keep SSA",
		       default = false,
		       toString = Bool.toString}

val keepDiagnostics = control {name = "keep diagnostics",
			       default = [],
			       toString = List.toString 
			                  (Layout.toString o 
					   Regexp.Compiled.layout)}

val keepDot = control {name = "keep dot",
		       default = false,
		       toString = Bool.toString}

val keepPasses = control {name = "keep passes",
			  default = [],
			  toString = List.toString 
			             (Layout.toString o 
				      Regexp.Compiled.layout)}

val libDir = control {name = "lib dir",
		      default = "<libDir unset>",
		      toString = fn s => s}

val libTargetDir = control {name = "lib target dir",
			    default = "<libTargetDir unset>",
			    toString = fn s => s} 
   
structure LimitCheck =
   struct
      datatype t = 
	 PerBlock
       | ExtBasicBlocks
       | LoopHeaders of {fullCFG: bool,
			 loopExits: bool}

      val toString =
	 fn PerBlock => "per block"
	  | ExtBasicBlocks => "extended basic blocks"
	  | LoopHeaders {fullCFG, loopExits} =>
	       concat ["loop headers (fullCFG = ",
		       Bool.toString fullCFG,
		       ", loopExits = ",
		       Bool.toString loopExits,
		       ")"]
   end

datatype limitCheck = datatype LimitCheck.t

val limitCheck = control {name = "limit check",
			  default = LoopHeaders {fullCFG = false,
						 loopExits = true},
			  toString = LimitCheck.toString}

val limitCheckCounts = control {name = "limit check counts",
				default = false,
				toString = Bool.toString}

val loopPasses = control {name = "loop passes",
			  default = 1,
			  toString = Int.toString}

val markCards = control {name = "mark cards",
			 default = true,
			 toString = Bool.toString}

val mayLoadWorld = control {name = "may load world",
			    default = true,
			    toString = Bool.toString}

structure Native =
   struct
      val native = control {name = "native",
			    default = true,
			    toString = Bool.toString}

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

      val cutoff = control {name = "native cutoff",
			    default = 100,
			    toString = Int.toString}

      val liveTransfer = control {name = "native live transfer",
				  default = 8,
				  toString = Int.toString}

      val future = control {name = "native future",
			    default = 64,
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

val newReturn = control {name = "new return",
			 default = false,
			 toString = Bool.toString}

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

structure Profile =
   struct
      datatype t = ProfileNone | ProfileAlloc | ProfileCount | ProfileTime

      val toString =
	 fn ProfileNone => "None"
	  | ProfileAlloc => "Alloc"
	  | ProfileCount => "Count"
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
			     default = true,
			     toString = Bool.toString}

structure ProfileIL =
   struct
      datatype t = ProfileSSA | ProfileSource

      val toString =
	 fn ProfileSSA => "ProfileSSA"
	  | ProfileSource => "ProfileSource"
   end

datatype profileIL = datatype ProfileIL.t
   
val profileIL = control {name = "profile IL",
			 default = ProfileSource,
			 toString = ProfileIL.toString}

val profileStack = control {name = "profile stack",
			    default = false,
			    toString = Bool.toString}

structure Representation =
   struct
      datatype t = Packed | Unpacked

      val toString =
	 fn Packed => "Packed"
	  | Unpacked => "Unpacked"
   end

datatype representation = datatype Representation.t

val representation = control {name = "representation",
			      default = Packed,
			      toString = Representation.toString}

val reserveEsp = control {name = "reserve esp",
			  default = NONE,
			  toString = Option.toString Bool.toString}

val safe = control {name = "safe",
		    default = true,
		    toString = Bool.toString}

val sequenceUnit = control {name = "sequence unit",
			    default = false,
			    toString = Bool.toString}

val showBasis = control {name = "show basis",
			 default = NONE,
			 toString = Option.toString File.toString}
   
val showBasisUsed = control {name = "show basis used",
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

val stackCont = control {name = "stack cont",
			 default = false,
			 toString = Bool.toString}

val static = control {name = "static",
		      default = false,
		      toString = Bool.toString}

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

fun targetIsBigEndian () =
   case !targetArch of
      X86 => false
    | Sparc => true

datatype os = datatype MLton.Platform.OS.t

val targetOS = control {name = "target OS",
			default = Linux,
			toString = MLton.Platform.OS.toString}

val textIOBufSize = control {name = "TextIO buffer size",
			     default = 4096,
			     toString = Int.toString}

val typeCheck = control {name = "type check",
			 default = false,
			 toString = Bool.toString}

structure TypeError =
   struct
      datatype t = Concise | Full

      val toString =
	 fn Concise => "concise"
	  | Full => "full"
   end

datatype typeError = datatype TypeError.t

val typeError = control {name = "type error",
			 default = Concise,
			 toString = TypeError.toString}
   
val useBasisLibrary = control {name = "use basis library",
			       default = true,
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

val warnNonExhaustive = control {name = "warn non-exhaustive",
				 default = true,
				 toString = Bool.toString}

val warnRedundant = control {name = "warn redundant",
			     default = true,
			     toString = Bool.toString}

val warnUnused = control {name = "warn unused",
			  default = false,
			  toString = Bool.toString}

val xmlPassesSet : (string -> string list Result.t) ref = 
   control {name = "xmlPassesSet",
	    default = fn _ => Error.bug ("xmlPassesSet not installed"),
	    toString = fn _ => "<xmlPassesSet>"}
val xmlPasses : string list ref = 
   control {name = "xmlPasses",
	    default = ["default"],
	    toString = List.toString String.toString}

datatype style = No | Assembly | C | Dot | ML

fun preSuf style =
   let
      val (p, s) =
	 case style of
	    No => ("", "")
	  | Assembly => ("# ", "")
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
      val toReal = Int.toReal o LargeInt.toInt o Time.toMilliseconds
      val per =
	 if Time.equals (total, Time.zero)
	    then "0"
	 else fmt (100.0 * (toReal gc / toReal total), 0)
      fun t2s t =
	 fmt (Real./ (Int.toReal (LargeInt.toInt (Time.toMilliseconds t)),
		      1000.0),
	      2)
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
	    handle e => (messageStr (verb,
				     concat [name, " raised"])
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
	 in
	    outputl (align [seq [str (concat [kind, ": "]),
				 str p, str ": ", msg],
			    indent (extra, 3)],
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
	 if not (List.exists (!keepDiagnostics, fn re =>
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

(* Code for profiling each pass. *)
val pass =
   fn z as {name, ...} =>
   if true
      then pass z
   else
      let
	 open MLton.Profile
	 val d = Data.malloc ()
	 val res = withData (d, fn () => pass z)
	 val _ = Data.write (d, concat ["/tmp/", name, ".mlmon"])
	 val _ = Data.free d
      in
	 res
      end

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

fun passSimplify {name, suffix, style, thunk, display, typeCheck, simplify} =
   let
      val result =
	 passTypeCheck {name = name,
			suffix = suffix,
			style = style,
			thunk = thunk,
			display = display,
			typeCheck = typeCheck}
   in passTypeCheck {name = name ^ "Simplify",
		     suffix = suffix,
		     style = style,
		     thunk = fn () => simplify result,
		     display = display,
		     typeCheck = typeCheck}
   end

end
