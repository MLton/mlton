(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Control: CONTROL =
struct

structure C = Control ()
open C

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

      val layout = Layout.str o toString
   end

datatype chunk = datatype Chunk.t
val chunk = control {name = "chunk",
		     default = Coalesce {limit = 4096},
		     toString = Chunk.toString}

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
	    toString = List.toString String.toString}

val exnHistory = control {name = "exn history",
			  default = false,
			  toString = Bool.toString}
   
val fixedHeap = control {name = "fixed heap",
			 default = NONE,
			 toString = Option.toString Int.toString}

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

val includes: string list ref =
   control {name = "includes",
	    default = [],
	    toString = List.toString String.toString}
   
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

val keepSSA = control {name = "keepSSA",
		       default = false,
		       toString = Bool.toString}

val keepDiagnostics = control {name = "keep diagnostics",
			       default = [],
			       toString = List.toString (fn s => s)}

val keepDot = control {name = "keep dot",
		       default = false,
		       toString = Bool.toString}

val keepPasses = control {name = "keep passes",
			  default = [],
			  toString = List.toString (fn s => s)}

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

      val layout = Layout.str o toString
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

val printAtFunEntry = control {name = "print at fun entry",
			       default = false,
			       toString = Bool.toString}

val profile = control {name = "profile",
		       default = false,
		       toString = Bool.toString}

val safe = control {name = "safe",
		    default = true,
		    toString = Bool.toString}

val showBasisUsed = control {name = "show basis used",
			     default = false,
			     toString = Bool.toString}
   
val showTypes = control {name = "show types",
			 default = false,
			 toString = Bool.toString}

val stackCont = control {name = "stack cont",
			 default = false,
			 toString = Bool.toString}

val static = control {name = "static",
		      default = false,
		      toString = Bool.toString}

val typeCheck = control {name = "type check",
			 default = false,
			 toString = Bool.toString}
   
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

val version = "MLton VERSION"

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

val build = concat [" (built ", Date.toString (Date.now ()),
		    " on ", Process.hostName (), ")"]

fun outputHeader (style: style, output: Layout.t -> unit) =
   let
      val (pre, suf) = preSuf style
      val lines =
	 concat [version, build]
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

(* Add a decimal point to a string so that the resulting
 * string has n digits after the decimal point.
 *)
fun addDecimal (s, n) =
   let
      val rec mkZero =
	 fn 0 => []
	  | n => #"0" :: mkZero (n - 1)
      val l = String.size s
   in
      if l <= n
	 then let
		 val zero = "0." ^ implode (mkZero n)
		 val pre = String.extract (zero, 0, SOME (n + 2 - l))
	      in
		 pre ^ s
	      end
      else let
	      val pre = String.extract (s, 0, SOME  (l - n))
	      val post = String.extract (s, l - n, NONE)
	   in
	      pre ^ "." ^ post
	   end
   end

fun time () =
   let
      open Time
      val {children, self, gc, ...} = times ()
      fun add {utime, stime} = utime + stime
   in
      (add self + add children, add gc)
   end

fun timeMinus (t1,t2,s)
  = Time.- (t1, t2)
    handle Time => Error.bug (concat ["timeMinus: ",
				      "t1: ",
				      Time.toString t1,
				      " ",
				      "t2: ",
				      Time.toString t2,
				      " :: ",
				      s])
fun timePlus (t1,t2,s)
  = Time.+ (t1, t2)
    handle Time => Error.bug (concat ["timePlus: ",
				      "t1: ",
				      Time.toString t1,
				      " ",
				      "t2: ",
				      Time.toString t2,
				      " :: ",
				      s])

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
   in concat [t2s (timeMinus (total, gc, "timeToString: total - gc")),
	      " + ",
	      t2s gc,
	      " (", per, "% GC)"]
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
		  timeToString {total = timeMinus (t', t, "trace: t' - t"),
				gc = timeMinus (gc', gc, "trace: gc' - gc")}
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
		  total := timePlus 
		           (!total,
			    timeMinus
			    (t', t,
			     "traceAdd: 't - t"),
			    "traceAdd: !total") ;
		  totalGC := timePlus
		             (!totalGC,
			      timeMinus
			      (gc', gc,
			       "traceAdd: gc' - gc"),
			      "traceAdd: !totalGC")
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

fun error (r: Region.t, msg: Layout.t): unit =
   (Int.inc numErrors
    ; Out.output (Out.error, 
		  concat [Region.toString r, " Error: ", Layout.toString msg])
    ; Out.newline Out.error
    ; if !numErrors = !errorThreshhold
	 then die "compilation aborted: too many errors"
      else ())

fun errorStr (r, msg) = error (r, Layout.str msg)

fun checkForErrors (name: string) =
   if !numErrors > 0
      then die (concat ["compilation aborted: ", name, " reported errors"])
   else ()

fun reset () = numErrors := 0

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
   if not (List.contains (!keepPasses, name, String.equals))
      then ()
   else saveToFile ({suffix = concat [name, ".", suffix]}, style, a, d)

fun pass {name: string,
	  suffix: string,
	  style: style,
	  display = disp,
	  thunk: unit -> 'a}: 'a =
   let
      val result =
	 if not (List.contains (!keepDiagnostics, name, String.equals))
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
