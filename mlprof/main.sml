(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Main =
struct

type int = Int.t
type word = Word.t

val busy = ref false : bool ref
val color = ref false
val depth: int ref = ref 0
val raw = ref false
val source = ref true
val static = ref false (* include static C functions *)
val thresh: int ref = ref 0

val die = Process.fail
   
structure Regexp =
struct
  open Regexp
      
  val eol = seq [star (oneOf "\t "), string "\n"]
  val hexDigit = isChar Char.isHexDigit
  val hexDigits = oneOrMore hexDigit
  val identifier = seq [isChar Char.isAlpha,
			star (isChar (fn #"_" => true
				       | #"'" => true
				       | c => Char.isAlphaNum c))]
end

structure StringMap:
sig
  type 'a t

  val foldi: 'a t * 'b * (string * 'a * 'b -> 'b) -> 'b
  val layout: ('a -> Layout.t) -> 'a t -> Layout.t
  val lookup: 'a t * string -> 'a
  val lookupOrInsert: 'a t * string * (unit -> 'a) -> 'a
  val new: unit -> 'a t
end =
struct
  datatype 'a t = T of (word * String.t * 'a) HashSet.t

  fun layout lay (T h)
    = HashSet.layout (fn (_, s, a) => Layout.tuple [String.layout s, lay a]) h

  fun new () = T (HashSet.new {hash = #1})
    
  fun foldi (T t, b, f)
    = HashSet.fold (t, b, fn ((_, s, a), ac) => f (s, a, ac))
	 
  fun lookupOrInsert (T t, s, f)
    = let
	val w = String.hash s
      in
	#3 (HashSet.lookupOrInsert
	    (t, w,
	     fn (w', s', _) => w = w' andalso s = s',
	     fn () => (w, s, f ())))
      end
	 
  fun peek (T t, s)
    = let
	val w = String.hash s
      in
	Option.map
	(HashSet.peek (t, w, fn (w', s', _) => w = w' andalso s = s'),
	 #3)
      end

  fun contains z = isSome (peek z)
  fun lookup z = valOf (peek z)
end

structure ProfileInfo =
struct
   datatype 'a t = T of {data: 'a,
			 minor: 'a t} list

   val empty = T []

   local
      open Layout
   in
      fun layout lay (T l)
	 = List.layout 
	   (fn {data, minor} => seq [str "{",
				     lay data,
				     layout lay minor,
				     str "}"])
	   l
   end
end

structure AFile =
   struct
      datatype t = T of {magic: word,
			 sources: string vector}

      fun layout (T {magic, sources}) =
	 Layout.record [("magic", Word.layout magic),
			("sources", Vector.layout String.layout sources)]

      fun new {afile: File.t}: t =
	 Process.callWithIn
	 (afile, ["@MLton", "show-prof"],
	  fn ins =>
	  let
	     val magic =
		valOf (Word.fromString (In.inputLine ins))
	     fun loop ac =
		case In.inputLine ins of
		   "" => Vector.fromListRev ac
		 | s => loop (String.dropSuffix (s, 1) :: ac)
	     val sources = loop []
	  in
	     T {magic = magic,
		sources = sources}
	  end)
	 
      val new = Trace.trace ("AFile.new", File.layout o #afile, layout) new
   end

structure Kind =
   struct
      datatype t = Alloc | Time

      val toString =
	 fn Alloc => "Alloc"
	  | Time => "Time"

      val layout = Layout.str o toString
   end

structure ProfFile =
   struct
      datatype t = T of {counts: IntInf.t vector,
			 kind: Kind.t,
			 magic: word}

      local
	 fun make f (T r) = f r
      in
	 val kind = make #kind
      end

      fun layout (T {counts, kind, magic}) =
	 Layout.record [("kind", Kind.layout kind),
			("magic", Word.layout magic),
			("counts", Vector.layout IntInf.layout counts)]

      fun new {mlmonfile: File.t}: t =
	 File.withIn
	 (mlmonfile, fn ins =>
	  let
	     val _ =
		if "MLton prof\n" = In.inputLine ins
		   then ()
		else die (concat [mlmonfile,
				  " does not appear to be an mlmon file"])
	     val kind =
		case In.inputLine ins of
		   "alloc\n" => Kind.Alloc
		 | "time\n" => Kind.Time
		 | _ => die "invalid profile kind"
	     fun line () = String.dropSuffix (In.inputLine ins, 1)
	     val magic = valOf (Word.fromString (line ()))
	     fun loop ac =
		case In.inputLine ins of
		   "" => Vector.fromListRev ac
		 | s => loop (valOf (IntInf.fromString s) :: ac)
	     val counts = loop []
	  in
	     T {counts = counts,
		kind = kind,
		magic = magic}
	  end)

      val new =
	 Trace.trace ("ProfFile.new", File.layout o #mlmonfile, layout) new

      fun merge (T {counts = c, kind = k, magic = m},
		 T {counts = c', magic = m', ...}): t =
	 if m <> m'
	    then die "incompatible mlmon files"
	 else
	    T {counts = Vector.map2 (c, c', IntInf.+),
	       kind = k,
	       magic = m}
   end

fun attribute (AFile.T {magic = m, sources},
	       ProfFile.T {counts, kind, magic = m'})
    : {name: string,
       ticks: IntInf.t} ProfileInfo.t option =
   if m <> m'
      then NONE
   else
      SOME
      (ProfileInfo.T
       (Vector.fold2 (counts, sources, [], fn (c, s, ac) =>
		      if c = IntInf.zero
			 then ac
		      else {data = {name = s, ticks = c},
			    minor = ProfileInfo.empty} :: ac)))
      
val replaceLine =
   Promise.lazy
   (fn () =>
    let
       open Regexp
       val beforeColor = Save.new ()
       val label = Save.new ()
       val afterColor = Save.new ()
       val nodeLine =
	  seq [save (seq [anys, string "fontcolor = ", dquote], beforeColor),
	       star (notOneOf String.dquote),
	       save (seq [dquote,
			  anys,
			  string "label = ", dquote,
			  save (star (notOneOf " \\"), label),
			  oneOf " \\",
			  anys,
			  string "\n"],
		     afterColor)]
       val c = compileNFA nodeLine
       val _ = if true
	          then ()
	       else Compiled.layoutDotToFile (c, "/tmp/z.dot")
    in
       fn (l, color) =>
       case Compiled.matchAll (c, l) of
	  NONE => l
	| SOME m =>
	     let
		val {lookup, ...} = Match.stringFuns m
	     in
		concat [lookup beforeColor,
			color (lookup label),
			lookup afterColor]
	     end
    end)

fun display (kind: Kind.t,
	     counts: {name: string, ticks: IntInf.t} ProfileInfo.t,
	     baseName: string,
	     depth: int) =
   let
      val ticksPerSecond = 100.0
      val thresh = Real.fromInt (!thresh)
      datatype t = T of {name: string,
			 ticks: IntInf.t,
			 row: string list,
			 minor: t} array
      val mult = if !raw then 2 else 1
      fun doit (info as ProfileInfo.T profileInfo,
		n: int,
		dotFile: File.t,
		stuffing: string list,
		totals: real list) =
	 let
	    val totalInt =
	       List.fold
	       (profileInfo, IntInf.fromInt 0,
		fn ({data = {ticks, ...}, ...}, total) =>
		IntInf.+ (total, ticks))
	    val total = Real.fromIntInf totalInt
	    val _ =
	       if n = 0
		  then
		     print
		     (concat
		      (case kind of
			  Kind.Alloc =>
			     [IntInf.toCommaString totalInt,
			      " bytes allocated\n"]
			| Kind.Time => 
			     [Real.format (total / ticksPerSecond, 
					   Real.Format.fix (SOME 2)),
			      " seconds of CPU time\n"]))
	       else ()
	    val space = String.make (5 * n, #" ")
	    val profileInfo =
	       List.fold
	       (profileInfo, [], fn ({data = {name, ticks}, minor}, ac) =>
		let
		   val rticks = Real.fromIntInf ticks
		   fun per total = 100.0 * rticks / total
		in
		   if per total < thresh
		      then ac
		   else
		      let
			 val per =
			    fn total =>
			    let
			       val a =
				  concat [Real.format (per total,
						       Real.Format.fix (SOME 2)),
					  "%"]
			    in
			       if !raw
				  then
				     [a,
				      concat
				      (case kind of
					  Kind.Alloc =>
					     ["(",
					      IntInf.toCommaString ticks,
					      ")"]
					| Kind.Time =>
					     ["(",
					      Real.format
					      (rticks / ticksPerSecond,
					       Real.Format.fix (SOME 2)),
					      "s)"])]
			       else [a]
			    end
		      in			    
			 {name = name,
			  ticks = ticks,
			  row = (List.concat
				 [[concat [space, name]],
				  stuffing,
				  per total,
				  if !busy
				     then List.concatMap (totals, per)
				  else (List.duplicate
					(List.length totals * mult,
					 fn () => ""))]),
			  minor = if n < depth
				     then doit (minor, n + 1,
						concat [baseName, ".",
							name, ".cfg.dot"],
						if !raw
						   then tl (tl stuffing)
						else tl stuffing,
						total :: totals)
				  else T (Array.new0 ())}
			 :: ac
		      end
		end)
	    val a = Array.fromList profileInfo
	    val _ =
	       QuickSort.sortArray
	       (a, fn ({ticks = t1, ...}, {ticks = t2, ...}) =>
		IntInf.>= (t1, t2))
	    (* Colorize. *)
	    val _ =
	       if n > 1 orelse not(!color) orelse 0 = Array.length a
		  then ()
	       else
		  let
		     val ticks: real =
			Real.fromIntInf (#ticks (Array.sub (a, 0)))
		     fun thresh r = Real.toIntInf (Real.* (ticks, r))
		     val thresh1 = thresh (2.0 / 3.0)
		     val thresh2 = thresh (1.0 / 3.0)
		     datatype z = datatype DotColor.t
		     fun color l =
			DotColor.toString
			(case Array.peek (a, fn {name, ...} =>
					  String.equals (l, name)) of
			    NONE => Black
			  | SOME {ticks, ...} =>
			       if IntInf.>= (ticks, thresh1)
				  then Red1
			       else if IntInf.>= (ticks, thresh2)
				       then Orange2
				    else Yellow3)
		  in
		     if not (File.doesExist dotFile)
			then ()
		     else
			let
			   val replaceLine = replaceLine ()
			   val lines = File.lines dotFile
			in
			   File.withOut
			   (dotFile, fn out =>
			    List.foreach
			    (lines, fn l =>
			     Out.output (out, replaceLine (l, color))))
			end
		  end
	 in T a
	 end
      fun toList (T a, ac) =
	 Array.foldr (a, ac, fn ({row, minor, ...}, ac) =>
		      row :: toList (minor, ac))
      val rows = toList (doit (counts, 0,
			       concat [baseName, ".call-graph.dot"],
			       List.duplicate (depth * mult, fn () => ""),
			       []),
			 [])
      val _ =
	 let
	    open Justify
	 in
	    outputTable
	    (table {justs = (Left
			     :: (List.duplicate ((depth + 1) * mult,
						 fn () => Right))),
		    rows = rows},
	     Out.standard)
	 end
   in
      ()
   end
   
fun makeOptions {usage} =
   let
      open Popt
   in
      List.map
      ([(Normal, "busy", "{false|true}", "show all percentages",
	 boolRef busy),
	(Normal, "color", " {false|true}", "color .dot files",
	 boolRef color),
	(Expert, "depth", " {0|1|2}", "depth of detail",
	 Int (fn i => if i < 0 orelse i > 2
			 then usage "invalid depth"
		      else depth := i)),
	(Normal, "raw", " {false|true}", "show raw counts",
	 boolRef raw),
	(Expert, "source", " {true|false}", "report info at source level",
	 boolRef source),
	(Normal, "static", " {false|true}", "show static C functions",
	 boolRef static),
	(Normal, "thresh", " {0|1|...|100}", "only show counts above threshold",
	 Int (fn i => if i < 0 orelse i > 100
			 then usage "invalid -thresh"
		      else thresh := i))],
       fn (style, name, arg, desc, opt) =>
       {arg = arg, desc = desc, name = name, opt = opt, style = style})
   end

val mainUsage = "mlprof [option ...] a.out mlmon.out [mlmon.out ...]"
val {parse, usage} =
   Popt.makeUsage {mainUsage = mainUsage,
		   makeOptions = makeOptions,
		   showExpert = fn () => false}

fun commandLine args =
   let
      val rest = parse args
    in
       case rest of
	  Result.No msg => usage msg
	| Result.Yes (afile::mlmonfile::mlmonfiles) =>
	     let
		val _ =
		   if !source andalso !depth > 0
		      then die "cannot report source info with depth > 0"
		   else ()
		val aInfo = AFile.new {afile = afile}
		val _ =
		   if true
		      then ()
		   else (print "AFile:\n"
			 ; Layout.outputl (AFile.layout aInfo, Out.standard))
		val profFile =
		   List.fold
		   (mlmonfiles, ProfFile.new {mlmonfile = mlmonfile},
		    fn (mlmonfile, profFile) =>
		    ProfFile.merge (profFile,
				    ProfFile.new {mlmonfile = mlmonfile}))
		val _ =
		   if true
		      then ()
		   else (print "ProfFile:\n"
			 ; Layout.outputl (ProfFile.layout profFile, Out.standard))
		val _ =
		   if !depth = 2
		      andalso ProfFile.kind profFile = Kind.Alloc
		      then usage "-depth 2 is meaningless with allocation profiling"
		   else ()
		val info =
		   case attribute (aInfo, profFile) of
		      NONE => die (concat [afile, " is incompatible with ",
					   mlmonfile])
		    | SOME z => z
		val _ = display (ProfFile.kind profFile, info, afile, !depth)
	     in
		()
	     end
	| Result.Yes _ => usage "wrong number of args"
   end

val main = Process.makeMain commandLine

end
