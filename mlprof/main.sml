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

val sourcesIndexGC: int = 1

structure GraphShow =
   struct
      datatype t = Above
   end

val graphShow = ref GraphShow.Above
val raw = ref false
val showLine = ref false
val thresh: int ref = ref 0

val die = Process.fail

structure Source =
   struct
      datatype t =
	 NamePos of {name: string,
		     pos: string}
       | Simple of string

      fun toString n =
	 case n of
	    NamePos {name, pos} =>
	       if !showLine
		  then concat [name, "  ", pos]
	       else name
	  | Simple s => s

      val layout = Layout.str o toString

      fun fromString s =
	 case String.tokens (s, fn c => Char.equals (c, #"\t")) of
	    [s] => Simple s
	  | [name, pos] => NamePos {name = name, pos = pos}
	  | _ => die "strange source"

      fun toDotLabel s =
	 case s of
	    NamePos {name, pos} =>
	       if !showLine
		  then [(name, Dot.Center),
			(pos, Dot.Center)]
	       else [(name, Dot.Center)]
	  | Simple s =>
	       [(s, Dot.Center)]
   end

structure AFile =
   struct
      datatype t = T of {magic: word,
			 name: string,
			 sourceSuccessors: int vector vector,
			 sources: Source.t vector}

      fun layout (T {magic, name, sourceSuccessors, sources}) =
	 Layout.record [("name", String.layout name),
			("magic", Word.layout magic),
			("sources", Vector.layout Source.layout sources),
			("sourceSuccessors",
			 Vector.layout (Vector.layout Int.layout)
			 sourceSuccessors)]

      fun new {afile: File.t}: t =
	 Process.callWithIn
	 (afile, ["@MLton", "show-prof"],
	  fn ins =>
	  let
	     fun line () = In.inputLine ins
	     val magic = valOf (Word.fromString (line ()))
	     val sourcesLength = valOf (Int.fromString (line ()))
	     val _ =
		if 0 = sourcesLength
		   then die (concat [afile, " not compiled for profiling"])
		else ()
	     val sources =
		Vector.tabulate (sourcesLength, fn _ =>
				 Source.fromString
				 (String.dropSuffix (line (), 1)))
	     val sourceSuccessors =
		Vector.tabulate
		(sourcesLength, fn _ =>
		 Vector.fromListMap
		 (String.tokens (line (), Char.isSpace), fn s =>
		  valOf (Int.fromString s)))
	     val _ =
		case line () of
		   "" => ()
		 | _ => Error.bug "mlmon file has extra line"
	  in
	     T {magic = magic,
		name = afile,
		sourceSuccessors = sourceSuccessors,
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

structure Style =
   struct
      datatype t = Current | Stack

      val toString =
	 fn Current => "Current"
	  | Stack => "Stack"

      val layout = Layout.str o toString
   end

structure Counts =
   struct
      datatype t =
	 Current of IntInf.t vector
       | Stack of {current: IntInf.t,
		   stack: IntInf.t,
		   stackGC: IntInf.t} vector

      val layout =
	 fn Current v => Vector.layout IntInf.layout v
	  | Stack v =>
	       Vector.layout
	       (fn {current, stack, stackGC} =>
		Layout.record [("current", IntInf.layout current),
			       ("stack", IntInf.layout stack),
			       ("stackGC", IntInf.layout stackGC)])
	       v

      fun merge (c: t, c': t): t =
	 case (c, c') of
	    (Current v, Current v') =>
	       Current (Vector.map2 (v, v', IntInf.+))
	  | (Stack v, Stack v') =>
	       Stack (Vector.map2
		      (v, v', fn ({current = c, stack = s, stackGC = g},
				  {current = c', stack = s', stackGC = g'}) =>
		       {current = IntInf.+ (c, c'),
			stack = IntInf.+ (s, s'),
			stackGC = IntInf.+ (g, g')}))
	  | _ => die "cannot merge -profile-stack false with -profile-stack true"
   end

structure ProfFile =
   struct
      datatype t = T of {counts: Counts.t,
			 kind: Kind.t,
			 magic: word,
			 total: IntInf.t,
			 totalGC: IntInf.t}

      local
	 fun make f (T r) = f r
      in
	 val kind = make #kind
      end

      fun layout (T {counts, kind, magic, total, totalGC}) =
	 Layout.record [("kind", Kind.layout kind),
			("magic", Word.layout magic),
			("total", IntInf.layout total),
			("totalGC", IntInf.layout totalGC),
			("counts", Counts.layout counts)]

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
	     val style =
		case In.inputLine ins of
		   "current\n" => Style.Current
		 | "stack\n" => Style.Stack
		 | _ => die "invalid profile style"
	     fun line () = String.dropSuffix (In.inputLine ins, 1)
	     val magic = valOf (Word.fromString (line ()))
	     val s2i = valOf o IntInf.fromString
	     val (total, totalGC) =
		case String.tokens (line (), Char.isSpace) of
		   [total, totalGC] => (s2i total, s2i totalGC)
		 | _ => die "invalid totals"
	     fun getCounts (fromLine: string -> 'a): 'a vector =
		let
		   fun loop ac =
		      case In.inputLine ins of
			 "" => Vector.fromListRev ac
		       | s => loop (fromLine s :: ac)
		in
		   loop []
		end
	     val counts =
		case style of
		   Style.Current => Counts.Current (getCounts s2i)
		 | Style.Stack =>
		      Counts.Stack
		      (getCounts (fn s =>
				  case String.tokens (s, Char.isSpace) of
				     [c, s, sGC] =>
					{current = s2i c,
					 stack = s2i s,
					 stackGC = s2i sGC}
				   | _ => die (concat ["strange line: ", s])))
	  in
	     T {counts = counts,
		kind = kind,
		magic = magic,
		total = total,
		totalGC = totalGC}
	  end)

      val new =
	 Trace.trace ("ProfFile.new", File.layout o #mlmonfile, layout) new

      fun merge (T {counts = c, kind = k, magic = m, total = t, totalGC = g},
		 T {counts = c', magic = m', total = t', totalGC = g', ...}): t =
	 if m <> m'
	    then die "incompatible mlmon files"
	 else
	    T {counts = Counts.merge (c, c'),
	       kind = k,
	       magic = m,
	       total = IntInf.+ (t, t'),
	       totalGC = IntInf.+ (g, g')}
   end

structure Graph = DirectedGraph
local
   open Graph
in
   structure Node = Node
end

fun display (AFile.T {name = aname, sources, sourceSuccessors, ...},
	     ProfFile.T {counts, kind, total, totalGC, ...}): unit =
   let
      val {get = nodeOptions: Node.t -> Dot.NodeOption.t list ref, ...} =
	 Property.get (Node.plist, Property.initFun (fn _ => ref []))
      val graph = Graph.new ()
      val ticksPerSecond = 100.0
      val thresh = Real.fromInt (!thresh)
      val totalReal = Real.fromIntInf (IntInf.+ (total, totalGC))
      fun per (ticks: IntInf.t): real * string list =
	 let
	    val rticks = Real.fromIntInf ticks
	    val per = 100.0 * rticks / totalReal
	    val row =
	       (concat [Real.format (per, Real.Format.fix (SOME 1)),
			"%"])
	       :: (if !raw
		      then
			 [concat
			  (case kind of
			      Kind.Alloc =>
				 ["(", IntInf.toCommaString ticks, ")"]
			    | Kind.Time =>
				 ["(",
				  Real.format
				  (rticks / ticksPerSecond,
				   Real.Format.fix (SOME 2)),
				  "s)"])]
		   else [])
	 in
	    (per, row)
	 end
      val profileStack =
	 case counts of
	    Counts.Current _ => false
	  | Counts.Stack _ => true
      fun doit (v, f) =
	 Vector.mapi
	 (v, fn (i, x) =>
	  let
	     val {per, row} = f x
	     val showInTable =
		(per > 0.0 andalso per >= thresh)
		orelse (not profileStack andalso i = sourcesIndexGC)
	     val source = Vector.sub (sources, i)
	     val node =
		if (not profileStack orelse i <> sourcesIndexGC)
		   andalso (case !graphShow of
			       GraphShow.Above => per >= thresh)
		   andalso per > 0.0
		   then
		      let
			 val node = Graph.newNode graph
			 val no = nodeOptions node
			 val _ = 
			    List.push
			    (no,
			     Dot.NodeOption.Label
			     (Source.toDotLabel source
			      @ [(concat (List.separate (row, " ")),
				  Dot.Center)]))
			 val _ =
			    List.push (no, Dot.NodeOption.Shape Dot.Box)
		      in
			 SOME node
		      end
		else NONE
	  in
	     {node = node,
	      per = per,
	      row = Source.toString source :: row,
	      showInTable = showInTable}
	  end)
      val counts =
	 case counts of
	    Counts.Current v =>
	       doit (v, fn i =>
		     let
			val (per, row) = per i
		     in
			{per = per, row = row}
		     end)
	  | Counts.Stack v =>
	       doit (v, fn {current, stack, stackGC} =>
		     let
			val (cp, cr) = per current
			val (sp, sr) = per stack
			val (_, gr) = per stackGC
		     in
			{per = cp, row = List.concat [cr, sr, gr]}
		     end)
      val _ =
	 Vector.mapi
	 (counts,
	  fn (i, {node, ...}) =>
	  case node of
	     NONE => ()
	   | SOME from =>
		Vector.foreach
		(Vector.sub (sourceSuccessors, i), fn j =>
		 let
		    val {node, ...} = Vector.sub (counts, j)
		 in
		    case node of
		       NONE => ()
		     | SOME to =>
			  (Graph.addEdge (graph, {from = from, to = to})
			   ; ())
		 end))
      val _ = 
	 File.withOut
	 (concat [aname, ".dot"], fn out =>
	  Layout.output
	  (Graph.layoutDot (graph,
			    fn _ => {edgeOptions = fn _ => [],
				     nodeOptions = ! o nodeOptions,
				     options = [],
				     title = "call-stack graph"}),
	   out))
      val tableRows =
	 QuickSort.sortVector
	 (Vector.keepAll (counts, #showInTable),
	  fn ({per = p, ...}, {per = p', ...}) => p >= p')
      val _ = 
	 print
	 (concat
	  (case kind of
	      Kind.Alloc =>
		 [IntInf.toCommaString total, " bytes allocated (",
		  IntInf.toCommaString totalGC, " bytes by GC)\n"]
	    | Kind.Time =>
		 let
		    fun t2s i = 
		       Real.format (Real.fromIntInf i / ticksPerSecond, 
				    Real.Format.fix (SOME 2))
		 in
		    [t2s total, " seconds of CPU time (",
		     t2s totalGC, " seconds GC)\n"]
		 end))
      val columnHeads =
	 "function"
	 :: let
	       val pers =
		  if profileStack
		     then ["cur", "stack", "GC"]
		  else ["cur"]
	    in
	       if !raw
		  then List.concatMap (pers, fn p => [p, "raw"])
	       else pers
	    end
      val cols =
	 (if profileStack then 3 else 1) * (if !raw then 2 else 1)
      val _ =
	 let
	    open Justify
	 in
	    outputTable
	    (table {columnHeads = SOME columnHeads,
		    justs = Left :: List.duplicate (cols, fn () => Right),
		    rows = Vector.toListMap (tableRows, #row)},
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
      ([(Expert, "graph", " {above|all}", " show graph nodes",
	 SpaceString (fn s =>
		      case s of
			 "above" => graphShow := GraphShow.Above
		       | _ => usage "invalid -graph arg")),
	(Normal, "raw", " {false|true}", "show raw counts",
	 boolRef raw),
	(Normal, "show-line", " {false|true}", " show line numbers",
	 boolRef showLine),
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
			 ; Layout.outputl (ProfFile.layout profFile,
					   Out.standard))
		val _ =
		   let
		      val AFile.T {magic = m, sources, ...} = aInfo
		      val ProfFile.T {magic = m', ...} = profFile
		   in
		      if m <> m'
			 then
			    die (concat [afile, " is incompatible with ",
					 mlmonfile])
		      else ()
		   end
		val _ = display (aInfo, profFile)
	     in
		()
	     end
	| Result.Yes _ => usage "wrong number of args"
   end

val main = Process.makeMain commandLine

end
