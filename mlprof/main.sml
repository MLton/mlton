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

val debug = false

val sourcesIndexGC: int = 1

val gray: bool ref = ref false
val mlmonFiles: string list ref = ref []
val raw = ref false
val showLine = ref false
val thresh: real ref = ref 0.0
val title: string option ref = ref NONE
val tolerant: bool ref = ref false

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
	  | _ => Error.bug "strange source"

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

structure Graph = DirectedGraph
local
   open Graph
in
   structure Edge = Edge
   structure Node = Node
end

structure AFile =
   struct
      datatype t = T of {callGraph: Graph.t,
			 magic: word,
			 name: string,
			 nodeIndex: Node.t -> int,
			 sources: {node: Node.t,
				   source: Source.t} vector}

      fun layout (T {magic, name, sources, ...}) =
	 Layout.record
	 [("name", String.layout name),
	  ("magic", Word.layout magic),
	  ("sources", Vector.layout (Source.layout o #source) sources)]

      fun new {afile: File.t}: t =
	 if not (File.doesExist afile)
	    then Error.bug "does not exist"
	 else if not (File.canRun afile)
	    then Error.bug "can not run"
	 else
	    Process.callWithIn
	    (afile, ["@MLton", "show-prof"],
	     fn ins =>
	     let
		fun line () = In.inputLine ins
		val magic = valOf (Word.fromString (line ()))
		val sourcesLength = valOf (Int.fromString (line ()))
		val _ =
		   if 0 = sourcesLength
		      then Error.bug "0 = sourcesLength"
		   else ()
		val graph = Graph.new ()
		val {get = nodeIndex, set = setNodeIndex, ...} =
		   Property.getSetOnce
		   (Node.plist, Property.initRaise ("index", Node.layout))
		val sources =
		   Vector.tabulate
		   (sourcesLength, fn i =>
		    let
		       val n = Graph.newNode graph
		       val _ = setNodeIndex (n, i)
		    in
		       {node = n,
			source = (Source.fromString
				  (String.dropSuffix (line (), 1)))}
		    end)
		val _ =
		   Int.for
		   (0, sourcesLength, fn i =>
		    let
		       val from = #node (Vector.sub (sources, i))
		    in
		       List.foreach
		       (String.tokens (line (), Char.isSpace), fn s =>
			let
			   val suc = valOf (Int.fromString s)
			   val _ =
			      Graph.addEdge
			      (graph,
			       {from = from,
				to = #node (Vector.sub (sources, suc))})
			in
			   ()
			end)
		    end)
		val _ =
		   case line () of
		      "" => ()
		    | _ => Error.bug "expected end of file"
	     in
		T {callGraph = graph,
		   magic = magic,
		   name = afile,
		   nodeIndex = nodeIndex,
		   sources = sources}
	     end)
   end

structure Kind =
   struct
      datatype t = Alloc | Empty | Time

      val toString =
	 fn Alloc => "Alloc"
	  | Empty => "Empty"
	  | Time => "Time"

      val layout = Layout.str o toString

      val merge: t * t -> t =
	 fn (k, k') =>
	 case (k, k') of
	    (Alloc, Alloc) => Alloc
	  | (_, Empty) => k
	  | (Empty, _) => k'
	  | (Time, Time) => Time
	  | _ => Error.bug "Kind.merge"
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
       | Empty
       | Stack of {current: IntInf.t,
		   stack: IntInf.t,
		   stackGC: IntInf.t} vector

      val layout =
	 fn Current v => Vector.layout IntInf.layout v
	  | Empty => Layout.str "empty"
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
	  | (Empty, _) => c'
	  | (_, Empty) => c
	  | (Stack v, Stack v') =>
	       Stack (Vector.map2
		      (v, v', fn ({current = c, stack = s, stackGC = g},
				  {current = c', stack = s', stackGC = g'}) =>
		       {current = IntInf.+ (c, c'),
			stack = IntInf.+ (s, s'),
			stackGC = IntInf.+ (g, g')}))
	  | _ =>
	       Error.bug
	       "cannot merge -profile-stack false with -profile-stack true"
   end

structure ProfFile =
   struct
      datatype t = T of {counts: Counts.t,
			 kind: Kind.t,
			 magic: word,
			 total: IntInf.t,
			 totalGC: IntInf.t}

      fun empty (AFile.T {magic, sources, ...}) =
	 T {counts = Counts.Empty,
	    kind = Kind.Empty,
	    magic = magic,
	    total = IntInf.zero,
	    totalGC = IntInf.zero}

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
		else Error.bug "bad header"
	     val kind =
		case In.inputLine ins of
		   "alloc\n" => Kind.Alloc
		 | "time\n" => Kind.Time
		 | _ => Error.bug "invalid profile kind"
	     val style =
		case In.inputLine ins of
		   "current\n" => Style.Current
		 | "stack\n" => Style.Stack
		 | _ => Error.bug "invalid profile style"
	     fun line () = String.dropSuffix (In.inputLine ins, 1)
	     val magic =
		case Word.fromString (line ()) of
		   NONE => Error.bug "invalid magic"
		 | SOME w => w
	     fun s2i s =
		case IntInf.fromString s of
		   NONE => Error.bug "invalid count"
		 | SOME i => i
	     val (total, totalGC) =
		case String.tokens (line (), Char.isSpace) of
		   [total, totalGC] => (s2i total, s2i totalGC)
		 | _ => Error.bug "invalid totals"
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
		      (getCounts
		       (fn s =>
			case String.tokens (s, Char.isSpace) of
			   [c, s, sGC] =>
			      {current = s2i c,
			       stack = s2i s,
			       stackGC = s2i sGC}
			 | _ =>
			      Error.bug
			      (concat ["strange line: ",
				       String.dropSuffix (s, 1)])))
	  in
	     T {counts = counts,
		kind = kind,
		magic = magic,
		total = total,
		totalGC = totalGC}
	  end)
   
      fun merge (T {counts = c, kind = k, magic = m, total = t, totalGC = g},
		 T {counts = c', kind = k', magic = m', total = t', totalGC = g',
		    ...}): t =
	 if m <> m'
	    then Error.bug "wrong magic number"
	 else
	    T {counts = Counts.merge (c, c'),
	       kind = Kind.merge (k, k'),
	       magic = m,
	       total = IntInf.+ (t, t'),
	       totalGC = IntInf.+ (g, g')}
   end

structure Atomic =
   struct
      datatype t =
	 Name of string * Regexp.Compiled.t
       | Thresh of real
       | ThreshGC of real
       | ThreshStack of real

      val toSexp: t -> Sexp.t =
	 fn a =>
	 let
	    datatype z = datatype Sexp.t
	 in
	    case a of
	       Name (s, _) => String s
	     | Thresh x => List [Atom "thresh", Atom (Real.toString x)]
	     | ThreshGC x => List [Atom "thresh-gc", Atom (Real.toString x)]
	     | ThreshStack x =>
		  List [Atom "thresh-stack", Atom (Real.toString x)]
	 end
   end

structure NodePred =
   struct
      datatype t =
	 All
       | And of t vector
       | Atomic of Atomic.t
       | Not of t
       | Or of t vector
       | PathFrom of t
       | PathTo of t
       | Pred of t
       | Succ of t

      val rec toSexp: t -> Sexp.t =
	 fn p =>
	 let
	    datatype z = datatype Sexp.t
	    fun nAry (name, ps) =
	       List (Atom name :: Vector.toListMap (ps, toSexp))
	    fun unary (name, p) =
	       List [Atom name, toSexp p]
	 in
	    case p of
	       All => Sexp.Atom "all"
	     | And ps => nAry ("and", ps)
	     | Atomic a => Atomic.toSexp a
	     | Not p => unary ("not", p)
	     | Or ps => nAry ("or", ps)
	     | PathFrom p => unary ("from", p)
	     | PathTo p => unary ("to", p)
	     | Pred p => unary ("pred", p)
	     | Succ p => unary ("succ", p)
	 end

      val layout = Sexp.layout o toSexp

      val fromString: string -> t =
	 fn s =>
	 case Sexp.fromString s of
	    Sexp.Eof => Error.bug "empty"
	  | Sexp.Error s => Error.bug s
	  | Sexp.Sexp s =>
	       let
		  fun parse (s: Sexp.t): t =
		     let
			fun err () = Error.bug (Sexp.toString s)
		     in			   
			case s of
			   Sexp.Atom s =>
			      (case s of
				  "all" => All
				| _ => err ())
			 | Sexp.List ss =>
			      (case ss of
				  [] => err ()
				| s :: ss =>
				     let
					fun nAry f =
					   f (Vector.fromListMap (ss, parse))
					fun unary f =
					   case ss of
					      [s] => f (parse s)
					    | _ => err ()
					fun thresh f =
					   case ss of
					      [Sexp.Atom x] =>
						 (case Real.fromString x of
						     NONE => err ()
						   | SOME x => Atomic (f x))
					    | _ => err ()
					datatype z = datatype Atomic.t
				     in
					case s of
					   Sexp.Atom s =>
					      (case s of
						  "and" => nAry And
						| "from" => unary PathFrom
						| "not" => unary Not
						| "or" => nAry Or
						| "pred" => unary Pred
						| "succ" => unary Succ
						| "thresh" => thresh Thresh
						| "thresh-gc" => thresh ThreshGC
						| "thresh-stack" =>
						     thresh ThreshStack
						| "to" => unary PathTo
						| _ => err ())
					 | _ => err ()
				     end)
			 | Sexp.String s =>
			      (case Regexp.fromString s of
				  NONE => err ()
				| SOME (r, _) =>
				     Atomic
				     (Atomic.Name (s, Regexp.compileNFA r)))
		     end
	       in
		  parse s
	       end

      fun nodes (p: t, g: Graph.t,
		 atomic: Node.t * Atomic.t -> bool): Node.t vector =
	 let
	    val {get = nodeIndex: Node.t -> int,
		 set = setNodeIndex, ...} =
	       Property.getSet (Node.plist,
				Property.initRaise ("index", Node.layout))
	    val nodes = Vector.fromList (Graph.nodes g)
	    val numNodes = Vector.length nodes
	    val _ = Vector.foreachi (nodes, fn (i, n) => setNodeIndex (n, i))
	    val transpose =
	       Promise.lazy
	       (fn () =>
		let
		   val (transpose, {newNode, ...}) = Graph.transpose g
		   val _ =
		      Graph.foreachNode
		      (g, fn n => setNodeIndex (newNode n, nodeIndex n))
		in
		   (transpose, newNode)
		end)
	    fun vectorToNodes (v: bool vector): Node.t vector =
	       Vector.keepAllMapi
	       (v, fn (i, b) =>
		if b
		   then SOME (Vector.sub (nodes, i))
		else NONE)
	    val all = Promise.lazy (fn () =>
				    Vector.tabulate (numNodes, fn _ => true))
	    val none = Promise.lazy (fn () =>
				     Vector.tabulate (numNodes, fn _ => false))
	    fun loop (p: t): bool vector =
	       case p of
		  All => all ()
		| And ps =>
		     Vector.fold (ps, all (), fn (p, v) =>
				  Vector.map2 (v, loop p, fn (b, b') =>
					       b andalso b'))
		| Atomic a => Vector.map (nodes, fn n => atomic (n, a))
		| Not p => Vector.map (loop p, not)
		| Or ps =>
		     Vector.fold (ps, none (), fn (p, v) =>
				  Vector.map2 (v, loop p, fn (b, b') =>
					       b orelse b'))
		| PathFrom p => path (p, (g, fn n => n))
		| PathTo p => path (p, transpose ())
		| Pred p =>
		     let
			val ns = vectorToNodes (loop p)
			val {destroy, get, set, ...}  =
			   Property.destGetSetOnce
			   (Node.plist, Property.initConst false)
			val _ = Vector.foreach (ns, fn n => set (n, true))
			val v =
			   Vector.map
			   (nodes, fn n =>
			    get n orelse
			    List.exists (Node.successors n, get o Edge.to))
			val _ = destroy ()
		     in
			v
		     end
		| Succ p =>
		     let
			val a = Array.array (numNodes, false)
			fun yes n = Array.update (a, nodeIndex n, true)
			val _ =
			   Vector.foreach
			   (vectorToNodes (loop p), fn n =>
			    (yes n
			     ; List.foreach (Node.successors n, yes o Edge.to)))
		     in
			Vector.fromArray a
		     end
	    and path (p: t, (g: Graph.t, getNode)): bool vector =
	       let
		  val roots = vectorToNodes (loop p)
		  val a = Array.array (numNodes, false)
		  val _ =
		     Graph.dfsNodes
		     (g,
		      Vector.toListMap (roots, getNode),
		      Graph.DfsParam.startNode (fn n =>
						Array.update
						(a, nodeIndex n, true)))
	       in
		  Vector.fromArray a
	       end
	    val v = loop p
	 in
	    vectorToNodes v
	 end
   end
   
val graphPred: NodePred.t option ref = ref NONE

fun display (AFile.T {callGraph, name = aname, sources, ...},
	     ProfFile.T {counts, kind, total, totalGC, ...}): unit =
   let
      val {get = nodeInfo: Node.t -> {keep: bool ref,
				      mayKeep: (Atomic.t -> bool) ref,
				      options: Dot.NodeOption.t list ref}, ...} =
	 Property.get (Node.plist,
		       Property.initFun (fn _ => {keep = ref false,
						  mayKeep = ref (fn _ => false),
						  options = ref []}))
      val graph = Graph.new ()
      val ticksPerSecond = 100.0
      val thresh = !thresh
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
			    | Kind.Empty => []
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
	  | Counts.Empty => false
	  | Counts.Stack _ => true
      fun doit (v, f) =
	 Vector.mapi
	 (v, fn (i, x) =>
	  let
	     val {per, perGC, perStack, row, sortPer} = f x
	     val showInTable =
		per > 0.0
		andalso (per >= thresh
			 orelse (not profileStack andalso i = sourcesIndexGC))
	     val {node, source, ...} = Vector.sub (sources, i)
	     val {mayKeep, options, ...} = nodeInfo node
	     val _ =
		mayKeep :=
		(fn a =>
		 let
		    datatype z = datatype Atomic.t
		 in
		    case a of
		       Name (_, rc) =>
			  (case (Regexp.Compiled.findShort
				 (rc, Source.toString source, 0)) of
			      NONE => false
			    | SOME _ => true)
		     | Thresh x => per >= x
		     | ThreshGC x => perGC >= x
		     | ThreshStack x => perStack >= x
		 end)
	     val _ = 
		options :=
		List.append
		([Dot.NodeOption.Label
		  (Source.toDotLabel source
		   @ (if per > 0.0
			 then [(concat (List.separate (row, " ")),
				Dot.Center)]
		      else [])),
		  Dot.NodeOption.Shape Dot.Box,
		  if !gray
		     then
			Dot.NodeOption.Color (DotColor.gray
					      (100 - (Real.round perStack)))
		  else Dot.NodeOption.Color DotColor.Black],
		 !options)
	  in
	     {sortPer = sortPer,
	      row = Source.toString source :: row,
	      showInTable = showInTable}
	  end)
      val counts =
	 case counts of
	    Counts.Current v =>
	       doit (v, fn z =>
		     let
			val (p, r) = per z
		     in
			{per = p, perGC = 0.0, perStack = 0.0,
			 row = r, sortPer = p}
		     end)
	  | Counts.Empty =>
	       let
		  val (p, r) = per IntInf.zero
	       in
		  doit (Vector.new (Vector.length sources, ()),
			fn () => {per = p, perGC = 0.0, perStack = 0.0,
				  row = r, sortPer = p})
	       end
	  | Counts.Stack v =>
	       doit (v, fn {current, stack, stackGC} =>
		     let
			val (cp, cr) = per current
			val (sp, sr) = per stack
			val (gp, gr) = per stackGC
		     in
			{per = sp, perGC = gp, perStack = sp,
			 row = List.concat [cr, sr, gr],
			 sortPer = cp}
		     end)
      (* Display the subgraph specified by -graph. *)
      val graphPred =
	 case !graphPred of
		 NONE =>
	       let
		  datatype z = datatype NodePred.t
		  datatype z = datatype Atomic.t
		  val p = Atomic (Thresh thresh)
	       in
		  if profileStack
		     then p
		  else PathTo p
	       end
	  | SOME p => p
      val keepNodes =
	 NodePred.nodes
	 (graphPred, callGraph, fn (n, a) => (! (#mayKeep (nodeInfo n))) a)
      val _ = Vector.foreach (keepNodes, fn n =>
			      #keep (nodeInfo n) := true)
      val (subgraph, {newNode, ...}) =
	 Graph.subgraph (callGraph, ! o #keep o nodeInfo)
      val {get = oldNode, set = setOldNode, ...} =
	 Property.getSetOnce (Node.plist,
			      Property.initRaise ("old node", Node.layout))
      val _ =
	 Graph.foreachNode
	 (callGraph, fn n =>
	  if !(#keep (nodeInfo n))
	     then setOldNode (newNode n, n)
	  else ())
      val title =
	 case !title of
	    NONE => concat [aname, " call-stack graph"]
	  | SOME s => s
      val _ = 
	 File.withOut
	 (concat [aname, ".dot"], fn out =>
	  Layout.output
	  (Graph.layoutDot (subgraph,
			    fn _ => {edgeOptions = fn _ => [],
				     nodeOptions =
				     fn n => ! (#options (nodeInfo (oldNode n))),
				     options = [],
				     title = title}),
	   out))
      (* Display the table. *)
      val tableRows =
	 QuickSort.sortVector
	 (Vector.keepAll (counts, #showInTable),
	  fn (z, z') => #sortPer z >= #sortPer z')
      val _ = 
	 print
	 (concat
	  (case kind of
	      Kind.Alloc =>
		 [IntInf.toCommaString total, " bytes allocated (",
		  IntInf.toCommaString totalGC, " bytes by GC)\n"]
	    | Kind.Empty => []
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
      ([(Normal, "graph", " <pred>", "show graph nodes",
	 SpaceString (fn s =>
		      graphPred := SOME (NodePred.fromString s)
		      handle e => usage (concat ["invalid -graph arg: ",
						 Exn.toString e]))),
	(Normal, "graph-title", " <string>", "set call-graph title",
	 SpaceString (fn s => title := SOME s)),
	(Normal, "gray", " {false|true}", "gray nodes according to stack %",
	 boolRef gray),
	(Normal, "mlmon", " <file>", "proces mlmon files listed in <file>",
	 SpaceString (fn s =>
		      mlmonFiles :=
		      List.concat [String.tokens (File.contents s, Char.isSpace),
				   !mlmonFiles])),
	(Normal, "raw", " {false|true}", "show raw counts",
	 boolRef raw),
	(Normal, "show-line", " {false|true}", "show line numbers",
	 boolRef showLine),
	(Normal, "thresh", " {0|1|...|100}", "only show counts above threshold",
	 Real (fn x => if x < 0.0 orelse x > 100.0
			 then usage "invalid -thresh"
		      else thresh := x)),
	(Normal, "tolerant", " {false|true}", "ignore broken mlmon files",
	 boolRef tolerant)],
       fn (style, name, arg, desc, opt) =>
       {arg = arg, desc = desc, name = name, opt = opt, style = style})
   end

val mainUsage = "mlprof [option ...] a.out [mlmon.out ...]"
val {parse, usage} =
   Popt.makeUsage {mainUsage = mainUsage,
		   makeOptions = makeOptions,
		   showExpert = fn () => false}

val die = Process.fail
   
fun commandLine args =
   let
      val rest = parse args
    in
       case rest of
	  Result.No msg => usage msg
	| Result.Yes (afile :: files) =>
	     let
		val mlmonFiles = files @ !mlmonFiles 
		val aInfo =
		   AFile.new {afile = afile}
		   handle e => die (concat ["error in ", afile, ": ",
					    Exn.toString e])
		val _ =
		   if debug
		      then
			 (print "AFile:\n"
			  ; Layout.outputl (AFile.layout aInfo, Out.standard))
		   else ()
		val profFile =
		   List.fold
		   (mlmonFiles, ProfFile.empty aInfo,
		    fn (mlmonfile, profFile) =>
		    ProfFile.merge
		    (profFile, ProfFile.new {mlmonfile = mlmonfile})
		    handle e =>
		       let
			  val msg =
			     concat ["error in ", mlmonfile, ": ",
				     Exn.toString e]
		       in
			  if !tolerant
			     then
				(Out.outputl (Out.error, msg)
				 ; profFile)
			  else die msg
		       end)
		val _ =
		   if debug
		      then
			 (print "ProfFile:\n"
			  ; Layout.outputl (ProfFile.layout profFile,
					    Out.standard))
		   else ()
		val _ = display (aInfo, profFile)
	     in
		()
	     end
	| Result.Yes _ => usage "wrong number of args"
   end

val main = Process.makeMain commandLine

end
