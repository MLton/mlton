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

val raw = ref false
val thresh: int ref = ref 0

val die = Process.fail
   
structure AFile =
   struct
      datatype t = T of {magic: word,
			 name: string,
			 sourceSuccessors: int vector vector,
			 sources: string vector}

      fun layout (T {magic, name, sourceSuccessors, sources}) =
	 Layout.record [("name", String.layout name),
			("magic", Word.layout magic),
			("sources", Vector.layout String.layout sources),
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
	     val sources =
		Vector.tabulate (sourcesLength, fn _ =>
				 String.dropSuffix (line (), 1))
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
      datatype t = Cumulative | Current

      val toString =
	 fn Cumulative => "Cumulative"
	  | Current => "Current"

      val layout = Layout.str o toString
   end

structure ProfFile =
   struct
      datatype t = T of {counts: IntInf.t vector,
			 kind: Kind.t,
			 magic: word,
			 total: IntInf.t}

      local
	 fun make f (T r) = f r
      in
	 val kind = make #kind
      end

      fun layout (T {counts, kind, magic, total}) =
	 Layout.record [("kind", Kind.layout kind),
			("magic", Word.layout magic),
			("total", IntInf.layout total),
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
	     val style =
		case In.inputLine ins of
		   "cumulative\n" => Style.Cumulative
		 | "current\n" => Style.Current
		 | _ => die "invalid profile style"
	     fun line () = String.dropSuffix (In.inputLine ins, 1)
	     val magic = valOf (Word.fromString (line ()))
	     val total = valOf (IntInf.fromString (line ()))
	     fun loop ac =
		case In.inputLine ins of
		   "" => Vector.fromListRev ac
		 | s => loop (valOf (IntInf.fromString s) :: ac)
	     val counts = loop []
	  in
	     T {counts = counts,
		kind = kind,
		magic = magic,
		total = total}
	  end)

      val new =
	 Trace.trace ("ProfFile.new", File.layout o #mlmonfile, layout) new

      fun merge (T {counts = c, kind = k, magic = m, total = t},
		 T {counts = c', magic = m', total = t', ...}): t =
	 if m <> m'
	    then die "incompatible mlmon files"
	 else
	    T {counts = Vector.map2 (c, c', IntInf.+),
	       kind = k,
	       magic = m,
	       total = IntInf.+ (t, t')}
   end

structure Graph = DirectedGraph
local
   open Graph
in
   structure Node = Node
end

fun display (AFile.T {name = aname, sources, sourceSuccessors, ...},
	     ProfFile.T {counts, kind, total, ...}): unit =
   let
      val {get = nodeOptions: Node.t -> Dot.NodeOption.t list ref, ...} =
	 Property.get (Node.plist, Property.initFun (fn _ => ref []))
      val graph = Graph.new ()
      val ticksPerSecond = 100.0
      val thresh = Real.fromInt (!thresh)
      val totalReal = Real.fromIntInf total
      val counts =
	 Vector.mapi
	 (counts, fn (i, ticks) =>
	  let
	     val rticks = Real.fromIntInf ticks
	     val per = 100.0 * rticks / totalReal
	  in
	     if per < thresh
		then NONE
	     else
		let
		   val name = Vector.sub (sources, i)
		   val node = Graph.newNode graph
		   val per =
		      (concat [Real.format (per, Real.Format.fix (SOME 2)),
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
		   val nodeIndex =
		      List.push
		      (nodeOptions node,
		       Dot.NodeOption.Label
		       [(name, Dot.Center),
			(concat (List.separate (per, " ")), Dot.Center)])
		in
		   SOME {node = node,
			 row = name :: per,
			 ticks = ticks}
		end
	  end)
      val _ =
	 Vector.mapi
	 (counts,
	  fn (i, z) =>
	  case z of
	     NONE => ()
	   | SOME {node = from, ...} =>
		Vector.foreach
		(Vector.sub (sourceSuccessors, i), fn j =>
		 case Vector.sub (counts, j) of
		    NONE => ()
		  | SOME {node = to, ...} => 
		       (Graph.addEdge (graph, {from = from, to = to})
			; ())))
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
      val counts = Vector.keepAllMap (counts, fn z => z)
      val counts =
	 QuickSort.sortVector
	 (counts, fn ({ticks = t1, ...}, {ticks = t2, ...}) =>
	  IntInf.>= (t1, t2))
      val _ = 
	 print
	 (concat
	  (case kind of
	      Kind.Alloc => [IntInf.toCommaString total, " bytes allocated\n"]
	    | Kind.Time => 
		 [Real.format (totalReal / ticksPerSecond, 
			       Real.Format.fix (SOME 2)),
		  " seconds of CPU time\n"]))
      val _ =
	 let
	    open Justify
	 in
	    outputTable
	    (table {justs = Left :: List.duplicate (if !raw then 2 else 1,
					            fn () => Right),
		    rows = Vector.toListMap (counts, #row)},
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
      ([(Normal, "raw", " {false|true}", "show raw counts",
	 boolRef raw),
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
