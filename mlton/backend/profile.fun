functor Profile (S: PROFILE_STRUCTS): PROFILE = 
struct

open S
open Rssa
structure Graph = DirectedGraph
local
   open Graph
in
   structure Edge = Edge
   structure Node = Node
end

type sourceSeq = int list

structure InfoNode =
   struct
      datatype t = T of {index: int,
			 info: SourceInfo.t,
			 node: Node.t}

      local
	 fun make f (T r) = f r
      in
	 val index = make #index
	 val info = make #info
	 val node = make #node
      end

      fun layout (T {index, info, ...}) =
	 Layout.record [("index", Int.layout index),
			("info", SourceInfo.layout info)]
   end

structure Push =
   struct
      datatype t =
	 Enter of InfoNode.t
       | Skip of SourceInfo.t

      fun layout z =
	 let
	    open Layout
	 in
	    case z of
	       Enter n => seq [str "Enter ", InfoNode.layout n]
	     | Skip i => seq [str "Skip ", SourceInfo.layout i]
	 end

      fun toSources (ps: t list): int list =
	 List.fold (rev ps, [], fn (p, ac) =>
		    case p of
		       Enter (InfoNode.T {index, ...}) => index :: ac
		     | Skip _ => ac)
   end

fun profile program =
   if !Control.profile = Control.ProfileNone
      then {frameProfileIndices = Vector.new0 (),
	    labels = Vector.new0 (),
	    program = program,
	    sources = Vector.new0 (),
	    sourceSeqs = Vector.new0 ()}
   else
   let
      val Program.T {functions, main, objectTypes} = program
      val debug = false
      val profile = !Control.profile
      val profileAlloc: bool = profile = Control.ProfileAlloc
      val profileTime: bool = profile = Control.ProfileTime
      val frameProfileIndices = ref []
      local
	 val graph = Graph.new ()
	 val {get = nodeOptions, ...} =
	    Property.get (Node.plist, Property.initFun (fn _ => ref []))
	 val table: InfoNode.t HashSet.t =
	    HashSet.new {hash = SourceInfo.hash o InfoNode.info}
	 val c = Counter.new 0
	 val sourceInfos = ref []
      in
	 fun addEdge {from, to} =
	    if List.exists (Node.successors from, fn e =>
			    Node.equals (to, Edge.to e))
	       then ()
	    else (Graph.addEdge (graph, {from = from, to = to}); ())
	 fun sourceInfoNode (si: SourceInfo.t) =
	    HashSet.lookupOrInsert
	    (table, SourceInfo.hash si,
	     fn InfoNode.T {info = si', ...} => SourceInfo.equals (si, si'),
	     fn () => let
			 val _ = List.push (sourceInfos, si)
			 val index = Counter.next c
			 val node = Graph.newNode graph
			 val _ =
			    List.push
			    (nodeOptions node,
			     Dot.NodeOption.label (SourceInfo.toString si))
		      in
			 InfoNode.T {index = index,
				     info = si,
				     node = node}
		      end)
	 val sourceInfoIndex = InfoNode.index o sourceInfoNode
	 fun firstEnter (ps: Push.t list): InfoNode.t option =
	    List.peekMap (ps, fn p =>
			  case p of
			     Push.Enter n => SOME n
			   | _ => NONE)
	 fun saveGraph () =
	    Control.saveToFile
	    ({suffix = "call-graph.dot"},
	     Control.Dot,
	     (),
	     Control.Layout (fn () =>
			     Graph.layoutDot
			     (graph,
			      fn _ => {edgeOptions = fn _ => [],
				       nodeOptions = ! o nodeOptions,
				       options = [],
				       title = "call graph"})))
	 fun makeSources () = Vector.fromListRev (!sourceInfos)
      end
      (* unknown must be 0, which == SOURCES_INDEX_UNKNOWN from gc.h *)
      val unknownInfoNode = sourceInfoNode SourceInfo.unknown
      val unknownIndex = InfoNode.index unknownInfoNode
      (* gc must be 1 which == SOURCES_INDEX_GC from gc.h *)
      val gcIndex = sourceInfoIndex SourceInfo.gc
      val mainIndex = sourceInfoIndex SourceInfo.main
      local
	 val table: {hash: word,
		     index: int,
		     sourceSeq: int vector} HashSet.t =
	    HashSet.new {hash = #hash}
	 val c = Counter.new 0
	 val sourceSeqs: int vector list ref = ref []
      in
	 fun sourceSeqIndex (s: sourceSeq): int =
	    let
	       val s = Vector.fromList s
	       val hash =
		  Vector.fold (s, 0w0, fn (i, w) =>
			       w * 0w31 + Word.fromInt i)
	    in
	       #index
	       (HashSet.lookupOrInsert
		(table, hash,
		 fn {sourceSeq = s', ...} => s = s',
		 fn () => let
			     val _ = List.push (sourceSeqs, s)
			  in
			     {hash = hash,
			      index = Counter.next c,
			      sourceSeq = s}
			  end))
	    end
	 fun makeSourceSeqs () = Vector.fromListRev (!sourceSeqs)
      end
      (* Ensure that SourceInfo unknown is index 0. *)
      val unknownSourceSeq = sourceSeqIndex [sourceInfoIndex SourceInfo.unknown]
      (* Treat the empty source sequence as unknown. *)
      val sourceSeqIndexSafe =
	 fn [] => unknownSourceSeq
	  | s => sourceSeqIndex s
      val {get = labelInfo: Label.t -> {block: Block.t,
					visited: bool ref},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce
	 (Label.plist, Property.initRaise ("info", Label.layout))
      val labels = ref []
      fun profileLabel (sourceSeq: int list): Statement.t =
	 let
	    val index = sourceSeqIndexSafe sourceSeq
	    val l = ProfileLabel.new ()
	    val _ = List.push (labels, {label = l,
					sourceSeqsIndex = index})
	 in
	    Statement.ProfileLabel l
	 end
      fun shouldPush (si: SourceInfo.t, ps: Push.t list): bool =
	 case firstEnter ps of
	    NONE => true
	  | SOME (InfoNode.T {index, ...}) =>
	       not (SourceInfo.isBasis si)
	       orelse index = mainIndex
	       orelse index = unknownIndex
      local
	 val {get: Func.t -> {callees: Node.t list ref,
			      callers: Node.t list ref}, ...} =
	    Property.get (Func.plist,
			  Property.initFun (fn _ => {callers = ref [],
						     callees = ref []}))
      in
	 val funcInfo = get
	 fun addFuncEdges () =
	    (* Don't need to add edges for main because no one calls it. *)
	    List.foreach (functions, fn f =>
			  let
			     val {callers, callees} = get (Function.name f)
			  in
			     List.foreach
			     (!callers, fn from =>
			      List.foreach (!callees, fn to =>
					    addEdge {from = from, to = to}))
			  end)
      end
      fun doFunction (f: Function.t): Function.t =
	 let
	    val {args, blocks, name, raises, returns, start} = Function.dest f
	    val {callees, ...} = funcInfo name
	    val _ =
	       Vector.foreach
	       (blocks, fn block as Block.T {label, ...} =>
		setLabelInfo (label, {block = block,
				      visited = ref false}))
	    val blocks = ref []
	    datatype z = datatype Statement.t
	    datatype z = datatype ProfileExp.t
	    fun backward {args,
			  kind,
			  label,
			  sourceSeq,
			  statements: Statement.t list,
			  transfer: Transfer.t}: unit =
	       let
		  val (npl, sourceSeq, statements) =
		     List.fold
		     (statements,
		      (true, sourceSeq, []), fn (s, (npl, sourceSeq, ss)) =>
		      case s of
			 Profile ps =>
			    let
			       val ss =
				  if profileTime andalso npl
				     then profileLabel sourceSeq :: ss
				  else ss
			       val sourceSeq' = 
				  case ps of
				     Enter si =>
					(case sourceSeq of
					    [] => Error.bug "unmatched Enter"
					  | si' :: sis =>
					       if si' = sourceInfoIndex si
						  then sis
					       else Error.bug "mismatched Enter")
				   | Leave si => sourceInfoIndex si :: sourceSeq
			    in
			       (false, sourceSeq', ss)
			    end
		       | _ => (true, sourceSeq, s :: ss))
		  val statements =
		     if profileTime andalso npl
			then profileLabel sourceSeq :: statements
		     else statements
		  val (args, kind, label) =
		     if profileAlloc
			andalso (case kind of
				    Kind.Cont _ => true
				  | Kind.Handler => true
				  | _ => false)
			then
			   let
			      val newLabel = Label.newNoname ()
			      val func = CFunction.profileAllocSetCurrentSource
			      val sourceIndex =
				 case sourceSeq of
				    [] => unknownIndex
				  | n :: _ => n
			      val _ =
				 List.push
				 (blocks,
				  Block.T
				  {args = args,
				   kind = kind,
				   label = label,
				   statements = Vector.new0 (),
				   transfer =
				   Transfer.CCall {args = (Vector.new1
							   (Operand.word
							    (Word.fromInt
							     sourceIndex))),
						   func = func,
						   return = SOME newLabel}})
			   in
			      (Vector.new0 (),
			       Kind.CReturn {func = func},
			       newLabel)
			   end
		     else (args, kind, label)
	       in		       
		  List.push (blocks,
			     Block.T {args = args,
				      kind = kind,
				      label = label,
				      statements = Vector.fromList statements,
				      transfer = transfer})
	       end
	    val backward =
	       Trace.trace
	       ("Profile.backward",
		fn {statements, sourceSeq, ...} =>
		Layout.tuple [List.layout Int.layout sourceSeq,
			      List.layout Statement.layout statements],
		Unit.layout)
	       backward
	    fun maybeSplit {args,
			    bytesAllocated,
			    enters: InfoNode.t list,
			    kind,
			    label,
			    leaves: InfoNode.t list,
			    maybe: bool,
			    sourceSeq,
			    statements} =
	       if profileAlloc
		  andalso (not (List.isEmpty enters)
			   orelse not (List.isEmpty leaves)
			   orelse maybe)
		  then
		     let
			val newLabel = Label.newNoname ()
			val func = CFunction.profileAllocIncLeaveEnter
			fun ssi (ns: InfoNode.t list): int =
			   sourceSeqIndex (List.revMap (ns, InfoNode.index))
			val enters =
			   (* add the current source to the enters *)
			   (case firstEnter sourceSeq of
			       NONE => unknownInfoNode
			     | SOME n => n) :: enters
			val transfer =
			   Transfer.CCall
			   {args = (Vector.new3
				    (Operand.word (Word.fromInt bytesAllocated),
				     Operand.word (Word.fromInt (ssi leaves)),
				     Operand.word (Word.fromInt (ssi enters)))),
			    func = func,
			    return = SOME newLabel}
			val sourceSeq = Push.toSources sourceSeq
			val _ =
			   backward {args = args,
				     kind = kind,
				     label = label,
				     sourceSeq = sourceSeq,
				     statements = statements,
				     transfer = transfer}
		     in
			{args = Vector.new0 (),
			 bytesAllocated = 0,
			 enters = [],
			 kind = Kind.CReturn {func = func},
			 label = newLabel,
			 leaves = [],
			 statements = []}
		     end
	       else
		  {args = args,
		   bytesAllocated = bytesAllocated,
		   enters = enters,
		   kind = kind,
		   label = label,
		   leaves = leaves,
		   statements = statements}
	    val maybeSplit =
	       Trace.trace
	       ("Profile.maybeSplit",
		fn {enters, leaves, sourceSeq, ...} =>
		Layout.record [("enters", List.layout InfoNode.layout enters),
			       ("leaves", List.layout InfoNode.layout leaves),
			       ("sourceSeq", List.layout Push.layout sourceSeq)],
		Layout.ignore)
	       maybeSplit
	    fun forward {args, kind, label, sourceSeq, statements} =
	       Vector.fold
	       (statements,
		{args = args,
		 bytesAllocated = 0,
		 enters = [],
		 kind = kind,
		 label = label,
		 leaves = [],
		 sourceSeq = sourceSeq,
		 statements = []},
		fn (s, {args, bytesAllocated, enters, kind, label, leaves,
			sourceSeq, statements}) =>
		(
		 if debug
		    then
		       let
			  open Layout
		       in
			  outputl (record
				   [("statement", Statement.layout s),
				    ("enters", List.layout InfoNode.layout enters),
				    ("leaves", List.layout InfoNode.layout leaves)],
				   Out.error)
		       end
		 else ()
		 ;
		 case s of
		    Object {size, ...} =>
		       let
			  val {args, bytesAllocated, enters, kind, label,
			       leaves, statements} =
			     maybeSplit {args = args,
					 bytesAllocated = bytesAllocated,
					 enters = enters,
					 kind = kind,
					 label = label,
					 leaves = leaves,
					 maybe = false,
					 sourceSeq = sourceSeq,
					 statements = statements}
		       in
			  {args = args,
			   bytesAllocated = bytesAllocated + size,
			   enters = enters,
			   kind = kind,
			   label = label,
			   leaves = leaves,
			   sourceSeq = sourceSeq,
			   statements = s :: statements}
		       end
		  | Profile ps =>
		       let
			  val (enters, leaves, sourceSeq, statements) =
			     case ps of
				Enter si =>
				   (if shouldPush (si, sourceSeq)
				       then
					  let
					     val n
						as InfoNode.T {node, ...} =
						sourceInfoNode si
					     val _ = 
						case firstEnter sourceSeq of
						   NONE =>
						      List.push (callees, node)
						    | SOME
						      (InfoNode.T
						       {node = node', ...}) =>
						      addEdge {from = node',
							       to = node}
					  in
					     (n :: enters,
					      leaves,
					      Push.Enter n :: sourceSeq,
					      s :: statements)
					  end
				    else (enters,
					  leaves,
					  Push.Skip si :: sourceSeq,
					  statements))
			      | Leave si =>
				   (case sourceSeq of
				       [] => Error.bug "unmatched Leave"
				     | p :: sourceSeq' =>
					  (case p of
					      Push.Enter (n as InfoNode.T {index, ...}) =>
						 if index = sourceInfoIndex si
						    then
						       let
							  val (enters, leaves) =
							     case enters of
								[] =>
								   ([],
								    n :: leaves)
							      | _ :: enters =>
								   (enters, leaves)
						       in
							  (enters,
							   leaves,
							   sourceSeq',
							   s :: statements)
						       end
						 else Error.bug "mismatched leave"
					    | Push.Skip si' =>
						 if SourceInfo.equals (si, si')
						    then (enters,
							  leaves,
							  sourceSeq',
							  statements)
						 else Error.bug "mismatched leave"))
		       in
			  {args = args,
			   bytesAllocated = bytesAllocated,
			   enters = enters,
			   kind = kind,
			   label = label,
			   leaves = leaves,
			   sourceSeq = sourceSeq,
			   statements = statements}
		       end
		  | _ => {args = args,
			  bytesAllocated = bytesAllocated,
			  enters = enters,
			  kind = kind,
			  label = label,
			  leaves = leaves,
			  sourceSeq = sourceSeq,
			  statements = s :: statements})
		)
	    fun goto (l: Label.t, sourceSeq: Push.t list): unit =
	       let
		  val _ =
		     if not debug
			then ()
		     else
		     let
			open Layout
		     in
			outputl (seq [str "goto (",
				      Label.layout l,
				      str ", ",
				      List.layout Push.layout sourceSeq,
				      str ")"],
				 Out.error)
		     end
		  val {block, visited, ...} = labelInfo l
	       in
		  if !visited
		     then ()
		  else
		     let
			val _ = visited := true
			val Block.T {args, kind, label, statements, transfer,
				     ...} = block
			val _ =
			   if Kind.isFrame kind
			      then List.push (frameProfileIndices,
					      (label,
					       sourceSeqIndexSafe
					       (Push.toSources sourceSeq)))
			   else ()
			val {args, bytesAllocated, enters, kind, label, leaves,
			     sourceSeq, statements} =
			   forward {args = args,
				    kind = kind,
				    label = label,
				    sourceSeq = sourceSeq,
				    statements = statements}
			val {args, kind, label, statements, ...} =
			   maybeSplit {args = args,
				       bytesAllocated = bytesAllocated,
				       enters = enters,
				       kind = kind,
				       label = label,
				       leaves = leaves,
				       maybe = bytesAllocated > 0,
				       sourceSeq = sourceSeq,
				       statements = statements}
			val _ =
			   Transfer.foreachLabel
			   (transfer, fn l => goto (l, sourceSeq))
			(* Record the call for the call graph. *)
			val _ =
			   case transfer of
			      Transfer.Call {func, ...} =>
				 Option.app
				 (firstEnter sourceSeq,
				  fn InfoNode.T {node, ...} =>
				  List.push (#callers (funcInfo func), node))
			    | _ => ()
		     in
			backward {args = args,
				  kind = kind,
				  label = label,
				  sourceSeq = Push.toSources sourceSeq,
				  statements = statements,
				  transfer = transfer}
		     end
	       end
	    val _ = goto (start, [])
	    val blocks = Vector.fromList (!blocks)
	 in
	    Function.new {args = args,
			  blocks = blocks,
			  name = name,
			  raises = raises,
			  returns = returns,
			  start = start}
	 end
      val program = Program.T {functions = List.revMap (functions, doFunction),
			       main = doFunction main,
			       objectTypes = objectTypes}
      val _ = addFuncEdges ()
      val _ = saveGraph ()
   in
      {frameProfileIndices = Vector.fromList (!frameProfileIndices),
       labels = Vector.fromList (!labels),
       program = program,
       sources = makeSources (),
       sourceSeqs = makeSourceSeqs ()}
   end

end
