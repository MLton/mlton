functor Profile (S: PROFILE_STRUCTS): PROFILE = 
struct

open S
open Rssa

type sourceSeq = int list

structure InfoNode =
   struct
      datatype t = T of {index: int,
			 info: SourceInfo.t,
			 successors: t list ref}

      local
	 fun make f (T r) = f r
      in
	 val index = make #index
	 val info = make #info
      end

      fun layout (T {index, info, ...}) =
	 Layout.record [("index", Int.layout index),
			("info", SourceInfo.layout info)]

      fun equals (n: t, n': t): bool = index n = index n'

      fun call {from = T {successors, ...}, to} =
	 if List.exists (!successors, fn n => equals (n, to))
	    then ()
	 else List.push (successors, to)

      val call =
	 Trace.trace ("InfoNode.call",
		      fn {from, to} =>
		      Layout.record [("from", layout from),
				     ("to", layout to)],
		      Unit.layout)
	 call
   end

structure FuncInfo =
   struct
      datatype t = T of {callers: InfoNode.t list ref,
			 enters: InfoNode.t list ref,
			 seen: bool ref,
			 tailCalls: t list ref}

      fun new () = T {callers = ref [],
		      enters = ref [],
		      seen = ref false,
		      tailCalls = ref []}
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
	    sourceSeqs = Vector.new0 (),
	    sourceSuccessors = Vector.new0 (),
	    sources = Vector.new0 ()}
   else
   let
      val Program.T {functions, main, objectTypes} = program
      val debug = false
      val profile = !Control.profile
      val profileAlloc: bool = profile = Control.ProfileAlloc
      val profileStack: bool = !Control.profileStack
      val profileTime: bool = profile = Control.ProfileTime
      val frameProfileIndices = ref []
      local
	 val table: InfoNode.t HashSet.t =
	    HashSet.new {hash = SourceInfo.hash o InfoNode.info}
	 val c = Counter.new 0
	 val sourceInfos = ref []
      in
	 fun sourceInfoNode (si: SourceInfo.t) =
	    HashSet.lookupOrInsert
	    (table, SourceInfo.hash si,
	     fn InfoNode.T {info = si', ...} => SourceInfo.equals (si, si'),
	     fn () => let
			 val _ = List.push (sourceInfos, si)
			 val index = Counter.next c
		      in
			 InfoNode.T {index = index,
				     info = si,
				     successors = ref []}
		      end)
	 val sourceInfoIndex = InfoNode.index o sourceInfoNode
	 fun firstEnter (ps: Push.t list): InfoNode.t option =
	    List.peekMap (ps, fn p =>
			  case p of
			     Push.Enter n => SOME n
			   | _ => NONE)
	 fun makeSources () = Vector.fromListRev (!sourceInfos)
      end
      (* unknown must be 0, which == SOURCES_INDEX_UNKNOWN from gc.h *)
      val unknownInfoNode = sourceInfoNode SourceInfo.unknown
      val unknownIndex = InfoNode.index unknownInfoNode
      (* gc must be 1 which == SOURCES_INDEX_GC from gc.h *)
      val _ = sourceInfoIndex SourceInfo.gc
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
	       val s = Vector.fromListRev s
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
      val sourceSeqIndex =
	 fn [] => unknownSourceSeq
	  | s => sourceSeqIndex s
      val {get = labelInfo: Label.t -> {block: Block.t,
					visited: bool ref},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce
	 (Label.plist, Property.initRaise ("info", Label.layout))
      val labels = ref []
      fun profileLabelIndex (sourceSeqsIndex: int): Statement.t =
	 let
	    val l = ProfileLabel.new ()
	    val _ = List.push (labels, {label = l,
					sourceSeqsIndex = sourceSeqsIndex})
	 in
	    Statement.ProfileLabel l
	 end
      fun profileLabel (sourceSeq: int list): Statement.t =
	 profileLabelIndex (sourceSeqIndex sourceSeq)
      local
	 val {get: Func.t -> FuncInfo.t, ...} =
	    Property.get (Func.plist, Property.initFun (fn _ => FuncInfo.new ()))
      in
	 val funcInfo = get
	 fun addFuncEdges () =
	    (* Don't need to add edges for main because no one calls it. *)
	    List.foreach
	    (functions, fn f =>
	     let
		val allSeen: bool ref list ref = ref []
		val func = Function.name f
		val fi as FuncInfo.T {callers, ...} = get func
		(* Add edges from all the callers to the enters in f and all
		 * functions that f tail calls.
		 *)
		fun call (FuncInfo.T {enters, seen, tailCalls, ...}): unit =
		   if !seen
		      then ()
		   else
		      let
			 val _ = seen := true
			 val _ = List.push (allSeen, seen)
			 val _ = 
			    List.foreach
			    (!callers, fn from =>
			     List.foreach
			     (!enters, fn to =>
			      InfoNode.call {from = from, to = to}))
		      in
			 List.foreach (!tailCalls, call)
		      end
		val _ = call fi
		val _ = List.foreach (!allSeen, fn r => r := false)
	     in
		()
	     end)
      end
      fun doFunction (f: Function.t): Function.t =
	 let
	    val {args, blocks, name, raises, returns, start} = Function.dest f
	    val FuncInfo.T {enters, tailCalls, ...} = funcInfo name
	    fun enter (ps: Push.t list, si: SourceInfo.t): Push.t list * bool =
	       let
		  val node = sourceInfoNode si
		  fun yes () = (Push.Enter node :: ps, true)
	       in
		  case firstEnter ps of
		     NONE => (List.push (enters, node)
			      ; yes ())
		   | SOME (node' as InfoNode.T {index, ...}) =>
			if not (SourceInfo.equals (si, SourceInfo.unknown))
			   andalso (not (SourceInfo.isBasis si)
				    orelse index = mainIndex
				    orelse index = unknownIndex)
			   then (InfoNode.call {from = node', to = node}
				 ; yes ())
			else (Push.Skip si :: ps, false)
	       end
	    val _ =
	       Vector.foreach
	       (blocks, fn block as Block.T {label, ...} =>
		setLabelInfo (label, {block = block,
				      visited = ref false}))
	    val blocks = ref []
	    datatype z = datatype Statement.t
	    datatype z = datatype ProfileExp.t
	    fun setCurrentSource (n: int): Statement.t =
	       Statement.Move
	       {dst = Operand.Runtime Runtime.GCField.CurrentSource,
		src = Operand.word (Word.fromInt n)}
	    fun backward {args,
			  kind,
			  label,
			  needsCurrentSource,
			  sourceSeq: int list,
			  statements: Statement.t list,
			  transfer: Transfer.t}: unit =
	       let
		  val (_, npl, sourceSeq, statements) =
		     List.fold
		     (statements,
		      (needsCurrentSource, true, sourceSeq, []),
		      fn (s, (ncs, npl, sourceSeq, ss)) =>
		      case s of
			 Object _ => (true, true, sourceSeq, s :: ss)
		       | Profile ps =>
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
			       val ss =
				  if profileAlloc andalso needsCurrentSource
				     then (setCurrentSource
					   (sourceSeqIndex sourceSeq) :: ss)
				  else ss
			    in
			       (false, false, sourceSeq', ss)
			    end
		       | _ => (ncs, true, sourceSeq, s :: ss))
		  val {args, kind, label} =
		     if profileStack andalso (case kind of
						 Kind.Cont _ => true
					       | Kind.Handler => true
					       | _ => false)
			then
			   let
			      val func = CFunction.profileLeave
			      val newLabel = Label.newNoname ()
			      val index = sourceSeqIndex sourceSeq
			      val _ =
				 List.push
				 (blocks,
				  Block.T
				  {args = args,
				   kind = kind,
				   label = label,
				   statements =
				   if profileTime
				      then Vector.new1 (profileLabelIndex index)
				   else Vector.new0 (),
				   transfer = 
				   Transfer.CCall
				   {args = (Vector.new1
					    (Operand.word (Word.fromInt index))),
				    func = func,
				    return = SOME newLabel}})
			   in
			      {args = Vector.new0 (),
			       kind = Kind.CReturn {func = func},
			       label = newLabel}
			   end
		     else {args = args, kind = kind, label = label}
		  val statements =
		     if profileTime andalso npl
			then profileLabel sourceSeq :: statements
		     else statements
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
	    fun profileEnter (sourceSeq: int list,
			      transfer: Transfer.t): Transfer.t =
	       let
		  val func = CFunction.profileEnter
		  val newLabel = Label.newNoname ()
		  val index = sourceSeqIndex sourceSeq
		  val statements =
		     if profileTime
			then Vector.new1 (profileLabelIndex index)
		     else Vector.new0 ()
		  val _ =
		     List.push
		     (blocks,
		      Block.T {args = Vector.new0 (),
			       kind = Kind.CReturn {func = func},
			       label = newLabel,
			       statements = statements,
			       transfer = transfer})
	       in
		  Transfer.CCall
		  {args = Vector.new1 (Operand.word (Word.fromInt index)),
		   func = func,
		   return = SOME newLabel}
	       end
	    fun needsCurrentSource (f: CFunction.t): bool =
	       (profileAlloc andalso CFunction.needsCurrentSource f)
	       orelse profileTime
	    fun goto (l: Label.t, pushes: Push.t list): unit =
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
				      List.layout Push.layout pushes,
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
			   if profileStack andalso Kind.isFrame kind
			      then List.push (frameProfileIndices,
					      (label,
					       sourceSeqIndex
					       (Push.toSources pushes)))
			   else ()
			fun maybeSplit {args, bytesAllocated, kind, label,
					pushes: Push.t list,
					statements} =
			   if profileAlloc andalso bytesAllocated > 0
			      then
				 let
				    val newLabel = Label.newNoname ()
				    val func = CFunction.profileInc
				    val transfer =
				       Transfer.CCall
				       {args = (Vector.new1
						(Operand.word
						 (Word.fromInt bytesAllocated))),
					func = func,
					return = SOME newLabel}
				    val sourceSeq = Push.toSources pushes
				    val _ =
				       backward {args = args,
						 kind = kind,
						 label = label,
						 needsCurrentSource = true,
						 sourceSeq = sourceSeq,
						 statements = statements,
						 transfer = transfer}
				 in
				    {args = Vector.new0 (),
				     bytesAllocated = 0,
				     kind = Kind.CReturn {func = func},
				     label = newLabel,
				     statements = []}
				 end
			   else {args = args,
				 bytesAllocated = 0,
				 kind = kind,
				 label = label,
				 statements = statements}
			val statements = Vector.toList statements
			val statements =
			   if (case kind of
				  Kind.CReturn {func, ...} =>
				     needsCurrentSource func
				| _ => false)
			      then setCurrentSource ~1 :: statements
			   else statements
			val {args, bytesAllocated, kind, label, pushes,
			     statements} =
			   List.fold
			   (statements,
			    {args = args,
			     bytesAllocated = 0,
			     kind = kind,
			     label = label,
			     pushes = pushes,
			     statements = []},
			    fn (s, {args, bytesAllocated, kind, label,
				    pushes: Push.t list,
				    statements}) =>
			    (if not debug
				then ()
			     else
				let
				   open Layout
				in
				   outputl
				   (seq [List.layout Push.layout pushes,
					 str " ",
					 Statement.layout s],
				    Out.error)
				end
			     ;
			    case s of
			       Object {size, ...} =>
				  {args = args,
				   bytesAllocated = bytesAllocated + size,
				   kind = kind,
				   label = label,
				   pushes = pushes,
				   statements = s :: statements}
			     | Profile ps =>
				  let
				     val {args, bytesAllocated, kind, label,
					  statements} =
					maybeSplit
					{args = args,
					 bytesAllocated = bytesAllocated,
					 kind = kind,
					 label = label,
					 pushes = pushes,
					 statements = statements}
				     datatype z = datatype ProfileExp.t
				     val (pushes, keep) =
					case ps of
					   Enter si => enter (pushes, si)
					 | Leave si =>
					      (case pushes of
						  [] =>
						     Error.bug "unmatched Leave"
						| p :: pushes' =>
						     let
							val (keep, isOk) =
							   case p of
							      Push.Enter
							      (InfoNode.T
							       {index, ...}) =>
								 (true,
								  index = sourceInfoIndex si)
							    | Push.Skip si' =>
								 (false,
								  SourceInfo.equals (si, si'))
						     in
							if isOk
							   then (pushes', keep)
							else Error.bug "mismatched Leave"
						     end)
				     val statements =
					if keep
					   then s :: statements
					else statements
				  in
				     {args = args,
				      bytesAllocated = bytesAllocated,
				      kind = kind,
				      label = label,
				      pushes = pushes,
				      statements = statements}
				  end
			     | _ =>
				  {args = args,
				   bytesAllocated = bytesAllocated,
				   kind = kind,
				   label = label,
				   pushes = pushes,
				   statements = s :: statements})
			    )
			val _ =
			   Transfer.foreachLabel
			   (transfer, fn l => goto (l, pushes))
			val {args, kind, label, statements, ...} =
			   maybeSplit {args = args,
				       bytesAllocated = bytesAllocated,
				       kind = kind,
				       label = label,
				       pushes = pushes,
				       statements = statements}
			val sourceSeq = Push.toSources pushes
			val (statements, transfer) =
			   case transfer of
			      Transfer.CCall {func, ...} =>
				 if needsCurrentSource func
				    then
				       let
					  val si =
					     SourceInfo.fromString
					     (concat ["<",
						      CFunction.name func,
						      ">"])
					  val set =
					     setCurrentSource
					     (sourceSeqIndex
					      (Push.toSources
					       (#1 (enter (pushes, si)))))
				       in
					  (set :: statements, transfer)
				       end
				 else (statements, transfer)
			    | Transfer.Call {func, return, ...} =>
				 let
				    val fi as FuncInfo.T {callers, ...} =
				       funcInfo func
				 in
				    case return of
				       Return.NonTail _ =>
					  let
					     val _ =
						Option.app
						(firstEnter pushes,
						 fn n => List.push (callers, n))
					  in
					     (statements,
					      if profileStack
						 then (profileEnter
						       (sourceSeq, transfer))
					      else transfer)
					  end
				     | _ =>
					  (List.push (tailCalls, fi)
					   ; (statements, transfer))
				 end
			    | _ => (statements, transfer)
		     in
			backward {args = args,
				  kind = kind,
				  label = label,
				  needsCurrentSource = false,
				  sourceSeq = sourceSeq,
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
      val sources = makeSources ()
      val sourceSuccessors =
	 Vector.map (sources, fn si =>
		     let
			val InfoNode.T {successors, ...} = sourceInfoNode si
		     in
			sourceSeqIndex
			(List.revMap (!successors, InfoNode.index))
		     end)
      (* This must happen after making sourceSuccessors, since that creates
       * new sourceSeqs.
       *)
      val sourceSeqs = makeSourceSeqs ()
   in
      {frameProfileIndices = Vector.fromList (!frameProfileIndices),
       labels = Vector.fromList (!labels),
       program = program,
       sourceSeqs = sourceSeqs,
       sourceSuccessors = sourceSuccessors,
       sources = sources}
   end

end
