functor Profile (S: PROFILE_STRUCTS): PROFILE = 
struct

open S
open Rssa
   
type sourceSeq = int list

structure Push =
   struct
      datatype t =
	 Enter of int
       | Skip of int

      fun layout z =
	 let
	    open Layout
	 in
	    case z of
	       Enter i => seq [str "Enter ", Int.layout i]
	     | Skip i => seq [str "Skip ", Int.layout i]
	 end

      fun toSources (ps: t list): int list =
	 List.fold (rev ps, [], fn (p, ac) =>
		    case p of
		       Enter i => i :: ac
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
      val debug = false
      val profile = !Control.profile
      val profileAlloc: bool = profile = Control.ProfileAlloc
      val profileTime: bool = profile = Control.ProfileTime
      val frameProfileIndices = ref []
      local
	 val table: {index: int,
		     info: SourceInfo.t} HashSet.t =
	    HashSet.new {hash = SourceInfo.hash o #info}
	 val c = Counter.new 0
	 val sourceInfos = ref []
      in
	 fun sourceInfoIndex (si: SourceInfo.t): int =
	    #index
	    (HashSet.lookupOrInsert
	     (table, SourceInfo.hash si,
	      fn {info = si', ...} => SourceInfo.equals (si, si'),
	      fn () => let
			  val _ = List.push (sourceInfos, si)
			  val index = Counter.next c
			  val _ =
			     if not debug
				then ()
			     else
			     let
				open Layout
			     in
				outputl (seq [Int.layout index,
					      str " ",
					      SourceInfo.layout si],
					 Out.error)
			     end
		       in
			 {index = index,
			  info = si}
		       end))
	 fun makeSources () = Vector.fromListRev (!sourceInfos)
      end
      val mainIndex = sourceInfoIndex SourceInfo.main
      val unknownIndex = sourceInfoIndex SourceInfo.unknown
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
      val sourceSeqIndex =
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
	    val index = sourceSeqIndex sourceSeq
	    val l = ProfileLabel.new ()
	    val _ = List.push (labels, {label = l,
					sourceSeqsIndex = index})
	 in
	    Statement.ProfileLabel l
	 end
      fun shouldPush (si: SourceInfo.t, ps: Push.t list): bool =
	 case List.peekMap (ps, fn Push.Enter i => SOME i | _ => NONE) of
	    NONE => true
	  | SOME i =>
	       not (SourceInfo.isBasis si)
	       orelse i = mainIndex
	       orelse i = unknownIndex
      fun doFunction (f: Function.t): Function.t =
	 let
	    val {args, blocks, name, raises, returns, start} = Function.dest f
	    val _ =
	       Vector.foreach
	       (blocks, fn block as Block.T {label, ...} =>
		setLabelInfo (label, {block = block,
				      visited = ref false}))
	    val blocks = ref []
	    datatype z = datatype Statement.t
	    datatype z = datatype ProfileStatement.t
	    fun backward {args,
			  kind,
			  label,
			  needsProfileAllocIndex,
			  profileInfo,
			  sourceSeq,
			  statements: Statement.t list,
			  transfer: Transfer.t}: unit =
	       let
		  val (_, npl, sourceSeq, statements) =
		     List.fold
		     (statements,
		      (needsProfileAllocIndex, true, sourceSeq, []),
		      fn (s, (npai, npl, sourceSeq, ss)) =>
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
				  if profileAlloc andalso needsProfileAllocIndex
				     then
					Statement.Move
					{dst = (Operand.Runtime
						Runtime.GCField.ProfileAllocIndex),
					 src = (Operand.word
						(Word.fromInt
						 (sourceSeqIndex sourceSeq)))}
					:: ss
				  else ss
			    in
			       (false, false, sourceSeq', ss)
			    end
		       | _ => (npai, true, sourceSeq, s :: ss))
		  val statements =
		     if profileTime andalso npl
			then profileLabel sourceSeq :: statements
		     else statements
	       in		       
		  List.push (blocks,
			     Block.T {args = args,
				      kind = kind,
				      label = label,
				      profileInfo = profileInfo,
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
			val Block.T {args, kind, label, profileInfo, statements,
				     transfer, ...} = block
			val _ =
			   if Kind.isFrame kind
			      then List.push (frameProfileIndices,
					      (label,
					       sourceSeqIndex
					       (Push.toSources sourceSeq)))
			   else ()
			fun maybeSplit {args, bytesAllocated, kind, label,
					sourceSeq: Push.t list,
					statements} =
			   if profileAlloc andalso bytesAllocated > 0
			      then
				 let
				    val newLabel = Label.newNoname ()
				    val func = CFunction.profileAllocInc
				    val transfer =
				       Transfer.CCall
				       {args = (Vector.new1
						(Operand.word
						 (Word.fromInt bytesAllocated))),
					func = func,
					return = SOME newLabel}
				    val sourceSeq = Push.toSources sourceSeq
				    val _ =
				       backward {args = args,
						 kind = kind,
						 label = label,
						 needsProfileAllocIndex = true,
						 profileInfo = profileInfo,
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
			val {args, bytesAllocated, kind, label, sourceSeq,
			     statements} =
			   Vector.fold
			   (statements,
			    {args = args,
			     bytesAllocated = 0,
			     kind = kind,
			     label = label,
			     sourceSeq = sourceSeq,
			     statements = []},
			    fn (s, {args, bytesAllocated, kind, label,
				    sourceSeq: Push.t list,
				    statements}) =>
			    (if not debug
				then ()
			     else
				let
				   open Layout
				in
				   outputl
				   (seq [List.layout Push.layout sourceSeq,
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
				   sourceSeq = sourceSeq,
				   statements = s :: statements}
			     | Profile ps =>
				  let
				     datatype z = datatype ProfileStatement.t
				     val {args, bytesAllocated, kind, label,
					  statements} =
					maybeSplit
					{args = args,
					 bytesAllocated = bytesAllocated,
					 kind = kind,
					 label = label,
					 sourceSeq = sourceSeq,
					 statements = statements}
				     val (keep, sourceSeq) =
					case ps of
					   Enter si =>
					      let
						 val i = sourceInfoIndex si
					      in
						 if shouldPush (si, sourceSeq)
						    then (true,
							  Push.Enter i
							  :: sourceSeq)
						 else (false,
						       Push.Skip i :: sourceSeq)
					      end
					 | Leave si =>
					      (case sourceSeq of
						  [] =>
						     Error.bug "unmatched Leave"
						| p :: sourceSeq' =>
						     let
							val (keep, i) =
							   case p of
							      Push.Enter i =>
								 (true, i)
							    | Push.Skip i =>
								 (false, i)
						     in
							if i = sourceInfoIndex si
							   then (keep, sourceSeq')
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
				      sourceSeq = sourceSeq,
				      statements = statements}
				  end
			     | _ =>
				  {args = args,
				   bytesAllocated = bytesAllocated,
				   kind = kind,
				   label = label,
				   sourceSeq = sourceSeq,
				   statements = s :: statements})
			    )
			val _ =
			   Transfer.foreachLabel
			   (transfer, fn l => goto (l, sourceSeq))
			val npai =
			   case transfer of
			      Transfer.CCall {func, ...} =>
				 CFunction.needsProfileAllocIndex func
			    | _ => false
			val {args, kind, label, statements, ...} =
			   maybeSplit {args = args,
				       bytesAllocated = bytesAllocated,
				       kind = kind,
				       label = label,
				       sourceSeq = sourceSeq,
				       statements = statements}
		     in
			backward {args = args,
				  kind = kind,
				  label = label,
				  needsProfileAllocIndex = npai,
				  profileInfo = profileInfo,
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
      val Program.T {functions, main, objectTypes} = program
      val program = Program.T {functions = List.revMap (functions, doFunction),
			       main = doFunction main,
			       objectTypes = objectTypes}
   in
      {frameProfileIndices = Vector.fromList (!frameProfileIndices),
       labels = Vector.fromList (!labels),
       program = program,
       sources = makeSources (),
       sourceSeqs = makeSourceSeqs ()}
   end

end
