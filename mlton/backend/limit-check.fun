functor LimitCheck (S: LIMIT_CHECK_STRUCTS): LIMIT_CHECK =
struct

open S
open Rssa

fun reduceSlop (n: int): int =
   if n < Runtime.limitSlop
      then 0
   else n - Runtime.limitSlop

structure Statement =
   struct
      open Statement

      fun objectBytesAllocated s =
	 case s of
	    Statement.Object {numPointers = p, numWordsNonPointers = np, ...} =>
	       Runtime.objectHeaderSize
	       + Runtime.objectSize {numPointers = p,
				     numWordsNonPointers = np}
	  | _ => 0
   end

structure BlockInfo =
   struct
      datatype t = T of {array: {bytesPerElt: int, numElts: Var.t} option,
			 heap: {bytes: int} option,
			 stack: bool}
      
      fun layout (T {array, heap, stack}) =
	 Layout.record
	 [("array", Option.layout
	            (fn {bytesPerElt, numElts, ...} =>
		     Layout.record
		     [("bytesPerElt", Int.layout bytesPerElt),
		      ("numElts", Var.layout numElts)])
		    array),
	  ("heap", Option.layout
	           (fn {bytes, ...} =>
		    Layout.record
		    [("bytes", Int.layout bytes)])
		   heap),
	  ("stack", Bool.layout stack)]
	 
   end

fun insertFunction (f: Function.t,
		    blockInfo: {blockIndex: int} -> BlockInfo.t) =
   let
      val {args, blocks, name, start} = Function.dest f
      val extra = ref []
      fun add {args, blockKind, lcKind, profileInfo, start, success} =
	 let
	    val failure = Label.newNoname ()
	 in
	    extra :=
	    Block.T {args = args,
		     kind = blockKind,
		     label = start,
		     profileInfo = profileInfo,
		     statements = Vector.new0 (),
		     transfer = Transfer.LimitCheck {failure = failure,
						     kind = lcKind,
						     success = success}}
	    :: Block.T {args = Vector.new0 (),
			kind = Kind.Runtime {prim = Prim.gcCollect},
			label = failure,
			profileInfo = profileInfo,
			statements = Vector.new0 (),
			transfer = Transfer.Goto {dst = success,
						  args = Vector.new0 ()}}
	    :: !extra
	 end
      val blocks =
	 Vector.mapi
	 (blocks, fn (i, block as Block.T {args, kind, label, profileInfo,
					   statements, transfer}) =>
	  let
	     val BlockInfo.T {array, heap, stack} =
	        blockInfo {blockIndex = i}
	     val _ = Assert.assert
	             ("LimitCheck.insertFunction: array", fn () =>
		      let
			 fun default () =
			    case array of
			       NONE => true
			     | _ => false
		      in
			if 0 < Vector.length statements
			   then case Vector.sub (statements, 0) of
			           Statement.Array {dst, numBytesNonPointers,
						    numElts, numPointers} =>
				      let
				         val bytesPerElt =
					    if numPointers = 0
					       then numBytesNonPointers
					    else if numBytesNonPointers = 0
						    then numPointers * 
						         Runtime.pointerSize
					    else Error.unimplemented "tricky arrays"
 				      in
				         case array of
					    SOME {bytesPerElt = bytesPerElt',
						  numElts = numElts'} =>
					       bytesPerElt = bytesPerElt'
					       andalso
					       Var.equals (numElts, numElts')
					 | _ => true
				     end
			         | _ => default ()
			else default ()
		      end)
	     val _ = Assert.assert
	             ("LimitCheck.insertFunction: stack", fn () =>
		      if Label.equals (start, label)
			 then stack
		      else not stack)
	     val lcKind =
	        case (array, heap, stack) of
		   (SOME {bytesPerElt, numElts}, SOME {bytes}, stack) =>
		      if bytesPerElt = 0
			 then if bytes = 0
			         then if stack
				         then SOME (LimitCheck.Stack)
				      else NONE
			      else SOME (LimitCheck.Heap {bytes = reduceSlop bytes,
							  stackToo = stack})
		      else SOME (LimitCheck.Array {bytesPerElt = bytesPerElt,
						   extraBytes = reduceSlop bytes,
						   numElts = numElts,
						   stackToo = stack})
		 | (SOME {bytesPerElt, numElts}, NONE, stack) =>
		      if bytesPerElt = 0
			 then if stack
				 then SOME (LimitCheck.Stack)
			      else NONE
		      else SOME (LimitCheck.Array {bytesPerElt = bytesPerElt,
						   extraBytes = 0,
						   numElts = numElts,
						   stackToo = stack})
		| (NONE, SOME {bytes}, stack) =>
		      if bytes = 0
			 then if stack
			         then SOME (LimitCheck.Stack)
			      else NONE
		      else SOME (LimitCheck.Heap {bytes = reduceSlop bytes,
						  stackToo = stack})
		| (NONE, NONE, stack) =>
		      if stack
			 then SOME (LimitCheck.Stack)
		      else NONE
	     fun insert (lcKind: LimitCheck.t) =
		let
		   val success = Label.newNoname ()
		   val _ = add {args = args,
				blockKind = kind,
				lcKind = lcKind,
				profileInfo = profileInfo,
				start = label,
				success = success}
		in
		   Block.T {args = Vector.new0 (),
			    kind = Kind.Jump,
			    label = success,
			    profileInfo = profileInfo,
			    statements = statements,
			    transfer = transfer}
		end
	  in
	     case lcKind of
	        SOME lcKind => insert lcKind
	      | NONE => block
	  end)
      val blocks = Vector.concat [blocks, Vector.fromList (!extra)]
   in
      Function.new {args = args,
		    blocks = blocks,
		    name = name,
		    start = start}
   end

fun insertPerBlock (f: Function.t) =
   let
      val {start, blocks, ...} = Function.dest f
      fun blockInfo {blockIndex} =
	 let 
	    val block as Block.T {label, statements, ...} = 
	       Vector.sub (blocks, blockIndex)
	    val array =
	       let
		 fun default () = NONE
	       in
		  if 0 < Vector.length statements
		     then case Vector.sub (statements, 0) of
		             Statement.Array {dst, numBytesNonPointers,
					      numElts, numPointers} =>
			        let
				   val bytesPerElt =
				      if numPointers = 0
				         then numBytesNonPointers
				      else if numBytesNonPointers = 0
				 	      then numPointers * 
					           Runtime.pointerSize
				      else Error.unimplemented "tricky arrays"
 			        in
				   SOME {bytesPerElt = bytesPerElt,
					 numElts = numElts}
				end 
			   | _ => default ()
		 else default ()
	       end
	    val bytes =
	       Vector.fold
	       (statements, 0, fn (s, ac) =>
		ac + Statement.objectBytesAllocated s)
	    val heap = SOME {bytes = bytes}
	    val stack = Label.equals (start, label)
	 in 
	    BlockInfo.T
	    {array = array,
	     heap = heap,
	     stack = Label.equals (start, label)}
	 end
   in
      insertFunction (f, blockInfo)
   end

structure Graph = DirectedGraph
structure Node = Graph.Node
structure Edge = Graph.Edge
structure Forest = Graph.LoopForest

val traceMaxPath = Trace.trace ("maxPath", Int.layout, Int.layout)

fun insertCoalesce (f: Function.t) =
   let
      val {args, blocks, name, start} = Function.dest f
      val n = Vector.length blocks
      val {get = labelIndex, set = setLabelIndex, rem = remLabelIndex, ...} =
	 Property.getSetOnce
	 (Label.plist, Property.initRaise ("LimitCheck.labelIndex", Label.layout))
      val {get = nodeIndex, set = setNodeIndex, ...} =
	 Property.getSetOnce
	 (Node.plist, Property.initRaise ("LimitCheck.nodeIndex", Node.layout))
      val _ =
	 Vector.foreachi
	 (blocks, fn (i, Block.T {label, ...}) =>
	  setLabelIndex (label, i))
      (* Build the graph. *)
      val g = Graph.new ()
      val nodes = 
	 Vector.tabulate
	 (n, fn i => 
	  let
	     val n = Graph.newNode g
	     val _ = setNodeIndex (n, i)
	  in 
	     n
	  end)
      fun indexNode i = Vector.sub (nodes, i)
      val labelNode = indexNode o labelIndex
      val root = Graph.newNode g
      (* mayHaveCheck == E U D
       *   E = set of entry nodes 
       *     = start, Cont, Handler, Runtime, or Jump that starts with an Array
       *   D = set of decycling nodes
       *)
      val mayHaveCheck =
	 Array.tabulate
	 (n, fn i =>
	  let
	     val Block.T {kind, statements, ...} = Vector.sub (blocks, i)
	     datatype z = datatype Kind.t
	     val b =
		case kind of
		   Cont _ => true
		 | CReturn _ => false
		 | Handler => true
		 | Jump => (0 < Vector.length statements
			    andalso (case Vector.sub (statements, 0) of
					Statement.Array _ => true
				      | _ => false))
		 | Runtime _ => true
	  in
	     b
	  end)
      val _ = Array.update (mayHaveCheck, labelIndex start, true)
      (* Build cfg. *)
      val _ = Graph.addEdge (g, {from = root, to = labelNode start})
      datatype z = datatype Control.limitCheck
      val fullCFG = 
	 case !Control.limitCheck of
	    ExtBasicBlocks => true
	  | LoopHeaders {fullCFG, ...} => fullCFG
	  | _ => Error.bug "LimitCheck.insertCoalesce: fullCFG"
      val _ =
	 Vector.foreachi
	 (blocks, fn (i, Block.T {transfer, ...}) =>
	  let
	     val from = indexNode i
	  in
	     Transfer.foreachLabel
	     (transfer, fn l =>
	      let
		 val i' = labelIndex l
		 val to = indexNode i'
		 fun addEdge from =
		    (Graph.addEdge (g, {from = from, to = to})
		     ; ())
	      in
		 if fullCFG
		    then addEdge from
		 else if Array.sub (mayHaveCheck, i')
		         then addEdge root
		      else addEdge from
	      end)
	  end)
      val objectBytesAllocated =
	 Vector.map
	 (blocks, fn Block.T {statements, ...} =>
	  Vector.fold (statements, 0, fn (s, ac) =>
		       ac + Statement.objectBytesAllocated s))

      fun insertCoalesceExtBasicBlocks () =
	 let
	    val preds = Array.new (n, 0)
	    fun incPred i =
	       Array.update (preds, i, 1 + (Array.sub (preds, i)))
	    val _ = 
	       Vector.foreach
	       (nodes, fn node => 
		List.foreach
		(Node.successors node, 
		 incPred o nodeIndex o Edge.to))
	    val _ =
	       Array.foreachi
	       (preds, fn (i, n) =>
		if n > 1 then Array.update (mayHaveCheck, i, true) else ())
	 in
	   ()
	 end

      fun insertCoalesceLoopHeaders loopExits =
	 let
	    (* Set equivalence classes, where two nodes are equivalent if they are
	     * in the same loop in the loop forest.
	     * Also mark loop headers as mayHaveCheck.
	     *)
	    val classes = Array.array (n, ~1)
	    fun indexClass i = Array.sub (classes, i)
	    val c = Counter.new 0
	    fun setClass (Forest.T {loops, notInLoop}) =
	       let
		  val class = Counter.next c
		  val _ =
		     Vector.foreach (notInLoop, fn n =>
				     if Node.equals (n, root)
				        then ()
				     else Array.update (classes, nodeIndex n, class))
		  val _ =
		     Vector.foreach
		     (loops, fn {headers, child} =>
		      (Vector.foreach (headers, fn n =>
				       Array.update (mayHaveCheck, nodeIndex n, true))
		       ; setClass child))
	       in
		  ()
	       end
	    val _ = setClass (Graph.loopForestSteensgaard (g, {root = root}))
	    val numClasses = Counter.value c
	    datatype z = datatype Control.limitCheck
	    val _ =
	       if loopExits
		  then let
			  (* Determine which classes allocate. *)
			  val classDoesAllocate = Array.array (numClasses, false)
			  val _ =
			     List.foreach
			     (Graph.nodes g, fn n =>
			      if Node.equals (n, root)
				 then ()
			      else
			      let
				 val i = nodeIndex n
			      in
				 if 0 < Vector.sub (objectBytesAllocated, i)
				    then Array.update (classDoesAllocate, 
						       indexClass i, 
						       true)
				 else ()
			      end)
			  (* Mark nodes that are post-exits of non-allocating 
			   * loops as mayHaveCheck.
			   *)
			  val _ =
			     List.foreach
			     (Graph.nodes g, fn n =>
			      if Node.equals (n, root)
				 then ()
			      else 
			      let
				 val i = nodeIndex n
				 val c = indexClass i
			      in
				if Array.sub (classDoesAllocate, c)
				   then ()
				else List.foreach 
				     (Node.successors n, fn e =>
				      let
					 val i' = nodeIndex (Edge.to e)
				      in
					 if c <> indexClass i'
					    then Array.update 
					         (mayHaveCheck, i', true)
					 else ()
				      end)
			      end)      
		       in
			 ()
		       end
	       else ()
	 in
	    ()
	 end

      datatype z = datatype Control.limitCheck
      val _ = 
	 case !Control.limitCheck of
	    ExtBasicBlocks => insertCoalesceExtBasicBlocks ()
	  | LoopHeaders {loopExits, ...} => insertCoalesceLoopHeaders loopExits
	  | _ => Error.bug "LimitCheck.insertCoalesce"

      (* If we remove edges into nodes that are mayHaveCheck, we have an
       * acyclic graph.
       * So, we can compute a function, maxPath, inductively that for each node
       * tells the maximum amount allocated along any path that passes only
       * through nodes that are not mayHaveCheck.
       *)
      local
	 val a = Array.array (n, NONE)
      in
	 fun maxPath arg =  (* i is a node index *)
	    traceMaxPath
	    (fn (i: int) =>
	    case Array.sub (a, i) of
	       SOME x => x
	     | NONE =>
		  let
		     val x = Vector.sub (objectBytesAllocated, i)
		     val max =
			List.fold
			(Node.successors (indexNode i), 0, fn (e, max) =>
			 let
			    val i' = nodeIndex (Edge.to e)
			 in
			    if Array.sub (mayHaveCheck, i')
			       then max
			    else Int.max (max, maxPath i')
			 end)
		     val x = x + max
		     val _ = Array.update (a, i, SOME x)
		  in
		     x
		  end
	       ) arg
      end
      fun blockInfo {blockIndex} =
	 let 
	    val block as Block.T {label, statements, ...} = 
	       Vector.sub (blocks, blockIndex)
	    val array =
	       let
		 fun default () = NONE
	       in
		  if 0 < Vector.length statements
		     then case Vector.sub (statements, 0) of
		             Statement.Array {dst, numBytesNonPointers,
					      numElts, numPointers} =>
			        let
				   val bytesPerElt =
				      if numPointers = 0
				         then numBytesNonPointers
				      else if numBytesNonPointers = 0
				 	      then numPointers * 
					           Runtime.pointerSize
				      else Error.unimplemented "tricky arrays"
 			        in
				   SOME {bytesPerElt = bytesPerElt,
					 numElts = numElts}
				end 
			   | _ => default ()
		 else default ()
	       end
	    val bytes =
	       Vector.fold
	       (statements, 0, fn (s, ac) =>
		ac + Statement.objectBytesAllocated s)
	    val heap = if Array.sub (mayHaveCheck, blockIndex)
	                  then SOME {bytes = maxPath blockIndex}
		       else NONE
	    val stack = Label.equals (start, label)
	 in 
	    BlockInfo.T
	    {array = array,
	     heap = heap,
	     stack = Label.equals (start, label)}
	 end
   in
      insertFunction (f, blockInfo)
   end

(*

fun insertCoalesce (f: Function.t) =
   let
      val {args, blocks, name, start} = Function.dest f
      val n = Vector.length blocks
      val {get = labelIndex, set = setLabelIndex, rem = remLabelIndex, ...} =
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("index", Label.layout))
      val {get = nodeIndex, set = setNodeIndex, rem, ...} =
	 Property.getSetOnce (Node.plist,
			      Property.initRaise ("index", Node.layout))
      val _ =
	 Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
			  setLabelIndex (label, i))
      (* Build the graph. *)
      val g = Graph.new ()
      val nodes = Vector.tabulate (n, fn i =>
				   let
				      val n = Graph.newNode g
				      val _ = setNodeIndex (n, i)
				   in
				      n
				   end)
      fun indexNode i = Vector.sub (nodes, i)
      val labelNode = indexNode o labelIndex
      val root = Graph.newNode g
      (* Mark nodes that we know may have a check:
       *   Cont, Handler, Runtime, or Jump that starts with an Array.
       *)
      val mayHaveCheck =
	 Array.tabulate
	 (n, fn i =>
	  let
	     val Block.T {kind, statements, ...} = Vector.sub (blocks, i)
	     datatype z = datatype Kind.t
	     val b =
		case kind of
		   Cont _ => true
		 | CReturn _ => false
		 | Handler => true
		 | Jump => (0 < Vector.length statements
			    andalso (case Vector.sub (statements, 0) of
					Statement.Array _ => true
				      | _ => false))
		 | Runtime _ => true
	     val _ =
		if b
		   then (Graph.addEdge (g, {from = root, to = indexNode i})
			 ; ())
		else ()
	  in
	     b
	  end)
      val _ = Array.update (mayHaveCheck, labelIndex start, true)
      val _ = Graph.addEdge (g, {from = root, to = labelNode start})
      (* Add edges into all nodes that do not have a check. *)
      val _ =
	 Vector.foreachi
	 (blocks, fn (i, Block.T {transfer, ...}) =>
	  let
	     val from = indexNode i
	  in
	     Transfer.foreachLabel
	     (transfer, fn l =>
	      let
		 val i' = labelIndex l
	      in
		 if Array.sub (mayHaveCheck, i')
		    then ()
		 else (Graph.addEdge (g, {from = from, to = indexNode i'})
		       ; ())
	      end)
	  end)
      val _ = Vector.foreach (blocks, remLabelIndex o Block.label)
      (* Set equivalence classes, where two nodes are equivalent if they are
       * in the same loop in the loop forest.
       * Also mark loop headers as mayHaveCheck.
       *)
      val classes = Array.array (n, ~1)
      fun indexClass i = Array.sub (classes, i)
      val c = Counter.new 0
      fun setClass (Forest.T {loops, notInLoop}) =
	 let
	    val class = Counter.next c
	    val _ =
	       Vector.foreach (notInLoop, fn n =>
			       if Node.equals (n, root)
				  then ()
			       else Array.update (classes, nodeIndex n, class))
	    val _ =
	       Vector.foreach
	       (loops, fn {headers, child} =>
		(Vector.foreach (headers, fn n =>
				 Array.update (mayHaveCheck, nodeIndex n, true))
		 ; setClass child))
	 in
	    ()
	 end
      val _ = setClass (Graph.loopForestSteensgaard (g, {root = root}))
      val numClasses = Counter.value c
      val objectBytesAllocated =
	 Vector.map
	 (blocks, fn Block.T {statements, ...} =>
	  Vector.fold (statements, 0, fn (s, ac) =>
		       ac + Statement.objectBytesAllocated s))
      (* Determine which classes allocate. *)
      val classDoesAllocate = Array.array (numClasses, false)
      val _ =
	 List.foreach
	 (Graph.nodes g, fn n =>
	  if Node.equals (n, root)
	     then ()
	  else
	     let
		val i = nodeIndex n
	     in
		if 0 < Vector.sub (objectBytesAllocated, i)
		   then Array.update (classDoesAllocate, indexClass i, true)
		else ()
	     end)
      (* Mark nodes that are post-exits of non-allocating loops as
       * mayHaveCheck.
       *)
      val _ =
	 List.foreach
	 (Graph.nodes g, fn n =>
	  if Node.equals (n, root)
	     then ()
	  else
	     let
		val i = nodeIndex n
		val c = indexClass i
	     in
		if Array.sub (classDoesAllocate, c)
		   then ()
		else
		   List.foreach (Node.successors n, fn e =>
				 let
				    val i' = nodeIndex (Edge.to e)
				 in
				    if c <> indexClass i'
				       then Array.update (mayHaveCheck, i', true)
				    else ()
				 end)
	     end)
      (* If we remove edges into nodes that are mayHaveCheck, we have an
       * acyclic graph (because all loop headers are mayHaveCheck).
       * So, we can compute a function, maxPath, inductively that for each node
       * tells the maximum amount allocated along any path that passes only
       * through nodes that are not mayHaveCheck.
       *)
      local
	 val a = Array.array (n, NONE)
      in
	 fun maxPath arg =  (* i is a node index *)
	    traceMaxPath
	    (fn (i: int) =>
	    case Array.sub (a, i) of
	       SOME x => x
	     | NONE =>
		  let
		     val x = Vector.sub (objectBytesAllocated, i)
		     val max =
			List.fold
			(Node.successors (indexNode i), 0, fn (e, max) =>
			 let
			    val i' = nodeIndex (Edge.to e)
			 in
			    if Array.sub (mayHaveCheck, i')
			       then max
			    else Int.max (max, maxPath i')
			 end)
		     val x = x + max
		     val _ = Array.update (a, i, SOME x)
		  in
		     x
		  end
	       ) arg
      end
      fun checkAmount {blockIndex = i} =
	 if Array.sub (mayHaveCheck, i)
	    then maxPath i
	 else 0
      fun nodeOptions n =
	 let
	    open Dot
	    open NodeOption
	 in
	    if Node.equals (n, root)
	       then [label "root"]
	    else
	       let
		  val i = nodeIndex n
		  val alloc = Vector.sub (objectBytesAllocated, i)
		  val checkAmount = checkAmount {blockIndex = i}
	       in
		  [Label [(Label.toString (Block.label (Vector.sub (blocks, i))),
			   Center),
			  (concat ["check amount = ", Int.toString checkAmount],
			   Center),
			  (concat ["maxPath = ", Int.toString (maxPath i)],
			   Center),
			  (concat ["alloc = ", Int.toString alloc], Center),
			  (concat ["class = ", Int.toString (indexClass i)],
			   Center)],
		   Shape Box,
		   Color (if checkAmount > 0
			     then DotColor.Red1
			  else if alloc > 0
				  then DotColor.Green1
			       else DotColor.Black)]
	       end
	 end
      val _ =
	 if true
	    then ()
	 else
	    File.withOut
	    (concat ["/tmp/", Func.toString (Function.name f), ".dot"],
	     fn out =>
	     Layout.outputl (Graph.layoutDot
			     (g, fn _ => {title = "graph",
					  options = [],
					  edgeOptions = fn _ => [],
					  nodeOptions = nodeOptions}),
			     out))
   in
      insertFunction (f, checkAmount)
   end

val insertCoalesce =
   Trace.trace ("insertCoalesce",
		Func.layout o Function.name,
		Layout.ignore)
   insertCoalesce
*)
fun insert (Program.T {functions, main}) =
   let
      datatype z = datatype Control.limitCheck
      val insertFunction =
	 case !Control.limitCheck of
	    PerBlock => insertPerBlock
	  | _ => insertCoalesce
   in
      Program.T {functions = List.revMap (functions, insertFunction),
		 main = insertFunction main}
   end


end
