(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(*
 * The goal of limit check insertion is to ensure that
 * 	1. At any allocation of b bytes, frontier + b <= base + heapSize
 * 	2. At entry to each function, stackTop <= stackLimit
 * 
 * It assumes that runtime provides several operands to help with this.
 * 	Frontier
 * 	Limit
 * 	LimitPlusSlop
 * 	StackLimit
 * 	StackTop
 * 
 * There are three different kinds of checks inserted, depending on the
 * amount being allocated and whether or not the program uses signal
 * handlers.
 * 
 * 1. If b <= LIMIT_SLOP, then continue (don't GC) if
 * 
 * 	frontier <= limit
 * 
 *    The reason this works is that if frontier <= limit and b <=
 *    LIMIT_SLOP, then 
 * 	frontier + b <= limit + LIMIT_SLOP 
 *                       = limitPlusSlop
 *                       = base + heapSize
 *    This works even if the program uses signal handlers, which set
 *    limit to zero, since frontier <= 0 will always be false.
 * 
 * 2. If b > LIMIT_SLOP and if the program doesn't use signal handlers,
 *    then continue (don't GC) if
 * 
 * 	b <= limitPlusSlop - frontier
 * 
 *    The reason this works is that the condition is equivalent to
 * 	
 * 	b + frontier <= limitPlusSlop = base + heapSize
 * 
 *    We write the condition the way we do instead of the more obvious way
 *    because "b + frontier" may overflow, while limitPlusSlop - frontier
 *    can not, unless the program uses signal handlers.
 * 
 * 3. If b > LIMIT_SLOP and if the program uses signal handlers, then
 *    continue (don't GC) if
 * 
 * 	limit > 0
 * 	and b <= limitPlusSlop - frontier
 * 
 *    This is like case (2), except that because the program uses signal
 *    handlers, the runtime may have set limit to zero to indicate that a
 *    signal needs to be handled.  So, we first check that this is not
 *    the case before continuing as in case (2).
 *
 * Stack limit checks are completely orthogonal to heap checks, and are simply
 * inserted at the start of each function.
 *)
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

      fun error () = Error.bug "Primitive with non-constant bytesNeeded"
      fun objectBytesAllocated s =
	 case s of
	    Statement.Object {numPointers = p, numWordsNonPointers = np, ...} =>
	       Runtime.objectHeaderSize
	       + Runtime.objectSize {numPointers = p, numWordsNonPointers = np}
	  | Statement.PrimApp {prim, args, ...} =>
	       (case Prim.bytesNeeded prim of
		   SOME f => (case f args of
				 Operand.Const c =>
				    (case Const.node c of
				        Const.Node.Word w => Word.toInt w
				      | _ => error ())
			       | _ => 0)
		 | NONE => 0)
	  | _ => 0
   end

structure BlockInfo =
   struct
      datatype t = T of {heap: {bytes: int} option,
			 stack: bool}
      
      fun layout (T {heap, stack}) =
	 Layout.record
	 [("heap", Option.layout
	           (fn {bytes, ...} =>
		    Layout.record
		    [("bytes", Int.layout bytes)])
		   heap),
	  ("stack", Bool.layout stack)]
	 
   end

val extraGlobals: Var.t list ref = ref []
   
fun insertFunction (f: Function.t,
		    handlesSignals: bool,
		    blockInfo: {blockIndex: int} -> BlockInfo.t) =
   let
      val {args, blocks, name, start} = Function.dest f
      val newBlocks = ref []
      val (_, allocTooLarge) = Block.allocTooLarge newBlocks
      val _ =
	 Vector.foreachi
	 (blocks, fn (i, block as Block.T {args, kind, label, profileInfo,
					   statements, transfer}) =>
	  let
	     val BlockInfo.T {heap, stack} = blockInfo {blockIndex = i}
	     val _ = Assert.assert
	             ("LimitCheck.insertFunction: stack", fn () =>
		      if Label.equals (start, label)
			 then stack
		      else not stack)
	     fun insert (amount: Operand.t) =
		let
		   val collect = Label.newNoname ()
		   val collectReturn = Label.newNoname ()
		   val dontCollect = Label.newNoname ()
		   val (dontCollect', collectReturnStatements, force) =
		      case !Control.gcCheck of
			 Control.First =>
			    let
			       val global = Var.newNoname ()
			       val _ = List.push (extraGlobals, global)
			       val global =
				  Operand.Var {var = global,
					       ty = Type.bool}
			       val dontCollect' = Label.newNoname ()
			       val _ =
				  List.push
				  (newBlocks,
				   Block.T
				   {args = Vector.new0 (),
				    kind = Kind.Jump,
				    label = dontCollect',
				    profileInfo = profileInfo,
				    statements = Vector.new0 (),
				    transfer =
				    Transfer.iff (global, {falsee = dontCollect,
							   truee = collect})})
			    in
			       (dontCollect',
				Vector.new1
				(Statement.Move {dst = global,
						 src = Operand.bool false}),
				global)
			    end
		       | Control.Limit =>
			    (dontCollect, Vector.new0 (), Operand.bool false)
		       | Control.Every =>
			    (collect, Vector.new0 (), Operand.bool true)
		   val _ = 
		      newBlocks :=
		      Block.T {args = Vector.new0 (),
			       kind = Kind.Jump,
			       label = collect,
			       profileInfo = profileInfo,
			       statements = Vector.new0 (),
			       transfer = (Transfer.Runtime
					   {args = Vector.new2 (amount, force),
					    prim = Prim.gcCollect,
					    return = collectReturn})}
		      :: Block.T {args = Vector.new0 (),
				  kind = Kind.Runtime {prim = Prim.gcCollect},
				  label = collectReturn,
				  profileInfo = profileInfo,
				  statements = collectReturnStatements,
				  transfer =
				  Transfer.Goto {dst = dontCollect,
						 args = Vector.new0 ()}}
		      :: Block.T {args = Vector.new0 (),
				  kind = Kind.Jump,
				  label = dontCollect,
				  profileInfo = profileInfo,
				  statements = statements,
				  transfer = transfer}
		      :: !newBlocks
		in
		   {collect = collect,
		    dontCollect = dontCollect'}
		end
	     fun newBlock (isFirst, statements, transfer) =
		let
		   val (args, kind, label) =
		      if isFirst
			 then (args, kind, label)
		      else (Vector.new0 (), Kind.Jump, Label.newNoname ())
		   val _ =
		      List.push
		      (newBlocks,
		       Block.T {args = args,
				kind = kind,
				label = label,
				profileInfo = profileInfo,
				statements = statements,
				transfer = transfer})
		in
		   label
		end
	     fun primApp (prim, op1, op2, {collect, dontCollect}) =
		let
		   val res = Var.newNoname ()
		   val s =
		      Statement.PrimApp {args = Vector.new2 (op1, op2),
					 dst = SOME (res, Type.bool),
					 prim = prim}
		   val transfer =
		      Transfer.iff
		      (Operand.Var {var = res, ty = Type.bool},
		       {falsee = dontCollect,
			truee = collect})
		in
		   (Vector.new1 s, transfer)
		end
	     datatype z = datatype RuntimeOperand.t
	     fun stackCheck (maybeFirst, z): Label.t =
		let
		   val (statements, transfer) =
		      primApp (Prim.word32Gt,
			       Operand.Runtime StackTop,
			       Operand.Runtime StackLimit,
			       z)
		in
		   newBlock (maybeFirst, statements, transfer)
		end
	     fun maybeStack (): Label.t =
		if stack
		   then stackCheck (true, insert (Operand.int 0))
		else
		   (* No limit check, just keep the block around. *)
		   (List.push (newBlocks, block)
		    ; label)
	     fun frontierCheck (isFirst,
				prim, op1, op2,
				z as {collect, dontCollect}): Label.t =
		let
		   val (statements, transfer) = primApp (prim, op1, op2, z)
		   val l = newBlock (isFirst andalso not stack,
				     statements, transfer)
		in
		   if stack
		      then stackCheck (isFirst, {collect = collect,
						 dontCollect = l})
		   else l
		end
	     fun heapCheck (isFirst: bool, amount: Operand.t): Label.t =
		let
		   val z as {collect, dontCollect} = insert amount
		   val res = Var.newNoname ()
		   val s =
		      (* Can't do Limit - Frontier, because don't know that
		       * Frontier < Limit.
		       *)
		      Statement.PrimApp
		      {args = Vector.new2 (Operand.Runtime LimitPlusSlop,
					   Operand.Runtime Frontier),
		       dst = SOME (res, Type.word),
		       prim = Prim.word32Sub}
		   val (statements, transfer) =
		      primApp (Prim.word32Gt,
			       amount,
			       Operand.Var {var = res, ty = Type.word},
			       z)
		   val statements = Vector.concat [Vector.new1 s, statements]
		in
		   if handlesSignals
		      then
			 frontierCheck (isFirst,
					Prim.eq,
					Operand.Runtime Limit,
					Operand.int 0,
					{collect = collect,
					 dontCollect = newBlock (false,
								 statements,
								 transfer)})
		   else if stack
			   then
			      stackCheck
			      (isFirst,
			       {collect = collect,
				dontCollect =
				newBlock (false, statements, transfer)})
			else newBlock (isFirst, statements, transfer)
		end
	     fun heapCheckNonZero (bytes: Word.t): Label.t =
		if bytes <= Word.fromInt Runtime.limitSlop
		   then frontierCheck (true,
				       Prim.word32Gt,
				       Operand.Runtime Frontier,
				       Operand.Runtime Limit,
				       insert (Operand.int 0))
		else heapCheck (true, Operand.word bytes)
	     fun noPrimitiveAllocation () =
		case heap of
		   NONE => maybeStack ()
		 | SOME {bytes} =>
		      if bytes = 0
			 then maybeStack ()
		      else heapCheckNonZero (Word.fromInt bytes)
	     fun primitiveAllocation (bytesNeeded: Operand.t) =
		let
		   val extraBytes =
		      Word.fromInt
		      (Runtime.arrayHeaderSize
		       + (case heap of
			     NONE => 0
			   | SOME {bytes} => bytes))
		in
		   case bytesNeeded of
		      Operand.Const c =>
			 (case Const.node c of
			     Const.Node.Word w =>
				heapCheckNonZero
				(MLton.Word.addCheck (w, extraBytes)
				 handle Overflow => Runtime.allocTooLarge)
			   | _ => Error.bug "strange primitive bytes needed")
		    | _ =>
			 let
			    val bytes = Var.newNoname ()
			 in
			    newBlock
			    (true,
			     Vector.new0 (),
			     Transfer.Arith
			     {args = Vector.new2 (Operand.word extraBytes,
						  bytesNeeded),
			      dst = bytes,
			      overflow = allocTooLarge (),
			      prim = Prim.word32AddCheck,
			      success = (heapCheck
					 (false, 
					  Operand.Var {var = bytes,
						       ty = Type.word})),
			      ty = Type.word})
			 end
		end
	     val _ =
		if 0 < Vector.length statements
		   then case Vector.sub (statements, 0) of
		      Statement.PrimApp {prim, args, ...} =>
			 (case Prim.bytesNeeded prim of
			     SOME f => primitiveAllocation (f args)
			   | _ => noPrimitiveAllocation ())
		    | _ => noPrimitiveAllocation ()
		else noPrimitiveAllocation ()
	  in
	     ()
	  end)
   in
      Function.new {args = args,
		    blocks = Vector.fromList (!newBlocks),
		    name = name,
		    start = start}
   end

fun insertPerBlock (f: Function.t, handlesSignals) =
   let
      val {start, blocks, ...} = Function.dest f
      fun blockInfo {blockIndex} =
	 let 
	    val block as Block.T {label, statements, ...} = 
	       Vector.sub (blocks, blockIndex)
	    val bytes =
	       Vector.fold
	       (statements, 0, fn (s, ac) =>
		ac + Statement.objectBytesAllocated s)
	    val heap = SOME {bytes = bytes}
	    val stack = Label.equals (start, label)
	 in 
	    BlockInfo.T
	    {heap = heap,
	     stack = Label.equals (start, label)}
	 end
   in
      insertFunction (f, handlesSignals, blockInfo)
   end

structure Graph = DirectedGraph
structure Node = Graph.Node
structure Edge = Graph.Edge
structure Forest = Graph.LoopForest

val traceMaxPath = Trace.trace ("maxPath", Int.layout, Int.layout)

fun insertCoalesce (f: Function.t, handlesSignals) =
   let
      val {args, blocks, name, start} = Function.dest f
      val n = Vector.length blocks
      val {get = labelIndex, set = setLabelIndex, rem = remLabelIndex, ...} =
	 Property.getSetOnce
	 (Label.plist,
	  Property.initRaise ("LimitCheck.labelIndex", Label.layout))
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
       *     = start, Cont, Handler, Runtime, or
       *         Jump that starts with a primitive with non-constant bytesNeeded
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
				        Statement.PrimApp {prim, args, ...} =>
					   (case Prim.bytesNeeded prim of
					       SOME f => (case f args of
							     Operand.Const c => false
							   | _ => true)
					     | _ => false)
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
	    val heap = if Array.sub (mayHaveCheck, blockIndex)
	                  then SOME {bytes = maxPath blockIndex}
		       else NONE
	    val stack = Label.equals (start, label)
	 in 
	    BlockInfo.T
	    {heap = heap,
	     stack = Label.equals (start, label)}
	 end
      val f = insertFunction (f, handlesSignals, blockInfo)
      val _ = Function.clear f
   in
      f
   end

fun insert (p as Program.T {functions, main}) =
   let
      datatype z = datatype Control.limitCheck
      val insertFunction =
	 case !Control.limitCheck of
	    PerBlock => insertPerBlock
	  | _ => insertCoalesce
      val handlesSignals = Program.handlesSignals p
      val insertFunction = fn f => insertFunction (f, handlesSignals)
      val functions = List.revMap (functions, insertFunction)
      val {args, blocks, name, start} = Function.dest (insertFunction main)
      val newStart = Label.newNoname ()
      val block =
	 Block.T {args = Vector.new0 (),
		  kind = Kind.Jump,
		  label = newStart,
		  profileInfo = {ssa = {func = "", label = ""}},
		  statements = (Vector.fromListMap
				(!extraGlobals, fn x =>
				 Statement.Bind {isMutable = true,
						 oper = Operand.bool true,
						 var = x})),
		  transfer = Transfer.Goto {args = Vector.new0 (),
					    dst = start}}
      val blocks = Vector.concat [Vector.new1 block, blocks]
      val main = Function.new {args = args,
			       blocks = blocks,
			       name = name,
			       start = newStart}
   in
      Program.T {functions = functions,
		 main = main}
   end


end
