functor SignalCheck (S: SIGNAL_CHECK_STRUCTS): SIGNAL_CHECK = 
struct

open S
open Rssa

structure Graph = DirectedGraph
structure Node = Graph.Node
structure Edge = Graph.Edge
structure Forest = Graph.LoopForest

fun insert p =
   if not (Program.usesSignals p)
      then p
   else
      let
	 val Program.T {functions, main} = p
	 fun insert (f: Function.t): Function.t =
	    let
	       val {args, blocks, name, start} = Function.dest f
	       val {get = labelIndex: Label.t -> int, set = setLabelIndex,
		    rem = remLabelIndex, ...} =
		  Property.getSetOnce
		  (Label.plist, Property.initRaise ("index", Label.layout))
	       val _ =
		  Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
				   setLabelIndex (label, i))
	       val g = Graph.new ()
	       val n = Vector.length blocks
	       val {get = nodeIndex: Node.t -> int, set = setNodeIndex, ...} =
		  Property.getSetOnce
		  (Node.plist, Property.initRaise ("index", Node.layout))
	       val nodes =
		  Vector.tabulate (n, fn i =>
				   let
				      val n = Graph.newNode g
				      val _ = setNodeIndex (n, i)
				   in
				      n
				   end)
	       val isHeader = Array.new (n, false)
	       fun indexNode i = Vector.sub (nodes, i)
	       val labelNode = indexNode o labelIndex
	       val _ =
		  Vector.foreachi
		  (blocks, fn (i, Block.T {label, transfer, ...}) =>
		   let
		      val from = indexNode i
		   in
		      if (case transfer of
			     Transfer.Runtime _ => true
			   | _ => false)
			 then ()
		      else
			 Transfer.foreachLabel
			 (transfer, fn to =>
			  (Graph.addEdge (g, {from = from,
					      to = labelNode to})
			   ; ()))
		   end)
	       val extra: Block.t list ref = ref []
	       (* Create extra blocks with signal checks for all blocks that are
		* loop headers.
		*)
	       fun loop (Forest.T {loops, ...}) =
		  Vector.foreach
		  (loops, fn {headers, child} =>
		   let
		      val _ =
			 Vector.foreach
			 (headers, fn n =>
			  let
			     val i = nodeIndex n
			     val _ = Array.update (isHeader, i, true)
			     val Block.T {args, kind, label, profileInfo,
					  statements, transfer} =
				Vector.sub (blocks, i)
			     val failure = Label.newNoname ()
			     val success = Label.newNoname ()
			     val collect = Label.newNoname ()
			     val collectReturn = Label.newNoname ()
			     val dontCollect = Label.newNoname ()
			     val res = Var.newNoname ()
			     val compare =
				Vector.new1
				(Statement.PrimApp
				 {args = Vector.new2 (Operand.Runtime
						      RuntimeOperand.Limit,
						      Operand.int 0),
				  dst = SOME (res, Type.bool),
				  prim = Prim.eq})
			     val compareTransfer =
				Transfer.Switch
				{cases = Cases.Int [(0, dontCollect),
						    (1, collect)],
				 default = NONE,
				 test = Operand.Var {var = res, ty = Type.bool}}
			     val _ =
				extra :=
 				Block.T {args = args,
 					 kind = kind,
					 label = label,
 					 profileInfo = profileInfo,
 					 statements = compare,
 					 transfer = compareTransfer}
				:: (Block.T
				    {args = Vector.new0 (),
				     kind = Kind.Jump,
				     label = collect,
				     profileInfo = profileInfo,
				     statements = Vector.new0 (),
				     transfer = (Transfer.Runtime
						 {args = (Vector.new2
							  (Operand.int 0,
							   Operand.int 0)),
						  prim = Prim.gcCollect,
						  return = collectReturn})})
				:: Block.T {args = Vector.new0 (),
					    kind = Kind.Runtime {prim = Prim.gcCollect},
					    label = collectReturn,
					    profileInfo = profileInfo,
					    statements = Vector.new0 (),
					    transfer =
					    Transfer.Goto {dst = dontCollect,
							   args = Vector.new0 ()}}
				:: Block.T {args = Vector.new0 (),
					    kind = Kind.Jump,
					    label = dontCollect,
					    profileInfo = profileInfo,
					    statements = statements,
					    transfer = transfer}
				:: !extra
			  in
			     ()
			  end)
		      val _ = loop child
		   in
		      ()
		   end)
	       val forest =
		  loop
		  (Graph.loopForestSteensgaard (g, {root = labelNode start}))
	       val blocks =
		  Vector.keepAllMap
		  (blocks, fn b as Block.T {label, ...} =>
		   if Array.sub (isHeader, labelIndex label)
		      then NONE
		   else SOME b)
	       val blocks = Vector.concat [blocks, Vector.fromList (!extra)]
	       val f = Function.new {args = args,
				     blocks = blocks,
				     name = name,
				     start = start}
	       val _ = Function.clear f
	    in
	       f
	    end
      in
	 Program.T {functions = List.revMap (functions, insert),
		    main = main}
      end

end
