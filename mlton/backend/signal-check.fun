functor SignalCheck (S: SIGNAL_CHECK_STRUCTS): SIGNAL_CHECK = 
struct

open S
open Rssa

structure Graph = DirectedGraph
structure Node = Graph.Node
structure Edge = Graph.Edge
structure Forest = Graph.LoopForest

fun insert p =
   if not (Program.hasPrim (p, fn p =>
			    Prim.name p = Prim.Name.Thread_finishHandler))
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
			     Transfer.LimitCheck {kind, ...} =>
				(case kind of
				    LimitCheck.Array _ => true
				  | LimitCheck.Heap _ => true
				  | _ => false)
			   | Transfer.Runtime _ => true
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
			     val Block.T {args, kind, label, statements,
					  transfer} =
				Vector.sub (blocks, i)
			     val failure = Label.newNoname ()
			     val success = Label.newNoname ()
			     val _ =
				extra :=
				Block.T {args = args,
					 kind = kind,
					 label = label,
					 statements = Vector.new0 (),
					 transfer = (Transfer.LimitCheck
						     {failure = failure,
						      kind = LimitCheck.Signal,
						      success = success})}
				:: Block.T {args = Vector.new0 (),
					    kind = Kind.Runtime {prim = Prim.gcCollect},
					    label = failure,
					    statements = Vector.new0 (),
					    transfer = (Transfer.Goto
							{args = Vector.new0 (),
							 dst = success})}
				:: Block.T {args = Vector.new0 (),
					    kind = Kind.Jump,
					    label = success,
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
	    in
	       Function.new {args = args,
			     blocks = blocks,
			     name = name,
			     start = start}
	    end
      in
	 Program.T {functions = List.revMap (functions, insert),
		    main = main}
      end

end
