(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Inline (S: INLINE_STRUCTS): INLINE = 
struct

open S
open Exp Transfer

type int = Int.t
type word = Word.t

structure Size =
   struct
      val check : (int * int option) -> bool =
	 fn (size, NONE) => false
	  | (size, SOME size') => size > size'

      val defaultExpSize : Exp.t -> int = 
	 fn ConApp {args, ...} => 1 + Vector.length args
	  | Const _ => 0
	  | PrimApp {args, ...} => 1 + Vector.length args
	  | Select _ => 1 + 1
	  | Tuple xs => 1 + Vector.length xs
	  | Var _ => 0
	  (* Handler* / Set* *)
	  | _ => 0
      fun expSize (size, max) (doExp, doTransfer) exp =
	 let
	    val size' = doExp exp
	    val size = size + size'
	 in
	    (size, check (size, max))
	 end
      fun statementSize (size, max) (doExp, doTransfer) =
	 fn Statement.T {exp, ...} => expSize (size, max) (doExp, doTransfer) exp
      fun statementsSize (size, max) (doExp, doTransfer) statements =
	 DynamicWind.withEscape
	 (fn escape =>
	  Vector.fold
	  (statements, (size, false), fn (statement, (size, check)) =>
	   if check
	      then escape (size, check)
	   else statementSize (size, max) (doExp, doTransfer) statement))
      val defaultTransferSize =
	 fn Bug => 1
	  | Call {args, ...} => 1 + Vector.length args
	  | Case {cases, ...} => 1 + Cases.length cases
	  | Goto {args, ...} => 1 + Vector.length args
	  | Prim {args, ...} => 1 + Vector.length args
	  | Raise x => 1 + 1
	  | Return xs => 1 + Vector.length xs
      fun transferSize (size, max) (doExp, doTransfer) transfer =
	 let
	    val size' = doTransfer transfer
	    val size = size + size'
	 in
	    (size, check (size, max))
	 end
      fun blockSize (size, max) (doExp, doTransfer) =
	 fn Block.T {statements, transfer, ...} =>
	 case statementsSize (size, max) (doExp, doTransfer) statements of
	    (size, true) => (size, true)
	  | (size, false) => transferSize (size, max) (doExp, doTransfer) transfer
      fun blocksSize (size, max) (doExp, doTransfer) blocks =
	 DynamicWind.withEscape
	 (fn escape =>
	  Vector.fold
	  (blocks, (size, false), fn (block, (size, check)) =>
	   if check
	      then escape (size, check)
	   else blockSize (size, max) (doExp, doTransfer) block))
      fun functionSize (size, max) (doExp, doTransfer) f =
	 blocksSize (size, max) (doExp, doTransfer) (#blocks (Function.dest f))

      val default = (defaultExpSize, defaultTransferSize)
      val exp = #1 o (expSize (0, NONE) default)
      val statement = #1 o (statementSize (0, NONE) default)
      val statements = #1 o (statementsSize (0, NONE) default)
      val transfer = #1 o (transferSize (0, NONE) default)
      val block = #1 o (blockSize (0, NONE) default)
      val blocks = #1 o (blocksSize (0, NONE) default)
      val function = #1 o (functionSize (0, NONE) default)

      fun expGT max = #2 o (expSize (0, max) default)
      fun statementGT max = #2 o (statementSize (0, max) default)
      fun statementsGT max = #2 o (statementsSize (0, max) default)
      fun transferGT max = #2 o (transferSize (0, max) default)
      fun blockGT max = #2 o (blockSize (0, max) default)
      fun blocksGT max = #2 o (blocksSize (0, max) default)
      fun functionGT max = #2 o (functionSize (0, max) default)
   end

local
   fun 'a make (dontInlineFunc: Function.t * 'a -> bool)
      (Program.T {functions, ...}, a: 'a): Func.t -> bool =
      let
	 val {get = shouldInline: Func.t -> bool, 
	      set = setShouldInline, ...} =
	    Property.getSetOnce (Func.plist, Property.initConst false)
      in
	 List.foreach
	 (functions, fn f =>
	  if dontInlineFunc (f, a)
	     then ()
	  else setShouldInline (Function.name f, true))
	 ; Control.diagnostics
	   (fn display =>
	    let open Layout
	    in List.foreach
	       (functions, fn f => 
		let 
		   val name = Function.name f
		   val shouldInline = shouldInline name
		in 
		   display
		   (seq [Func.layout name, str ": ",
			 record [("shouldInline", Bool.layout shouldInline)]])
		end)
	    end)
	 ; shouldInline
      end
   fun containsCall (f: Function.t): bool =
      DynamicWind.withEscape
      (fn escape =>
       (Vector.foreach
	(Function.blocks f, fn Block.T {transfer, ...} =>
	 case transfer of
	    Call _ => escape true
	  | _ => ())
	; false))
   fun containsLoop (f: Function.t): bool =
      let
	 val {get, set, destroy} =
	    Property.destGetSet (Label.plist, Property.initConst false)
      in
	 DynamicWind.withEscape
	 (fn escape =>
	  let
	     fun loop (Tree.T (Block.T {label, transfer, ...}, children)) =
	        (set (label, true)
		 ; case transfer of
		      Goto {dst, ...} => if get dst then escape true else ()
		    | _ => ()
		 ; Vector.foreach (children, loop)
		 ; set (label, false))
	  in
	     loop (Function.dfsTree f)
	     ; false
	  end)
	 before (destroy ())
      end
in
   val leaf = make (fn (f, {size}) =>
		    Size.functionGT size f
		    orelse containsCall f)
   val leafNoLoop = make (fn (f, {size}) =>
			  Size.functionGT size f
			  orelse containsCall f
			  orelse containsLoop f)
end

structure Graph = DirectedGraph
structure Node = Graph.Node

fun nonRecursive (program as Program.T {functions, ...}, 
		  {size: int option}) =
   let
      val {get = funcInfo: Func.t -> {isBig: bool,
				      node: Node.t,
				      numCalls: int ref,
				      shouldInline: bool ref},
	   set = setFuncInfo, ...} =
	 Property.getSetOnce
	 (Func.plist, Property.initRaise ("funcInfo", Func.layout))
      val {get = nodeFunc: Node.t -> Func.t,
	   set = setNodeFunc, ...} = 
	 Property.getSetOnce 
	 (Node.plist, Property.initRaise ("nodeFunc", Node.layout))
      val graph = Graph.new ()
      (* initialize the info for each func *)
      val _ = 
	 List.foreach
	 (functions, fn f =>
	  let 
	     val name = Function.name f
	     val n = Graph.newNode graph
	  in setNodeFunc (n, name)
	     ; setFuncInfo (name, {node = n,
				   isBig = Size.functionGT size f,
				   numCalls = ref 0,
				   shouldInline = ref true})
	  end)
      (* Update call counts. *)
      val _ =
	 List.foreach
	 (functions, fn f => 
	  let
	     val {name, blocks, ...} = Function.dest f
	  in
	     Vector.foreach
	     (blocks, fn Block.T {transfer, ...} =>
	      case transfer of
		 Call {func, ...} =>
		    let
		       val {numCalls, shouldInline, isBig, ...} = funcInfo func
		       val numCalls' = !numCalls
		    in
		       if (numCalls' = 1 andalso isBig)
			  orelse Func.equals (name, func)
			  then shouldInline := false
		       else ()
		       ; numCalls := numCalls' + 1
		    end
	       | _ => ())
	  end)
      (* Build the call graph of potentially inlinable functions.
       * Don't add edges for functions that are not inlined, since they
       * can't cause infinite unrolling.
       *)
      val _ = 
	 List.foreach
	 (functions, fn f =>
	  let 
	     val {name, blocks, ...} = Function.dest f
	     val {node = caller, shouldInline = siCaller, ...} = funcInfo name
	  in 
	     Vector.foreach
	     (blocks, fn Block.T {transfer, ...} =>
	      case transfer of
		 Call {func, ...} =>
		    let
		       val {node = callee, shouldInline = siCallee, numCalls, ...} = 
			  funcInfo func
		    in
		       if !siCaller andalso !siCallee
			  then (Graph.addEdge (graph, {from = caller, to = callee})
				; ())
		       else ()
		    end
	       | _ => ())
	  end)
      (* Compute strongly-connected components and set any function that is
       * in a nontrivial scc to be not inlined.
       *)
      val _ = 
	 List.foreach
	 (Graph.stronglyConnectedComponents graph,
	  fn [] => ()
	   | [_] => ()
	   | ns => List.foreach (ns, fn n =>
				 #shouldInline (funcInfo (nodeFunc n)) := false))
      val _ =
	 Control.diagnostics
	 (fn display =>
	  let open Layout
	  in List.foreach
	     (functions, fn f => 
	      let 
	         val name = Function.name f
		 val {numCalls, shouldInline, ...} = funcInfo name
		 val numCalls = !numCalls
		 val shouldInline = !shouldInline
		 val size = Size.function f
	      in 
		 display
		 (seq [Func.layout name, str ": ",
		       record [("size", Int.layout size),
			       ("numCalls", Int.layout numCalls),
			       ("size * numCalls", Int.layout (size * numCalls)),
			       ("shouldInline", Bool.layout shouldInline)]])
	      end)
	  end)
   in
      ! o #shouldInline o funcInfo
   end

fun product (program as Program.T {functions, ...},
	     {small: int, product: int}) =
   let
      type info = {doesCallSelf: bool ref,
		   function: Function.t,
		   node: Node.t,
		   numCalls: int ref,
		   shouldInline: bool ref,
		   size: int ref}
      val {get = funcInfo: Func.t -> info,
	   set = setFuncInfo, ...} =
	 Property.getSetOnce
	 (Func.plist, Property.initRaise ("funcInfo", Func.layout))
      val {get = nodeFunc: Node.t -> Func.t,
	   set = setNodeFunc, ...} = 
	 Property.getSetOnce 
	 (Node.plist, Property.initRaise ("nodeFunc", Node.layout))
      val graph = Graph.new ()
      (* initialize the info for each func *)
      val _ = 
	 List.foreach
	 (functions, fn f =>
	  let 
	     val name = Function.name f
	     val n = Graph.newNode graph
	  in setNodeFunc (n, name)
	     ; setFuncInfo (name, {node = n,
				   doesCallSelf = ref false,
				   function = f,
				   numCalls = ref 0,
				   shouldInline = ref false,
				   size = ref 0})
	  end)
      (* Update call counts. *)
      val _ = 
	 List.foreach
	 (functions, fn f =>
	  let 
	     val {name, blocks, ...} = Function.dest f
	     val {doesCallSelf, ...} = funcInfo name
	  in
	     Vector.foreach
	     (blocks, fn Block.T {transfer, ...} =>
	      case transfer of
		 Call {func, ...} =>
		    let
		       val {numCalls, ...} = funcInfo func
		    in
		       if Func.equals (name, func)
			  then doesCallSelf := true
		       else Int.inc numCalls
		    end
	       | _ => ())
	  end)
      fun mayInline (setSize: bool,
		     {function, doesCallSelf, numCalls, size, ...}: info): bool =
	 not (!doesCallSelf)
	 andalso let
		    val (n, _) = 
		       Size.functionSize
		       (0, NONE)
		       (Size.defaultExpSize,
			fn t as Call {func, ...} =>
			      let
				val {shouldInline, size, ...} = funcInfo func
			      in
				if !shouldInline
				   then !size
				else Size.defaultTransferSize t
			      end
			 | t => Size.defaultTransferSize t)
		       function
		 in
		    if setSize
		       then size := n
		    else ()
		    ; (!numCalls - 1) * (n - small) <= product
		 end
      (* Build the call graph.  Do not include functions that we already know
       * will not be inlined.
       *)
      val _ =
	 List.foreach
	 (functions, fn f => 
	  let 
	     val {name, blocks, ...} = Function.dest f
	     val info as {node, ...} = funcInfo name
	  in
	     if mayInline (false, info)
	        then Vector.foreach
		     (blocks, fn Block.T {transfer, ...} =>
		      case transfer of
			 Call {func, ...} =>
			    if Func.equals (name, func)
			       then ()
			    else (Graph.addEdge 
				  (graph, {from = node, to = #node (funcInfo func)})
				  ; ())
		       | _ => ())
	     else ()
	  end)
      (* Compute strongly-connected components.
       * Then start at the leaves of the call graph and work up.
       *)
      val _ = 
	 List.foreach
	 (rev (Graph.stronglyConnectedComponents graph),
	  fn [n] => let val info as {shouldInline, ...} = funcInfo (nodeFunc n)
		    in shouldInline := mayInline (true, info)
		    end
	   | _ => ())
      val _ =
	 Control.diagnostics
	 (fn display =>
	  let open Layout
	  in List.foreach
	     (functions, fn f => 
	      let 
	         val name = Function.name f
		 val {numCalls, shouldInline, size, ...} = funcInfo name
		 val numCalls = !numCalls
		 val shouldInline = !shouldInline
		 val size = !size
	      in 
		 display
		 (seq [Func.layout name, str ": ",
		       record [("numCalls", Int.layout numCalls),
			       ("size", Int.layout size),
			       ("shouldInline", Bool.layout shouldInline)]])
	      end)
	  end)
   in
      ! o #shouldInline o funcInfo
   end

fun inline (program as Program.T {datatypes, globals, functions, main}) =
   let
      val shouldInline: Func.t -> bool =
	 let open Control
	 in case !inline of
	    NonRecursive r => product (program, r)
	  | Leaf r => leaf (program, r)
	  | LeafNoLoop r => leafNoLoop (program, r)
	 end
      val {get = funcInfo: Func.t -> Function.t,
	   set = setFuncInfo, ...} =
	 Property.getSetOnce
	 (Func.plist, Property.initRaise ("Inline.funcInfo", Func.layout))
      val _ = List.foreach (functions, fn f => setFuncInfo (Function.name f, f))

      fun doit (blocks: Block.t vector,
		return: Return.t) : Block.t vector =
	 let
	    val newBlocks = ref []
	    val blocks =
	       Vector.map
	       (blocks, fn block as Block.T {label, args, statements, transfer} =>
		let
		   fun new transfer =
		      Block.T {label = label,
			       args = args,
			       statements = statements,
			       transfer = transfer}
		in
		  case transfer of
		     Call {func, args, return = return'} =>
		        let
			   val return = Return.compose (return, return')
			in
			   if shouldInline func
			      then 
			      let
				 local
				    val {name, args, start, blocks, ...} =
				       (Function.dest o Function.alphaRename) 
				       (funcInfo func)
				    val blocks = doit (blocks, return)
				    val _ = List.push (newBlocks, blocks)
				    val name = Label.newString (Func.originalName name)
				    val _ = 
				       List.push 
				       (newBlocks,
					Vector.new1
					(Block.T
					 {label = name,
					  args = args,
					  statements = Vector.new0 (),
					  transfer = Goto {dst = start,
							   args = Vector.new0 ()}}))
				 in
				    val name = name
				 end
			      in
				 new (Goto {dst = name, 
					    args = args})
			      end
			   else new (Call {func = func,
					   args = args,
					   return = return})
			end
	           | Raise xs =>
			(case return of
			    Return.NonTail
			    {handler = Handler.Handle handler, ...} =>
			       new (Goto {dst = handler,
					  args = xs})
			  | _ => block)
		   | Return xs =>
			(case return of
			    Return.NonTail {cont, ...} =>
			       new (Goto {dst = cont, args = xs})
			  | _ => block)
		   | _ => block
		end)
	 in
	    Vector.concat (blocks::(!newBlocks))
	 end

      val shrink = shrinkFunction globals
      val functions =
	 List.fold
	 (functions, [], fn (f, ac) =>
	  let
	     val {name, args, start, blocks, returns, mayRaise} = Function.dest f
	  in
	     if Func.equals (name, main)
	        orelse
		not (shouldInline name)
		then let
		        val blocks = doit (blocks, Return.Tail)
		     in
			shrink (Function.new {name = name,
					      args = args,
					      start = start,
					      blocks = blocks,
					      returns = returns,
					      mayRaise = mayRaise})
			:: ac
		     end
	      else ac
	  end)
      val program =
	 Program.T {datatypes = datatypes,
		    globals = globals,
		    functions = functions,
		    main = main}
      val _ = Program.clear program
   in
      program
   end

end
