(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor NewOnce (S: NEW_ONCE_STRUCTS): NEW_ONCE = 
struct

open S
open Exp Transfer

structure Graph = DirectedGraph
local open Graph
in
   structure Edge = Edge
   structure Node = Node
end

structure Multi =
  struct
    structure L = TwoPointLattice (val bottom = "once"
				   val top = "multi")

    open L
    val forceMulti = makeTop
    val isMulti = isTop
    val whenMulti = addHandler
  end

structure MultiThreaded =
  struct
    structure L = TwoPointLattice (val bottom = "single threaded"
				   val top = "multi threaded")

    open L
    val forceMultiThreaded = makeTop
    val isMultiThreaded = isTop
    val whenMultiThreaded = addHandler
  end

structure FuncInfo =
  struct
    datatype t = T of {calls: int ref,
		       multi: Multi.t,
		       multiThreaded: MultiThreaded.t,
		       node: Node.t}

    fun layout (T {calls, multi, multiThreaded, ...}) 
      = Layout.record [("calls", Int.layout (!calls)),
		       ("multi", Multi.layout multi),
		       ("multiThreaded", MultiThreaded.layout multiThreaded)]

    local
       fun make f (T r) = f r
       fun make' f = (make f, ! o (make f))
    in
      val (calls,calls') = make' #calls
      val multi = make #multi
      val multiThreaded = make #multiThreaded
      val node = make #node
    end
   
    fun new node: t = T {calls = ref 0,
			 multi = Multi.new (),
			 multiThreaded = MultiThreaded.new (),
			 node = node}

    val forceMulti = Multi.forceMulti o multi
    val isMulti = Multi.isMulti o multi
    fun whenMulti (vi, th) = Multi.whenMulti (multi vi, th)
    val forceMultiThreaded = MultiThreaded.forceMultiThreaded o multiThreaded
    val isMultiThreaded = MultiThreaded.isMultiThreaded o multiThreaded
    fun whenMultiThreaded (vi, th) = MultiThreaded.whenMultiThreaded (multiThreaded vi, th)
  end

structure LabelInfo =
  struct
    datatype t = T of {multi: Multi.t}

    fun layout (T {multi, ...}) 
      = Layout.record [("multi", Multi.layout multi)]

    local
       fun make f (T r) = f r
       fun make' f = (make f, ! o (make f))
     in
       val multi = make #multi
     end
   
     fun new (): t = T {multi = Multi.new ()}
		      
     val forceMulti = Multi.forceMulti o multi
     val isMulti = Multi.isMulti o multi
     fun whenMulti (vi, th) = Multi.whenMulti (multi vi, th)
   end

structure VarInfo =
  struct
    datatype t = T of {multi: Multi.t}

    fun layout (T {multi, ...}) 
      = Layout.record [("multi", Multi.layout multi)]

    local
       fun make f (T r) = f r
       fun make' f = (make f, ! o (make f))
     in
       val multi = make #multi
     end
   
     fun new (): t = T {multi = Multi.new ()}
		      
     val forceMulti = Multi.forceMulti o multi
     val isMulti = Multi.isMulti o multi
     fun whenMulti (vi, th) = Multi.whenMulti (multi vi, th)
   end


fun once (p as Program.T {globals, functions, main, ...})
  = let
      (* funcInfo *)
      val {get = funcInfo: Func.t -> FuncInfo.t,
	   set = setFuncInfo, ...}
	= Property.getSetOnce
	  (Func.plist, Property.initRaise ("Once.funcInfo", Func.layout))

      (* nodeInfo *)
      val {get = nodeInfo: Node.t -> Function.t,
	   set = setNodeInfo, ...}
	= Property.getSetOnce 
	  (Node.plist, Property.initRaise ("Once.nodeInfo", Node.layout))

      (* labelInfo *)
      val {get = labelInfo: Label.t -> LabelInfo.t, ...}
	= Property.get
	  (Label.plist, Property.initFun (fn _ => LabelInfo.new ()))

      (* varInfo *)
      val {get = varInfo: Var.t -> VarInfo.t, ...}
	= Property.get
	  (Var.plist, Property.initFun (fn _ => VarInfo.new ()))

      (* update call counts 
       * construct call graph 
       * compute multiThreaded
       *)
      val G = Graph.new ()
      fun newNode () = Graph.newNode G
      fun addEdge edge = (Graph.addEdge (G, edge); ())
      fun addEdge' edge = if Node.hasEdge edge then () else addEdge edge
      val _ = List.foreach
	      (functions, fn f =>
	       let
		 val n = newNode ()
	       in 
		 setFuncInfo (Function.name f, FuncInfo.new n) ;
		 setNodeInfo (n, f)
	       end)
      val _ = List.foreach
	      (functions, fn f =>
	       let
		 val {name = f, blocks, ...} = Function.dest f
		 val fi = funcInfo f
	       in
		 Vector.foreach
		 (blocks, fn Block.T {transfer, ...} =>
		  case transfer
		    of Call {func = g, ...}
		     => let
			  val gi = funcInfo g
			in 
			  FuncInfo.whenMultiThreaded
			  (gi, fn () => FuncInfo.forceMultiThreaded fi) ;
			  Int.inc (FuncInfo.calls gi) ;
			  addEdge {from = FuncInfo.node fi,
				   to = FuncInfo.node gi}
			end
		     | Runtime {prim, ...}
		     => if Prim.name prim = Prim.Name.Thread_copyCurrent
			  then FuncInfo.forceMultiThreaded fi
			  else ()
		     | _ => ())
	       end)

      fun visitFunc f
	= let
	    val {name, args, blocks, ...} = Function.dest f
	    val fi = funcInfo name
	    val _ = if FuncInfo.calls' fi > 1
		      then FuncInfo.forceMulti fi
		      else ()

	    fun forceMultiBlock (Block.T {label, args, statements, transfer})
	      = let
		  val li = labelInfo label
		in
		  if LabelInfo.isMulti li
		    then ()
		    else (LabelInfo.forceMulti li ;
			  Vector.foreach
			  (args, VarInfo.forceMulti o varInfo o #1) ;
			  Vector.foreach
			  (statements, fn Statement.T {var, ...} =>
			   Option.app (var, VarInfo.forceMulti o varInfo)) ;
			  Transfer.foreachFunc
			  (transfer, FuncInfo.forceMulti o funcInfo))
		end

	    fun forceMultiFunc ()
	      = (Vector.foreach (args, VarInfo.forceMulti o varInfo o #1) ;
		 Vector.foreach (blocks, forceMultiBlock))
	  in
	    if FuncInfo.isMulti fi
	      then forceMultiFunc ()
	      else let
		     val _ = FuncInfo.whenMulti (fi, forceMultiFunc)

		     val {graph, labelNode, nodeBlock}
		       = Function.controlFlow f

		     fun forceMultiBlocksReachable nodes
		       = Graph.dfsNodes
		         (graph, nodes,
			  Graph.DfsParam.finishNode
			  (forceMultiBlock o nodeBlock))

		     fun visitBlock (Block.T {transfer, ...})
		       = case transfer
			   of Call {func, return, ...}
			    => if FuncInfo.isMultiThreaded (funcInfo func)
				 then forceMultiBlocksReachable
				      (let val nodes = ref []
				       in Return.foreachLabel
					  (return, fn l =>
					   List.push (nodes, labelNode l)) ;
					  !nodes
				       end)
				 else ()
			    | Runtime {prim, return, ...}
			    => if Prim.name prim = Prim.Name.Thread_copyCurrent
				 then forceMultiBlocksReachable [labelNode return]
				 else ()
			    | _ => ()
		     val forceMultiBlock
		       = fn block => (forceMultiBlock block; visitBlock block)
		   in
		     List.foreach
		     (Graph.stronglyConnectedComponents graph,
		      fn [] => ()
		       | [n] => if Node.hasEdge {from = n, to = n}
				  then forceMultiBlock (nodeBlock n)
				  else visitBlock (nodeBlock n)
		       | ns => List.foreach (ns, forceMultiBlock o nodeBlock))
		   end
	  end
      fun forceMultiFunc f
	= (FuncInfo.forceMulti (funcInfo (Function.name f)) ; visitFunc f)

      val _ = List.foreach
	      (Graph.stronglyConnectedComponents G,
	       fn [] => ()
	        | [n] => if Node.hasEdge {from = n, to = n}
			   then forceMultiFunc (nodeInfo n)
			   else visitFunc (nodeInfo n)
		| ns => List.foreach (ns, forceMultiFunc o nodeInfo))

      val _ = Control.diagnostics
	      (fn display =>
	       let open Layout
	       in 
		 List.foreach
		 (functions, fn f =>
		  let 
		    val {name = f, blocks, ...} = Function.dest f
		  in 
		    display (seq [Func.layout f,
				  str " ",
				  FuncInfo.layout (funcInfo f)]) ;
		    Vector.foreach
		    (blocks, fn Block.T {label, ...} =>
		     display (seq [Label.layout label,
				   str " ",
				   LabelInfo.layout (labelInfo label)]))
		  end)
	       end)
    in
      {funcIsSingleThreaded = not o FuncInfo.isMultiThreaded o funcInfo,
       funcIsUsedOnce = not o FuncInfo.isMulti o funcInfo,
       labelIsUsedOnce = not o LabelInfo.isMulti o labelInfo,
       varIsBoundOnce = not o VarInfo.isMulti o varInfo}
    end
end

functor Once (S: ONCE_STRUCTS): ONCE = 
struct

open S
open Exp Transfer

structure Graph = DirectedGraph
local open Graph
in
   structure Edge = Edge
   structure Node = Node
end

fun usesConts p = 
   Program.hasPrim (p, fn p => Prim.name p = Prim.Name.Thread_switchTo)
       
(*
 * Build control flow graph for main function.
 * Compute strongly-connected components and set the vars for any basic
 * block that is a trivial component and does not jump to itself to be once.
 *)
   
fun once (program as Program.T {globals, functions, main, ...}) =
   if usesConts program
      then fn _ => false
   else
   let
      val {get = once, set = setOnce, ...} =
	 Property.getSet (Var.plist, Property.initConst false)

      val main = 
	 case List.peek 
	      (functions, fn f => 
	       Func.equals (Function.name f, main)) of
	    NONE => Error.bug "no main"
	  | SOME f => f

      val {graph, labelNode, nodeBlock} =
	 Function.controlFlow main

      val _ =
	 List.foreach
	 (Graph.stronglyConnectedComponents graph,
	  fn [] => ()
	   | [n] => if not (Node.hasEdge {from = n, to = n})
	               then let val Block.T {statements, ...} = nodeBlock n
			    in Vector.foreach
			       (statements, fn Statement.T {var, exp, ...} =>
				case exp of
				   PrimApp {prim, ...} =>
				      if let datatype z = datatype Prim.Name.t
					 in case Prim.name prim of
					       Array_array => true
					     | Array_array0 => true
					     | Ref_ref => true
					     | _ => false
					 end
					 then setOnce (valOf var, true)
				      else ()
				 | _ => ())
			    end
		    else ()
	   | _ => ())
      val _ = 
	 Vector.foreach 
	 (globals, fn Statement.T {var, ...} => setOnce (valOf var, true))

      val _ =
	 Control.diagnostics
	 (fn display =>
	  (display (Layout.str "\n\nOnce:")
	   ; Vector.foreach
	     (Function.blocks main, fn Block.T {statements, ...} =>
	      Vector.foreach
	      (statements, fn Statement.T {var, ...} =>
	       case var of
		  SOME x => if once x 
			      then display (Var.layout x)
			      else ()
		| _ => ()))))
   in
      once
   end
   
end
