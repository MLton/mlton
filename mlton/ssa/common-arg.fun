(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor CommonArg (S: COMMON_ARG_STRUCTS): COMMON_ARG = 
struct

open S
open Exp Transfer
  
structure Graph = DirectedGraph
structure Node = Graph.Node

structure VarInfo =
  struct
    datatype t = T of {node: unit DirectedGraph.Node.t}

    fun layout lNode (T {node, ...}) =
      let open Layout
      in record [("node", lNode node)]
      end

    local
      fun make f (T r) = f r
    in
      val node = make #node
    end

    fun new node = T {node = node}
  end

structure NodeInfo =
  struct
    datatype t = T of {var: Var.t}

    local
      fun make f (T r) = f r
    in
      val var = make #var
    end

    fun new var = T {var = var}
  end

fun eliminate (Program.T {datatypes, globals, functions, main}) =
  let
    val shrink = shrinkFunction globals

    val {get = nodeInfo: unit Node.t -> NodeInfo.t, 
	 set = setNodeInfo, ...} =
      Property.getSetOnce
      (Node.plist,
       Property.initRaise ("CommonArg.nodeInfo", Node.layout))
      
    val {get = labelArgs: Label.t -> (Var.t * Type.t) vector, 
	 set = setLabelArgs, ...} =
      Property.getSetOnce
      (Label.plist, 
       Property.initRaise ("CommonArg.labelArgs", Label.layout))

    fun eliminateFunction (f: Function.t) =
      let
	val {name, args, start, blocks, returns, raises} = Function.dest f

	(* Argument flow graph. *)
	val G = Graph.new ()
	val root = Graph.newNode G
	fun newNode v = let
			  val node = Graph.newNode G
			  val _ = setNodeInfo (node, NodeInfo.new v)
			in
			  node
			end
	fun newRootedNode v =
	   let
	      val node = newNode v
	      val _ = Graph.addEdge (G, {from = root, to = node})
	   in
	      node
	   end
	val {get = varInfo: Var.t -> VarInfo.t, 
	     set = setVarInfo, ...} =
	  Property.getSetOnce
	  (Var.plist,
	   Property.initFun (VarInfo.new o newRootedNode))
	(* Initialize *)
	val _ = 
	  Vector.foreach
	  (blocks, fn Block.T {label, args, ...} =>
	   (setLabelArgs (label, args);
	    Vector.foreach (args, fn (v, _) =>
			    setVarInfo (v, VarInfo.new (newNode v)))))
	(* Flow Transfer.Goto arguments. *)
	fun flowVarVar (v, v') = 
	   let
	      val vi = varInfo v
	      val node = VarInfo.node vi
	      val vi' = varInfo v'
	      val node' = VarInfo.node vi'
	      val _ = Graph.addEdge (G, {from = node, to = node'})
	   in
	      ()
	   end
	fun flowVarVarTy (v, (v', _)) = flowVarVar (v, v')
	fun flowVarsVarTys (vs, vts') = Vector.foreach2 (vs, vts', flowVarVarTy)
	fun flowVarsLabelArgs (vs, l) = flowVarsVarTys (vs, labelArgs l)
	(* Visit in unknown contexts. *)
	fun visitVar v = 
	   let
	      val vi = varInfo v
	      val node = VarInfo.node vi
	      val _ = Graph.addEdge (G, {from = root, to = node})
	   in
	      ()
	   end
	fun visitVarTy (v, _) = visitVar v
	fun visitArgs args = Vector.foreach (args, visitVarTy)
	fun visitLabelArgs l = visitArgs (labelArgs l)
	(* Analyze *)
	val _ = 
	  Vector.foreach
	  (blocks, fn Block.T {transfer, ...} =>
	   (case transfer of
	      Arith {overflow, success, ...} =>
		(visitLabelArgs overflow;
		 visitLabelArgs success)
	    | Bug => ()
	    | Call {return, ...} =>
		(case return of
		   Return.NonTail {cont, handler} =>
		     (visitLabelArgs cont;
		      case handler of
			Handler.Handle hand =>
			  (visitLabelArgs hand)
		      | _ => ())
		 | _ => ())
	    | Case {cases, default, ...} =>
		   (Cases.foreach
		    (cases, visitLabelArgs);
		    Option.app (default, visitLabelArgs))
	    | Goto {dst, args} => 
		   (flowVarsLabelArgs (args, dst))
	    | Raise _ => ()
	    | Return _ => ()
	    | Runtime {return, ...} => 
		   (visitLabelArgs return)))
	val () = Graph.removeDuplicateEdges G
	(* Compute dominators. *)
	fun computeDominators () = 
	  let val {idom} = Graph.dominators (G, {root = root})
	  in idom 
	  end
	val idom = computeDominators ()
	fun getVar v =
	  let
	    val vi = varInfo v
	    val node = VarInfo.node vi
	  in
	    case idom node of
	      Graph.Idom parent => if Node.equals (parent, root)
				     then v
				     else NodeInfo.var (nodeInfo parent)
	    | Graph.Unreachable => v
	    | Graph.Root => Error.bug "CommonArg.getVar"
	  end
	fun keepVar v = Var.equals (v, getVar v)

	(* Diagnostics *)
	val _ = 
	  Control.diagnostics
	  (fn display =>
	   let
	     open Layout
	     fun lNode n = 
	       record [("idom", case idom n of
			Graph.Idom parent => 
			  if Node.equals (parent, root)
			    then str "root"
			    else Var.layout (NodeInfo.var (nodeInfo parent))
		      | _ => str "???")]
	     val _ = 
	       File.withOut
	       (concat [Func.toString name, ".commonArg.dot"],
		fn out =>
		Layout.outputl
		(Graph.layoutDot 
		 (G, fn _ =>
		  {title = concat [Func.toString name, " argument-flow graph"],
		   options = [],
		   edgeOptions = fn _ => [],
		   nodeOptions = fn n => 
		   [Dot.NodeOption.label
		    (if Node.equals (n, root)
		       then "root"
		       else Var.toString (NodeInfo.var (nodeInfo n)))]}), out))
	   in
	     display (seq [str "\n",
			   Func.layout name]);
	     Vector.foreach
	     (blocks, fn Block.T {args, label, ...} =>
	      if Vector.exists(args, not o keepVar o #1)
		then display (seq [Label.layout label,
				   str " ",
				   Vector.layout
				   (fn (v, _) =>
				    seq [Var.layout v,
					 str ": ",
					 VarInfo.layout lNode (varInfo v)])
				   args])
		else ())
	   end)
	  
	(* Transform *)
	val blocks = 
	  Vector.map
	  (blocks, fn Block.T {args, label, statements, transfer} =>
	   let
	     val {yes = args, no = rems} = 
	       Vector.partition (args, keepVar o #1)
	     val statements =
	       if Vector.isEmpty rems
		 then statements
		 else Vector.concat
		      [Vector.map
		       (rems, fn (v, ty) =>
			Statement.T 
			{var = SOME v,
			 ty = ty,
			 exp = Var (getVar v)}),
		       statements]
	     val transfer =
	       case transfer of
		 Goto {args, dst} => 
		   let
		     val args =
		       Vector.keepAllMap2
		       (args, labelArgs dst, fn (arg, (v, _)) =>
			if keepVar v
			  then SOME arg
			  else NONE)
		   in
		     Goto {args = args, dst = dst}
		   end
	       | _ => transfer 
	   in
	     Block.T {args = args,
		      label = label, 
		      statements = statements,
		      transfer = transfer}
	   end)
      in
	shrink (Function.new {name = name,
			      args = args,
			      start = start,
			      blocks = blocks,
			      raises = raises,
			      returns = returns})
      end

    val program =
      Program.T {datatypes = datatypes,
		 globals = globals,
		 functions = List.revMap (functions, eliminateFunction),
		 main = main}
    val _ = Program.clearTop program
  in
    program
  end

end
