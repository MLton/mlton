(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor Once (S: ONCE_STRUCTS): ONCE = 
struct

open S
open Dec PrimExp Transfer

structure Graph = DirectedGraph
local open Graph
in
   structure Edge = Edge
   structure Node = Node
end

fun usesConts p = Program.hasPrim (p, fn p =>
				  case Prim.name p of
				     Prim.Name.Thread_switchToCont => true
				   | _ => false)
       
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
      val {get = once, set = setOnce} =
	 Property.getSet (Var.plist, Property.initConst false)
      val {set = setNode, get = jumpNode} =
	 Property.getSetOnce (Jump.plist,
			      Property.initRaise ("Once.info", Jump.layout))
      val {set = setNodeInfo, get = nodeInfo} =
	 Property.getSetOnce (Node.plist,
			      Property.initRaise ("jump", Node.layout))
      val nodeVars = #vars o nodeInfo
      val nodeName = #name o nodeInfo
      val jumpHandlers = inferHandlers program
      val graph = Graph.new ()
      fun loop (e: Exp.t, vars: Var.t list ref, node: Node.t,
	       handlers: Jump.t list) =
	 let
	    val {decs, transfer} = Exp.dest e
	    fun jump j =
	       (Graph.addEdge (graph, {from = node, to = jumpNode j}); ())
	    fun handler () =
	       case List.fold (decs, handlers, deltaHandlers) of
		  h :: _ => jump h
		| _ => ()
	    val _ =
	       List.foreach
	       (decs, 
		fn Bind {var, exp = PrimApp {prim, info, ...}, ...} =>
		(PrimInfo.foreachJump (info, jump)
		 ; if let
			 datatype z = datatype Prim.Name.t
		      in
			 case Prim.name prim of
			    Array_array => true
			  | Array_array0 => true
			  | Ref_ref => true
			  | _ => false
		      end
		      then List.push (vars, var)
		   else ())
		 | Fun {name, body, ...} =>
		      let
			 val n = Graph.newNode graph
			 val vars = ref []
			 val _ = setNodeInfo (n, {name = Jump.layout name,
						  vars = vars})
			 val _ = setNode (name, n)
		      in
			 loop (body, vars, n, jumpHandlers name)
		      end
		 | _ => ())
	 in
	    case transfer of
	       Call {cont, ...} => (case cont of
				      NONE => ()
				    | SOME c => (jump c; handler ()))
	     | Jump {dst, ...} => jump dst
	     | Case {cases, default, ...} =>
		  (Cases.foreach (cases, jump)
		   ; (case default of
			 NONE => ()
		       | SOME j => jump j))
	     | Raise _ => handler ()
	     | _ => ()
	 end
      val Function.T {body, ...} =
	 case Vector.peek (functions, fn Function.T {name, ...} =>
			   Func.equals (name, main)) of
	    NONE => Error.bug "no main"
	  | SOME r => r
      val _ = 
	 let
	    val n = Graph.newNode graph
	    val vars = ref []
	 in setNodeInfo (n, {name = Layout.str "main",
			    vars = vars});
	    loop (body, vars, n, [])
	 end
      val _ =
	 Control.diagnostic
	 (fn display =>
	  (display (Exp.layout body)
	   ; display (Layout.str "\n\n")
	   ; let open Graph.LayoutDot
	     in display (layout {graph = graph,
				 title = "once",
				 options = [],
				 edgeOptions = fn _ => [],
				 nodeOptions = fn n => [NodeOption.Label
							(Layout.toString
							 (nodeName n))]})
	     end))
      val _ =
	 List.foreach
	 (Graph.stronglyConnectedComponents graph,
	  fn [] => ()
	   | [n] =>
		if Node.hasEdge {from = n, to = n}
		   then ()
		else List.foreach (! (nodeVars n), fn x => setOnce (x, true))
	   | _ => ())
      val _ = Vector.foreach (globals, fn {var, ...} => setOnce (var, true))
      val _ =
	 Control.diagnostic
	 (fn display =>
	  Exp.foreachVar (body, fn (x, _) =>
			  if once x then display (Var.layout x)
			  else ()))
   in
      once
   end
   
end
