(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
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
