(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor SimplifyTypes (S: SIMPLIFY_TYPES_STRUCTS): SIMPLIFY_TYPES = 
struct

open S

structure Graph = DirectedGraph
structure Node = Graph.Node

fun simplifyTypes (p as Program.T {datatypes, body, ...}) =
   let
      val g = Graph.new ()
      val {get = tyconInfo: Tycon.t -> {node: Node.t,
					isOneVariantArrow: bool ref,
					cons: {con: Con.t,
					       arg: Type.t option
					       } vector
					} option,
	   set = setTyconInfo, destroy = destroyTycon} =
	 Property.destGetSetOnce (Tycon.plist, Property.initConst NONE)
      val {get = nodeTycon, set = setNodeTycon} =
	 Property.getSetOnce (Node.plist,
			      Property.initRaise ("tycon", Node.layout))
      val _ =
	 Vector.foreach
	 (datatypes, fn {tycon, cons, ...} =>
	  let val node = Graph.newNode g
	  in setTyconInfo (tycon, SOME {node = node,
					isOneVariantArrow = ref false,
					cons = cons})
	     ; setNodeTycon (node, tycon)
	  end)
      val _ = 
	 Vector.foreach
	 (datatypes, fn {tycon, cons, ...} =>
	  let
	     val {node = from, ...} = valOf (tyconInfo tycon)
	     fun loop (t: Type.t): unit =
		case Type.dest t of
		   Type.Var _ => ()
		 | Type.Con (tycon', ts) =>
		      (if Tycon.equals (tycon, tycon')
			  then (case tyconInfo tycon' of
				   NONE => ()
				 | SOME {node = to, ...} =>
				      (Graph.addEdge (g, {from = from,
							  to = to})
				       ; ()))
		       else ()
			  ; Vector.foreach (ts, loop))
	  in Vector.foreach (cons, fn {arg, ...} =>
			     case arg of
				NONE => ()
			      | SOME t => loop t)
	  end)
      fun num (datatypes, p) =
	 List.fold (datatypes, 0, fn (d, n) => if p d then n + 1 else n)
      val numDatatypes = Vector.length datatypes
      val arrowDatatypes =
	 Vector.keepAll
	 (datatypes, fn {cons, ...} =>
	  Vector.exists (cons, fn {arg, ...} =>
			 case arg of
			    NONE => false
			  | SOME t =>
			       Type.containsTycon (t, Atoms.Tycon.arrow)))
      val numArrowDatatypes = Vector.length arrowDatatypes
      val oneVariantArrows = Vector.keepAll (arrowDatatypes, fn {cons, ...} =>
					     1 = Vector.length cons)
      val numOneVariantArrows = Vector.length oneVariantArrows
      val _ =
	 Vector.foreach
	 (oneVariantArrows, fn {tycon, ...} =>
	  let val {isOneVariantArrow, ...} = valOf (tyconInfo tycon)
	  in isOneVariantArrow := true
	  end)
      val components = Graph.stronglyConnectedComponents g
      val numEliminable =
	 List.fold
	 (components, 0, fn (nodes, n) =>
	  case nodes of
	     [node] =>
		if Node.hasEdge {from = node, to = node}
		   then n
		else
		   let
		      val {isOneVariantArrow, ...} =
			 valOf (tyconInfo (nodeTycon node))
		   in if !isOneVariantArrow
			 then n + 1
		      else n
		   end
	   | _ => n)
      val _ =
	 Control.message
	 (Control.Detail, fn () =>
	  let open Layout
	  in align [seq [str "datatypes: ", Int.layout numDatatypes],
		    seq [str "-> datatypes: ", Int.layout numArrowDatatypes],
		    seq [str "one variants: ", Int.layout numOneVariantArrows],
		    seq [str "eliminable: ", Int.layout numEliminable]]
	  end)
      val _ = destroyTycon ()
   in
      p
   end

end
