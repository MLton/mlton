(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(* Insert limit checks at
 * 1. Loop headers
 * 2. Continuations
 * 3. Handlers
 *)
functor LimitCheck (S: LIMIT_CHECK_STRUCTS): LIMIT_CHECK = 
struct

open S
datatype z = datatype Exp.t
datatype z = datatype Transfer.t

datatype t = No | Maybe | Yes

val toString =
   fn Maybe => "Maybe"
    | No => "No"
    | Yes => "Yes"

val layout = Layout.str o toString

val op < =
   fn (Yes, _) => false
    | (Maybe, Yes) => true
    | (No, No) => false
    | (No, _) => true
    | _ => false

structure Edge = DirectedGraph.Edge
structure Node = DirectedGraph.Node

fun limitCheck (program as Program.T {functions, ...})=
   let
      val usesSignals =
	 Program.hasPrim (program, fn p =>
			  Prim.name p = Prim.Name.Thread_finishHandler)
   in
      fn f =>
      let
	 val {blocks, name, ...} = Function.dest f
	 val {get = labelInfo: Label.t -> {inBody: bool ref,
					   limitCheck: t ref}} =
	    Property.get (Label.plist,
			  Property.initFun (fn _ => {inBody = ref false,
						     limitCheck = ref No}))
	 fun up (r: t ref, v: t) =
	    if !r < v
	       then r := v
	    else ()
	 val _ = 
	    Vector.foreach
	    (blocks, fn Block.T {statements, ...} =>
	     Vector.foreach
	     (statements, fn Statement.T {exp, ...} =>
	      case exp of
		 SetHandler h => up (#limitCheck (labelInfo h), Maybe)
	       | _ => ()))
	 val {labelNode, nodeBlock, ...} = Function.controlFlow f
	 val _ =
	    Tree.traverse
	    (Function.dominatorTree f, fn Block.T {label, ...} =>
	     let
		val {inBody, ...} = labelInfo label
		val _ = inBody := true
	     in
		fn () =>
		let
		   val _ =
		      List.foreach
		      (Node.successors (labelNode label), fn e =>
		       let
			  val n = Edge.to e
			  val {inBody, limitCheck, ...} =
			     labelInfo (Block.label (nodeBlock n))
		       in if !inBody
			     then up (limitCheck,
				      if usesSignals then Yes else Maybe)
			  else ()
		       end)
		   val _ = inBody := false
		in
		   ()
		end
	     end)
	 val _ =
	    Control.diagnostics
	    (fn display =>
	     let
		open Layout
		val _ =
		   display (seq [str "limit checks for ",
				 Func.layout name])
		val _ =
		   Vector.foreach
		   (blocks, fn Block.T {label, ...} =>
		    let
		       val {limitCheck, ...} = labelInfo label
		    in
		       display (seq [Label.layout label,
				     str " ",
				     layout (!limitCheck)])
		    end)
	     in
		()
	     end)
      in
	 ! o #limitCheck o labelInfo
      end
   end
   
end


