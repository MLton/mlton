(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Zone (S: ZONE_STRUCTS): ZONE = 
struct

open S

type int = Int.t

structure Set = DisjointSet
structure Graph = DirectedGraph
local
   open Graph
in
   structure LoopForest = LoopForest
   structure Node = Node
end
   
fun zoneFunction (f, ac) =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
	 Function.dest f
   in
      if Vector.length blocks <= !Control.maxFunctionSize
	 then f :: ac
      else
	 let
	    val G = Graph.new ()
	    type node = unit Node.t
	    val {get = labelInfo: Label.t -> {node: node}, ...} =
	       Property.get (Label.plist,
			     Property.initFun (fn _ => {node = Graph.newNode G}))
	    val labelNode = #node o labelInfo
	    (* Build control-flow graph. *)
	    val () =
	       Vector.foreach
	       (blocks, fn Block.T {label, transfer, ...} =>
		let
		   val {node = from, ...} = labelInfo label
		in
		   Transfer.foreachLabel
		   (transfer, fn l =>
		    ignore (Graph.addEdge (G, {from = from, to = labelNode l})))
		end)
	    fun layout f =
	       let
		  val {loops, notInLoop} = LoopForest.dest f
	       in
		  Layout.record
		  [("notInLoop", Int.layout (Vector.length notInLoop)),
		   ("loops", Vector.layout (layout o #child) loops)]
	       end
	    datatype count = Many | None | One of Label.t
	    (* Display classes. *)
	    val () =
	       Control.diagnostics
	       (fn display =>
		(display (Func.layout name)
		 ; display (layout
			    (Graph.loopForestSteensgaard
			     (G, {root = labelNode start})))))
	 in
	    f :: ac
	 end
   end
   
fun zone (Program.T {datatypes, globals, functions, main}) =
   Program.T {datatypes = datatypes,
	      globals = globals,
	      functions = List.fold (functions, [], zoneFunction),
	      main = main}

end

